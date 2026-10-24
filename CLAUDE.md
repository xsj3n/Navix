# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Navix is a Haskell backend that acts as a self-hosted fleet-management control plane for NixOS
machines. On startup it bootstraps its own private PKI (via `openssl` subprocesses), serves HTTPS
with Warp/WarpTLS, and talks to Postgres to track enrolled machines and the actions queued for
them (wipe, rebuild, reboot, rotate credentials, malware scan, etc.).

## Development environment

This project is developed inside a Nix flake dev shell — enter it with `nix develop` before running
any `cabal` commands. The shell's `shellHook` (in `flake.nix`) automatically initializes a local
Postgres cluster under `./database` (via `initdb`) and starts it with `pg_ctl` using `PGDATA=./database`,
`PGHOST=/tmp` (so the Postgres unix socket lives in `/tmp`, not `./database`). This means Postgres is
already running by the time you have a shell prompt — no separate `pg_ctl start` step.

`cabal.project.local` sets `tests: True`.

## Common commands

- Build: `cabal build`
- Build everything including tests: `cabal build all`
- Run the server: `cabal run Navix` (listens on `https://localhost:3000`)
- Run all tests: `cabal test`
- Run a single test suite: `cabal test pki-test` or `cabal test query-test`

There are two test suites, each a plain `exitcode-stdio-1.0` executable rather than a unit-test
framework — they call into library code and exit non-zero on failure:
- `tests/pki/Test.hs` — runs `initPKI` then `verifyPKI`, checking cert chain validation end-to-end.
- `tests/query/Test.hs` — exercises `readQueryFile`/`readQueryFiles` against the files in `queries/`.

`hie.yaml` maps HLS cradles to four components: `lib:Navix`, `exe:Navix`, `test:pki-test`,
`test:query-test` — matching the four build targets in `Navix.cabal`.

## Architecture

The library (`src/Navix/`) is consumed by both the `Navix` executable (`app/Main.hs`) and the two
test suites, so library code should stay side-effect-isolated from the executable's wiring where
possible.

- **`Navix.PKI`** — owns the entire private CA. `initPKI` shells out to `openssl` to generate an EC
  root key/self-signed root cert, then an EC TLS key/CSR signed by that root, all under
  `./cert-store/` (`cert-store/root-*.pem`, `cert-store/transport/tls-*.pem`). `verifyPKI` validates
  the TLS cert chain against the store using `crypton-x509-validation`. `Main.hs` calls `initPKI`
  only if `cert-store/` is empty, then always calls `verifyPKI` and refuses to boot if validation
  fails.
- **`Navix.TLS`** — Warp TLS settings/mutual-TLS scaffolding, separate from the PKI *generation*
  logic in `Navix.PKI`. `mutualTLSSettings` and `onClientCertificateMutalTLS` exist but are not yet
  wired into `Main.hs` (which currently uses one-way `tlsSettings`, not mTLS).
  `onClientCertificateMutalTLS` currently accepts every client cert unconditionally — this is a
  stated placeholder, not a finished check.
- **`Navix.Net`** — intended home for poll-related networking logic (`pollResponse`); currently a
  stub returning `undefined`.
- **`Navix.DatabaseInterface`** — all Postgres access. `getDatabaseConnection` connects as superuser
  to create the `managed` database if absent, then reconnects as the app user and runs
  `queries/init/table-creation.sql` to ensure schema exists. Connection parameters
  (`connectHost`/`connectUser`/`connectPort`) are hardcoded in `getConnectionInfo` /
  `getSuperUserConnectionInfo`, not read from config. SQL lives in `.sql` files under `queries/`
  rather than inline Haskell strings; `readQueryFile`/`readQueryFiles` load them at runtime.
- **`Navix.DataSchema`** — Aeson `FromJSON`/`ToJSON` and postgresql-simple `FromRow`/`ToRow`
  instances for the wire/DB types: `NixMachine`, `NixMachineEnrollment` (enrollment token + machine
  details), `PollResponse` (the set of actions a machine can be told to perform).
- **`Navix.Registration`** — handles `POST /register`. Reads the request body incrementally via
  `getRequestBodyChunk` up to a 1024-byte cap, decodes a `NixMachineEnrollment`, checks the
  enrollment token against a flat file (`./enrollment_tokens`, one token per line — a stated
  stand-in for eventually storing tokens in Postgres), and on success inserts the machine using the
  query from `queries/register.sql`.
- **`app/Main.hs`** — process entrypoint and routing. Dispatches on HTTP method + last path segment
  (`register`, `deregister`, `poll`) in `app`; `deregister` and `poll` are currently `undefined`.

## Database schema

`queries/init/table-creation.sql` defines the `nix` schema: `nix.machines` (keyed by
`assignedCert` + `hardwareFingerprint`), `nix.machineActions` (FK to `machines`, one row per queued
action), `nix.machineInfo` (FK to `machines`, assigned user). Note: as currently written this file
has trailing commas before closing parens in two `CREATE TABLE` statements and an index referencing
`nix.machineStatus`, a table that doesn't exist (the actual table is `nix.machines`) — treat this
file as not-yet-runnable SQL rather than verified schema.
