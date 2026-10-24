create schema if not exists nix;
--
create table if not exists nix.machines(
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    deviceName text not null,
    assignedCert text not null,
    hardwareFingerprint text not null,
    status boolean not null default true,
    createdAt timestamptz not null default now(),
    unique (assignedCert, hardwareFingerprint)
);
--
create table if not exists nix.machine_actions(
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    machineId bigint not null references nix.machines(id) on delete cascade,
    actionType text not null,
    payload text
);
--
create table if not exists nix.machine_info(
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    machineId bigint not null references nix.machines(id) on delete cascade,
    assignedUser text
 );
--
create table if not exists nix.enrollment_tokens(
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    token_hash bytea not null unique,
    created_at timestamptz not null default now(),
    expires_at timestamptz not null,
    used_at timestamptz,
    revoked_at timestamptz
);
--
-- this will only be ran on startup by the server so no need to wait for lock 
--
create index idxMachineActionsRefId on nix.machine_actions(machineId);
