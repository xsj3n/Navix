update nix.enrollment_tokens set used_at = now() where token_hash = ?;
