select expires_at, revoked_at, used_at
from nix.enrollment_tokens
where token_hash = ?;
