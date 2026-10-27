select expires_at, revoked_at, used_at
from nix.enrollment_token
where token_hash = ?;
