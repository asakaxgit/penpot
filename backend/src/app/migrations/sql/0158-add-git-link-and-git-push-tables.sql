-- One Git repository link per file, and the pushes sent for it.
-- The token column holds a JWE ciphertext, not the provider token.
-- The encryption key is derived from PENPOT_SECRET_KEY. A secret that
-- changes on restart (the one generated when that variable is unset)
-- cannot decrypt tokens stored before the restart. Set a fixed
-- PENPOT_SECRET_KEY before linking a repository.
--
-- git_push records push attempts only. It never stores the token or
-- the Authorization header.

CREATE TABLE git_link (
  id uuid PRIMARY KEY,
  -- DEFERRABLE so the test-suite reset (SET CONSTRAINTS ALL DEFERRED)
  -- can delete parent rows first.
  file_id uuid NOT NULL UNIQUE REFERENCES file(id) ON DELETE CASCADE DEFERRABLE,
  profile_id uuid NULL REFERENCES profile(id) ON DELETE SET NULL DEFERRABLE,

  provider text NOT NULL,
  repo_url text NOT NULL,
  branch text NOT NULL,
  folder text NOT NULL,
  token text NOT NULL,

  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),

  CONSTRAINT git_link_provider_check CHECK (provider IN ('github', 'gitlab'))
);

CREATE TABLE git_push (
  id uuid PRIMARY KEY,
  git_link_id uuid NOT NULL REFERENCES git_link(id) ON DELETE CASCADE DEFERRABLE,
  -- Unique so a retried job sees the row and does not commit twice.
  -- A new "Push now" uses a new job id and can try again.
  job_id uuid NULL UNIQUE,

  created_at timestamptz NOT NULL DEFAULT now(),
  status text NOT NULL,
  commit_message text NOT NULL,
  error text NULL,
  req_data jsonb NULL,
  rsp_data jsonb NULL,

  CONSTRAINT git_push_status_check CHECK (status IN ('running', 'success', 'error'))
);

CREATE INDEX git_push__git_link_id__created_at__idx
    ON git_push (git_link_id, created_at DESC);
