-- A pull uses the same log as a push. Existing rows are pushes.

ALTER TABLE git_push
  ADD COLUMN direction text NOT NULL DEFAULT 'push';

ALTER TABLE git_push
  ADD CONSTRAINT git_push_direction_check
  CHECK (direction IN ('push', 'pull'));
