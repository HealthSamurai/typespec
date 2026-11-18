CREATE TABLE IF NOT EXISTS context_example_todos (
  id bigserial PRIMARY KEY,
  title text NOT NULL CHECK (length(trim(title)) BETWEEN 1 AND 200),
  done boolean NOT NULL DEFAULT false
)
