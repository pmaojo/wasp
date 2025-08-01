CREATE TABLE cli_runs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    command TEXT NOT NULL,
    args TEXT NOT NULL,
    status TEXT NOT NULL,
    output TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
