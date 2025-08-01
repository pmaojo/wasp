CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    email TEXT NOT NULL UNIQUE,
    name TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    user_id UUID NOT NULL REFERENCES users(id),
    title TEXT NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE cli_runs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    command TEXT NOT NULL,
    args TEXT NOT NULL,
    status TEXT NOT NULL,
    output TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
