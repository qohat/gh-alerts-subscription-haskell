CREATE TABLE IF NOT EXISTS users (user_id UUID, slack_user_id varchar, slack_channel_id varchar);
CREATE TABLE IF NOT EXISTS repositories (repository_id UUID, owner varchar, repository varchar);
CREATE TABLE IF NOT EXISTS subscriptions (user_id UUID, repository_id UUID, TBD varchar);