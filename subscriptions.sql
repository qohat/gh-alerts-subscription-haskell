CREATE TABLE IF NOT EXISTS users (
   user_id VARCHAR UNIQUE NOT NULL,
   slack_user_id VARCHAR NOT NULL, 
   slack_channel_id VARCHAR NOT NULL,
   PRIMARY KEY (user_id)
);

CREATE TABLE IF NOT EXISTS repositories (
   repository_id VARCHAR UNIQUE NOT NULL,
   owner VARCHAR NOT NULL, 
   repository VARCHAR NOT NULL,
   PRIMARY KEY (repository_id)
);

CREATE TABLE IF NOT EXISTS subscriptions (
   user_id VARCHAR NOT NULL,
   repository_id VARCHAR NOT NULL, 
   TBD VARCHAR NOT NULL
);