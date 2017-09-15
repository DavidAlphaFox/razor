CREATE TABLE razor_user (
  user_id BIGSERIAL PRIMARY KEY,
  name VARCHAR(200) UNIQUE NOT NULL
);

CREATE TABLE razor_object (
  object_id BIGSERIAL PRIMARY KEY
);

CREATE TABLE razor_vote_item (
  vote_item_id BIGSERIAL PRIMARY KEY,
  parent_id BIGINT REFERENCES razor_vote_item ON DELETE RESTRICT UNIQUE,
  is_upvote BOOLEAN
);

CREATE TABLE razor_vote (
  vote_id BIGSERIAL PRIMARY KEY,
  vote_subject_id BIGINT NOT NULL,
  user_id BIGINT REFERENCES razor_user ON DELETE CASCADE NOT NULL,
  vote_item_id BIGINT REFERENCES razor_vote_item ON DELETE RESTRICT NOT NULL,
  UNIQUE(vote_subject_id, user_id)
);

CREATE TABLE razor_vote_subject (
  upvotes BIGINT DEFAULT 0 NOT NULL,
  downvotes BIGINT DEFAULT 0 NOT NULL
) INHERITS (razor_object);

CREATE TABLE razor_revision (
  revision_id BIGSERIAL PRIMARY KEY,
  parent_id BIGINT REFERENCES razor_revision ON DELETE RESTRICT DEFAULT NULL UNIQUE,
  user_id BIGINT REFERENCES razor_user ON DELETE SET NULL,
  revision_body TEXT,
  revision_time TIMESTAMP WITHOUT TIME ZONE
);

CREATE TABLE razor_post (
  post_subject_id BIGINT NOT NULL,
  parent_id BIGINT REFERENCES razor_post ON DELETE RESTRICT DEFAULT NULL,
  revision_id BIGINT REFERENCES razor_revision ON DELETE RESTRICT NOT NULL,
  user_id BIGINT REFERENCES razor_user ON DELETE SET NULL,
  post_time TIMESTAMP WITHOUT TIME ZONE,
  PRIMARY KEY(object_id)
) INHERITS (razor_object, razor_vote_subject);

CREATE TABLE razor_post_subject (
  post_count BIGINT DEFAULT 0 NOT NULL
) INHERITS (razor_object);

CREATE TABLE razor_thread (
  title VARCHAR(200) UNIQUE NOT NULL,
  PRIMARY KEY(object_id)
) INHERITS (razor_object, razor_post_subject);


CREATE TABLE razor_item (
  id BIGINT PRIMARY KEY,
  data TEXT
);
