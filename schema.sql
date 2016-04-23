----------------------------------------
-- pub structure
----------------------------------------

create table if not exists "publication_types" (
  id               integer,
  publication_type text
);

insert into "publication_types" (id, publication_type) select 0, 'story' where not exists (select 1 from "publication_types" where id = 0);

insert into "publication_types" (id, publication_type) select 1, 'comment' where not exists (select 1 from "publication_types" where id = 1);

create table if not exists "publications" (
  id          integer,
  type_id     integer,
  username    text, -- username fk, 'by' in structure
  time        integer,
  date        text, -- \todo remove? calculate from time?
  url         text,
  title       text,
  mail        boolean,
  tag         text, -- \todo make fk?
  tag2        text, -- \todo make fk?
  text        text,
  md          text, -- \todo remove, should be calculated
  web_domain  text,
  score       integer,
  deleted     boolean,
  draft       boolean,
  parent_id   integer, -- fk into publication
  locked      boolean,
  no_kill     boolean
);

-- maps to cc field in pub structure
create table if not exists "publication_cc" (
  id       integer, -- fk into publication, NOT pk, one-to-many
  username text
);

-- maps to ctag field in pub structure
create table if not exists "publication_community_tags" (
  id   integer, -- fk into publication, NOT pk, one-to-many
  tag  text
);

-- \todo figure out why there are both 'ctag' and 'ctags'
-- maps to ctags field in pub structure
create table if not exists "publication_community_tagses" (
  id       integer, -- fk into publication, NOT pk, one-to-many
  username text,
  tag      text
);

-- \todo remove this, when a fulltext search solution exists
create table if not exists "publication_search_text" (
  id   integer, -- fk into publication, NOT a pk, one pub has many ctagses
  word text
);

-- \todo remove this, when a fulltext search solution exists
create table if not exists "publication_search_title" (
  id   integer, -- fk into publication, NOT a pk, one pub has many ctagses
  word text
);

-- \todo remove this, when a fulltext search solution exists
create table if not exists "publication_search_url" (
  id   integer, -- fk into publication, NOT a pk, one to many
  word text
);

create table if not exists "publication_votes" (
  id       integer, -- fk into publication, NOT a pk, one to many
  vote_id  integer, -- fk int votes (which doesn't have a table yet)
  username text, -- fk into usernames. Probably duplicate data
  up       boolean, -- redundant? Do downvotes exist in hubski?
  num      integer -- \todo figure out what this is used for. Weight?
);

create table if not exists "publication_saved_by" (
  id       integer, -- fk into publication, NOT a pk, one to many
  username text 
);

create table if not exists "publication_shared_by" (
  id       integer, -- fk into publication, NOT a pk, one to many
  username text 
);

create table if not exists "publication_badged_by" (
  id       integer, -- fk into publication, NOT a pk, one to many
  username text 
);

-- \todo remove, replace with query
create table if not exists "publication_badged_kids" (
  id     integer, -- fk into publication, NOT a pk, one to many
  kid_id integer -- fk into publication
);

create table if not exists "publication_cubbed_by" (
  id       integer, -- fk into publication, NOT a pk, one to many
  username text
);

create table if not exists "publication_kids" (
  id     integer, -- fk into publication, NOT a pk, one to many
  kid_id integer  -- fk into publication
);

create table if not exists "donations" (
  username text, -- (will be) fk into users, NOT a pk, one to many
  donation_cents integer,
  donation_time timestamp
);


----------------------------------------
-- profile structure
----------------------------------------

-- \todo determine 'email' type
-- \todo put uncommon fields (like spammer) in their own table ?
create table if not exists "users" (
  id                      text, -- pk
  joined                  timestamp,
  inactive                boolean,
  clout                   double precision,
  word_count              boolean,
  average_com_numerator   integer,
  average_com_denominator integer,
  ignore_newbies          boolean,
  global_ignored          boolean,
  new_tabs                boolean,
  publication_tabs        boolean,
  reply_alerts            boolean,
  pm_alerts               boolean,
  follower_alerts         boolean,
  shout_outs              boolean,
  badge_alerts            boolean,
  saved_notifications     boolean,
  feed_times              boolean,
  share_counts            boolean,
  show_global_filtered    boolean,
  follows_badges          boolean,
  embed_videos            boolean,
  bio                     text,
  email                   boolean,
  hubski_style            text,
  homepage                text,
  follower_count          integer, -- \todo remove, duplicate of sum(users_followers)
  posts_count             integer, -- \todo remove, duplicate of sum(publications)
  shareds_count           integer,  -- \todo remove, duplicate of sum(publications_sharedby)
  unread_notifications    boolean,  -- \todo remove, duplicate of notifications
  last_com_time           timestamp,
  com_clout_date          timestamp,
  zen                     boolean,
  spammer                 boolean
);

create table if not exists "users_submitted" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

create table if not exists "users_saved" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

-- \todo determine if used, remove if not
create table if not exists "users_sticky" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

-- \todo determine if used, remove if not
create table if not exists "users_hidden" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

create table if not exists "users_mail" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

create table if not exists "users_drafts" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

create table if not exists "users_shareds" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

create table if not exists "users_cubbeds" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

-- \todo remove, duplicate of publication_votes
create table if not exists "users_votes" (
  id             text, -- fk into users, NOT a pk, one to many
  publication_id integer,
  vote_id        integer, -- fk int votes (which doesn't have a table yet)
  username       text, -- fk into usernames. Probably duplicate data
  web_domain     text,
  up             boolean -- redundant? Do downvotes exist in hubski?
);

-- \todo remove, duplicate of publication_community_tagses ?
create table if not exists "users_suggested_tags" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer, -- fk into publications
       tag text
);

-- \todo remove, duplicate of publication_badged_by
create table if not exists "users_badged" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

-- \todo remove, duplicate of publication_badged_by
create table if not exists "users_badges" (
       id text, -- fk into users, NOT a pk, one to many
       publication_id integer -- fk into publications
);

create table if not exists "users_ignoring" (
       id text, -- fk into users, NOT a pk, one to many
       ignoring_id text -- fk into users
);

create table if not exists "users_muting" (
       id text, -- fk into users, NOT a pk, one to many
       muting_id text -- fk into users
);

create table if not exists "users_hushing" (
       id text, -- fk into users, NOT a pk, one to many
       hushing_id text -- fk into users
);

create table if not exists "users_blocking" (
       id text, -- fk into users, NOT a pk, one to many
       blocking_id text -- fk into users
);

create table if not exists "users_ignoring_tag" (
       id text, -- fk into users, NOT a pk, one to many
       tag text
);

-- \todo determine if used
-- \todo determine what a 'dom' is, and its type
create table if not exists "users_ignoring_dom" (
       id text, -- fk into users, NOT a pk, one to many
       dom text
);

-- \todo remove, duplicate of users_ignoring
create table if not exists "users_ignored_by" (
       id text, -- fk into users, NOT a pk, one to many
       by_id text -- fk into users
);

-- \todo remove, duplicate of users_muting
create table if not exists "users_muted_by" (
       id text, -- fk into users, NOT a pk, one to many
       by_id text -- fk into users
);

-- \todo remove, duplicate of users_hushing
create table if not exists "users_hushed_by" (
       id text, -- fk into users, NOT a pk, one to many
       by_id text -- fk into users
);

-- \todo remove, duplicate of users_blocking
create table if not exists "users_blocked_by" (
       id text, -- fk into users, NOT a pk, one to many
       by_id text -- fk into users
);

create table if not exists "users_followed" (
       id text, -- fk into users, NOT a pk, one to many
       followed_id text -- fk into users
);

-- \todo remove, duplicate of users_followed
create table if not exists "users_follower" (
       id text, -- fk into users, NOT a pk, one to many
       follower_id text -- fk into users
);

-- \todo remove, duplicate of data in publications
create table if not exists "users_personal_tags" (
       id text, -- fk into users, NOT a pk, one to many
       tag text -- fk into users
);

create table if not exists "users_followed_tags" (
       id text, -- fk into users, NOT a pk, one to many
       tag text -- fk into users
);

-- \todo determine what 'dom' is, and its type
create table if not exists "users_followed_dom" (
       id text, -- fk into users, NOT a pk, one to many
       dom text -- fk into users
);

-- \todo determine if duplicate, if used
create table if not exists "users_notified" (
  id text, -- fk into users, NOT a pk, one to many
  notified_id integer -- fk into publications?
);

create table if not exists "users_password_hashes" (
  id text, -- pk, fk into users
  hash_type text,
  hash1 text,
  hash2 text
);

create table if not exists "users_cookies" (
  id text, -- pk, fk into users
  cookie text
);

create table if not exists "users_emails" (
  id text, -- pk, fk into users
  email text
);
