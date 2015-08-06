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
  id   integer, -- fk into publication, NOT pk, one-to-many
  tag  text
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
