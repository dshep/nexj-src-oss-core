-- NOTE: in postgresql.conf set max_prepared_transactions to >= max_connections and restart the server.

create user test with password 'test';
alter user test set time zone 'UTC';
create tablespace nexjtest01 owner test location 'c:/postgresql/data/nexj';
create database test owner test template template0 encoding 'UTF8'
   lc_collate 'english' lc_ctype 'english' tablespace nexjtest01;
\c test
create schema authorization test;
create table test.Mutex(id integer, constraint Mutex_PK primary key (id) using index tablespace nexjtest01) tablespace nexjtest01;
alter table test.Mutex owner to test;
insert into test.Mutex(id) values (1);

create cast (bytea as oid) with function blob_write(bytea) as assignment;
create cast (oid as bytea) with function blob_read(oid) as implicit;

-- Enable implicit cast of integer to boolean
update pg_cast set castcontext = 'i' where oid in (
   select c.oid
   from pg_cast c
   inner join pg_type src on src.oid = c.castsource
   inner join pg_type tgt on tgt.oid = c.casttarget
   where src.typname like 'int%' and tgt.typname like 'bool%'
);

-- NOTE: In PostgreSQL <9.1, "create extension" is not available,
-- so run the following scripts manually from the share/contrib directory:
-- \i 'C:/Program Files (x86)/PostgreSQL/9.1/share/contrib/fuzzystrmatch.sql'
-- \i 'C:/Program Files (x86)/PostgreSQL/9.1/share/contrib/lo.sql'
-- \i 'C:/Program Files (x86)/PostgreSQL/9.1/share/contrib/uuid-ossp.sql'
-- uuid-ossp.sql was not available in Windows x64 installers, but it can be downloaded
-- from http://winpg.jp/~saito/pg_work/OSSP_win32/build-x86-64/pg90_uuid_ossp_x64.zip
-- and unpacked in the PostgreSQL installation directory.

create extension fuzzystrmatch;
create extension lo;
create extension "uuid-ossp";
