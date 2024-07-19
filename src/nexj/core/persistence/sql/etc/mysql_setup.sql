create database ${database:nexj};

grant select, insert, update, delete, references on ${database:nexj}.* to '${user:nexj}' identified by '${password:nexj}';
grant select, insert, update, delete, references on ${database:nexj}.* to '${user:nexj}'@'localhost' identified by '${password:nexj}';

${iftest:
-- '${user:nexj}' has full access to all tables.
-- This user is used for DB administration such as schema upgrades (DDL), etc.

grant all on ${database:nexj}.* TO '${user:nexj}' identified by '${password:nexj}' with grant option;

-- Add @localhost to handle default MySQL servers which have Host='localhost', User=''
-- (see "Causes of Access-Denied Errors" in MySQL docs)

grant all on ${database:nexj}.* TO '${user:nexj}'@'localhost' identified by '${password:nexj}' with grant option;
}