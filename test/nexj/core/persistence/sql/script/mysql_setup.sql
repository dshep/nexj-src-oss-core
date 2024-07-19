-- If encountering "Lock wait timeout exceeded; try restarting transaction" messages
--    then increase the value of "innodb_lock_wait_timeout" from default 50sec to 300sec in the "my.ini" file and restart the DB
--    @see http://dev.mysql.com/doc/refman/5.0/en/server-system-variables.html
--    @see http://dev.mysql.com/doc/refman/5.0/en/innodb-parameters.html#sysvar_innodb_lock_wait_timeout
--    to test:
--       1) Open a session in the db console, start a transaction and update the test.Mutex id column. Do not commit.
--       2) Start MySQL AdapterTest - it should time out after the 5 min.
-- In MySQL 5.0 CREATE TRIGGER requires the SUPER privilege. @see http://dev.mysql.com/doc/refman/5.0/en/create-trigger.html


create database test;
create user test identified by 'test';
grant all on test.* to 'test' with grant option;
grant super on *.* to 'test';
create table test.Mutex(id int primary key);
insert into test.Mutex(id) values (1);
