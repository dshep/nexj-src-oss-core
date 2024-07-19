-- Used by insert scripts to create BLOBs from bytea
create function blob_write(data bytea)
returns oid volatile
as $f$
declare
   loid oid;
   lfd integer;
   lsize integer;
begin
   if(data is null) then
      return null;
   end if;

   loid := lo_create(0);
   lfd := lo_open(loid,131072);
   lsize := lowrite(lfd,data);
   perform lo_close(lfd);

   return loid;
end;
$f$
language plpgsql
;
-- Used by select statements for comparision of lo with bytea
create function blob_read(loid oid)
returns bytea volatile
as $f$
declare
   data bytea;
   lfd integer;
   lsize integer;
begin
   if(loid is null) then
      return null;
   end if;

   lfd := lo_open(loid,262144);
   lsize := lo_lseek(lfd,0,2);
   perform lo_lseek(lfd,0,0);
   data := loread(lfd,lsize);
   perform lo_close(lfd);

   return data;
end;
$f$
language plpgsql
;
create function encode(loid oid, type text)
returns text
as $f$ begin return encode(blob_read(loid), type); end; $f$
language plpgsql stable strict
