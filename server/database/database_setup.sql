begin;
do $$
begin
  create table "data" ("data" jsonb);
end $$ language plpgsql;
commit;