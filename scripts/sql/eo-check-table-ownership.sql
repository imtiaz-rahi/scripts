SELECT grantee, table_name, string_agg(privilege_type, ', ' ORDER BY privilege_type) AS privileges FROM information_schema.role_table_grants 
  WHERE grantee != 'postgres' and table_schema  = 'public' AND length(table_name)=2 GROUP BY 1, 2;
