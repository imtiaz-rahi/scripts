-- https://wiki.postgresql.org/wiki/Index_Maintenance

WITH RECURSIVE index_details AS (
  SELECT
    'pgbench_accounts_pkey'::text idx
),
size_in_pages_index AS (
  SELECT
    (pg_relation_size(idx::regclass) / (2^13))::int4 size_pages
  FROM
    index_details
),
page_stats AS (
  SELECT
    index_details.*,
    stats.*
  FROM
    index_details,
    size_in_pages_index,
    lateral (SELECT i FROM generate_series(1, size_pages - 1) i) series,
    lateral (SELECT * FROM bt_page_stats(idx, i)) stats
),
meta_stats AS (
  SELECT
    *
  FROM
    index_details s,
    lateral (SELECT * FROM bt_metap(s.idx)) meta
),
pages_raw AS (
  SELECT
    *
  FROM
    page_stats
  ORDER BY
    btpo DESC
),
/* XXX: Note ordering dependency within this CTE */
pages_walk(item, blk, level) AS (
  SELECT
    1,
    blkno,
    btpo
  FROM
    pages_raw
  WHERE
    btpo_prev = 0
    AND btpo = (SELECT level FROM meta_stats)
  UNION
  SELECT
    CASE WHEN level = btpo THEN w.item + 1 ELSE 1 END,
    blkno,
    btpo
  FROM
    pages_raw i,
    pages_walk w
  WHERE
    i.btpo_prev = w.blk OR (btpo_prev = 0 AND btpo = w.level - 1)
)
SELECT
  /* Uncomment if these details interesting */
  /*
  idx,
  btpo_prev,
  btpo_next,
  */
 
  /*
   * "level" is level of tree -- 0 is leaf.  First tuple returned is root.
   */
  btpo AS level,
 
  /*
   * Ordinal number of item on this level
   */
  item AS l_item,
 
  /*
   * Block number, and details of page
   */
  blkno,
  btpo_flags,
  TYPE,
  live_items,
  dead_items,
  avg_item_size,
  page_size,
  free_size,
 
  /*
   * distinct_real_item_keys is how many distinct "data" fields on page
   * (excludes highkey).
   *
   * If this is less than distinct_block_pointers on an internal page, that
   * means that there are so many duplicates in its children that there are
   * duplicate high keys in children, so the index is probably pretty bloated.
   *
   * Even unique indexes can have duplicates.  It's sometimes interesting to
   * watch out for how many distinct real items there are within leaf pages,
   * compared to the number of live items, or total number of items.  Ideally,
   * these will all be exactly the same for unique indexes.
   */
  distinct_real_item_keys,
 
  /*
   * Per pageinspect docs, first item on non-rightmost page on level is "high
   * key" item, which represents an upper bound on items on the page.
   * (Rightmost pages are sometimes considered to have a conceptual "positive
   * infinity" item, and are shown to have a high key that's NULL by this query)
   *
   * This can be used to visualize how finely or coarsely separated the
   * keyspace is.
   *
   * Note that below int4_from_page_data() function could produce more useful
   * visualization of split points.
   */
  CASE WHEN btpo_next != 0 THEN first_item END AS highkey,
 
  /*
   * distinct_block_pointers is table blocks that are pointed to by items on
   * the page (not including high key, which doesn't point anywhere).
   *
   * This is interesting on leaf pages, because it indicates how fragmented the
   * index is with respect to table accesses, which is important for range
   * queries.
   *
   * This should be redundant on internal levels, because all downlinks in internal
   * pages point to distinct blocks in level below.
   */
  distinct_block_pointers
 
FROM
  pages_walk w,
  pages_raw i,
  lateral (
    SELECT
    COUNT(DISTINCT (CASE WHEN btpo_next = 0 OR itemoffset > 1 THEN (DATA COLLATE "C") END)) AS distinct_real_item_keys,
    COUNT(DISTINCT (CASE WHEN btpo_next = 0 OR itemoffset > 1 THEN (ctid::text::point)[0]::BIGINT END)) AS distinct_block_pointers,
    (array_agg(DATA))[1] AS first_item
    FROM bt_page_items(idx, blkno)
  ) items
  WHERE w.blk = i.blkno
  /* Uncomment to avoid showing leaf level (faster): */
  /* and level > 0*/
ORDER BY btpo DESC, item;
