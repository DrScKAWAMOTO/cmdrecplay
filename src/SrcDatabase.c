/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:43:35 JST
 */

#include <sqlite3.h>
#include <stdio.h>
#include <string.h>

#include "debug.h"
#include "SrcDatabase.h"
#include "Database.h"

void database_hedsrc(const char* header, const char* source)
{
  sqlite3 *db;
  char *sql;
  /* SQLite データベースを open/create */
  db = database_init();
#if USE_TRANSACTION
  /* BEGIN TRANSACTION */
  database_sql_exec(db, "BEGIN", 0, 0);
#endif
  /* INSERT */
  sql = sqlite3_mprintf("INSERT INTO srcinc VALUES (%Q,%Q);", header, source);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 3
  fprintf(stderr, "sql = `%s'\n", sql);
#endif
#endif
  database_sql_exec(db, sql, 0, 0);
  sqlite3_free(sql);
#if USE_TRANSACTION
  /* COMMIT TRANSACTION */
  database_sql_exec(db, "COMMIT", 0, 0);
#endif
  database_term(db);
}

static int count;
static char* source_ptr;

static int callback(void *arg, int argc, char **argv, char **column)
{
  if (count == 0)
    count = 1;
  else
    {
      count = 2;
      return SQLITE_OK;
    }
  for (int offset = 0; offset < argc; ++offset)
    {
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 3
      fprintf(stderr, "%s = '%s'\n", column[offset], argv[offset]);
#endif
#endif
      if (strcmp(column[offset], "source") == 0)
        strcpy(source_ptr, argv[offset]);
    }
  return SQLITE_OK;
}

int database_hedto(const char* header, char* source)
{
  sqlite3 *db;
  char *sql;
  /* SQLite データベースを open/create */
  db = database_init();
  /* SELECT */
  sql = sqlite3_mprintf("SELECT * FROM srcinc WHERE header = %Q;", header);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 3
  fprintf(stderr, "sql = %s\n", sql);
#endif
#endif
  count = 0;
  source_ptr = source;
  database_sql_exec(db, sql, callback, 0);
  sqlite3_free(sql);
  database_term(db);
  return count;
}

/* Local Variables:     */
/* mode: c              */
/* End:                 */
