/*
 * Project: cmdrecplay
 * Version: 2.0
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/05/13 10:43:35 JST
 */

#include <sqlite3.h>
#include <string.h>
#include <stdio.h>

#include "config.h"
#include "debug.h"
#include "Database.h"

static int callback(void *arg, int argc, char **argv, char **column)
{
  for (int offset = 0; offset < argc; ++offset)
    {
      if ((strcmp(column[offset], "file") == 0) ||
          (strcmp(column[offset], "header") == 0))
        printf("%s\n", argv[offset]);
    }
  return SQLITE_OK;
}

int main(int argc, char* argv[])
{
  sqlite3 *db;
  /* SQLite データベースを open/create */
  db = database_init();
  /* query 実施 */
  database_sql_exec(db, "SELECT file FROM cmdrec;", callback, 0);
  /* query 実施 */
  database_sql_exec(db, "SELECT header FROM srcinc;", callback, 0);
  database_term(db);
  return 0;
}

/* Local Variables:     */
/* mode: c              */
/* End:                 */
