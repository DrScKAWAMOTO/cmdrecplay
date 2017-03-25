/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:43:35 JST
 */

#include <sqlite3.h>

#include "debug.h"
#include "RecDatabase.h"
#include "Database.h"

void database_rec(const ParameterSet_s* parameter_set)
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
  sql = sqlite3_mprintf("INSERT INTO cmdrec VALUES (%Q,%Q,%Q,%Q,'execvp',0,0);",
                        parameterSet_refer_string_value(parameter_set,
                                                        PARAMETER_TYPE_RECFILE),
                        parameterSet_refer_string_value(parameter_set,
                                                        PARAMETER_TYPE_RECCMD),
                        parameterSet_refer_string_value(parameter_set,
                                                        PARAMETER_TYPE_RECCWD),
                        parameterSet_refer_string_value(parameter_set,
                                                        PARAMETER_TYPE_RECARGS));
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

/* Local Variables:     */
/* mode: c              */
/* End:                 */
