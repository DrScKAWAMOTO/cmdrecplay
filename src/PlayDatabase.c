/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:43:35 JST
 */

#include <sqlite3.h>
#include <string.h>
#include <assert.h>

#include "debug.h"
#include "PlayDatabase.h"
#include "Database.h"

static int count;
static ParameterSet_s* paramset;

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
      char* string;
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 3
      fprintf(stderr, "%s = '%s'\n", column[offset], argv[offset]);
#endif
#endif
      if (strcmp(column[offset], "file") == 0)
        {
          const char* text = 
            parameterSet_refer_string_value(paramset, PARAMETER_TYPE_RECFILE);
          assert(strcmp(text, argv[offset]) == 0);
        }
      else if (strcmp(column[offset], "cmd") == 0)
        parameterSet_set_by_copy_string(paramset, PARAMETER_TYPE_RECCMD, argv[offset]);
      else if (strcmp(column[offset], "cwd") == 0)
        parameterSet_set_by_copy_string(paramset, PARAMETER_TYPE_RECCWD, argv[offset]);
      else if (strcmp(column[offset], "args") == 0)
        parameterSet_set_by_copy_string(paramset, PARAMETER_TYPE_RECARGS, argv[offset]);
      else if (strcmp(column[offset], "function") == 0)
        parameterSet_set_by_copy_string(paramset, PARAMETER_TYPE_FUNCTION, argv[offset]);
      else if (strcmp(column[offset], "pid") == 0)
        parameterSet_set_by_copy_string(paramset, PARAMETER_TYPE_PID, argv[offset]);
      else if (strcmp(column[offset], "ppid") == 0)
        parameterSet_set_by_copy_string(paramset, PARAMETER_TYPE_PPID, argv[offset]);
    }
  return SQLITE_OK;
}

int database_play(ParameterSet_s* parameter_set)
{
  sqlite3 *db;
  char *sql;
  /* SQLite データベースを open/create */
  db = database_init();
#if USE_TRANSACTION
#if DEBUG_LEVEL >= 3
  /* BEGIN TRANSACTION */
  database_sql_exec(db, "BEGIN", 0, 0);
#endif
#endif
  /* SELECT */
  count = 0;
  paramset = parameter_set;
  sql = sqlite3_mprintf("SELECT * FROM cmdrec WHERE file = %Q;",
                        parameterSet_refer_string_value(parameter_set,
                                                        PARAMETER_TYPE_RECFILE));
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 3
  fprintf(stderr, "sql = %s\n", sql);
#endif
#endif
  database_sql_exec(db, sql, callback, 0);
#if USE_TRANSACTION
  /* COMMIT TRANSACTION */
  database_sql_exec(db, "COMMIT", 0, 0);
#endif
  database_term(db);
  return count;
}

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
