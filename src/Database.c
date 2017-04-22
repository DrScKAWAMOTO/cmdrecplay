/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:43:35 JST
 */

#include <sqlite3.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "Database.h"

#define DATABASE_CREATE_CMDREC_TABLE_SQL "CREATE TABLE cmdrec (file text primary key on conflict replace, cmd text, cwd text, args text, function text, pid text, ppid text);"
#define DATABASE_CREATE_SRCINC_TABLE_SQL "CREATE TABLE srcinc (header text primary key on conflict replace, source text);"

sqlite3* database_init(void)
{
  sqlite3 *db;
  int result;
  char *sql;
  const char* home;
  char database_file_name[PATH_MAX];
  /* SQLite データベースを open/create */
  home = (const char*)getenv("HOME");
  if (home)
    sprintf(database_file_name, "%s/.cmdrec.db", home);
  else
    strcpy(database_file_name, "cmdrec.db");
  result = sqlite3_open_v2(database_file_name, &db,
                           SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);
  if (!db)
    {
      fprintf(stderr, "sqlite3_open_v2(): error result = %d\n", result);
      exit(EXIT_FAILURE);
    }
  /* CREATE TABLE(既に存在しておればresult=1) */
  database_sql_exec(db, DATABASE_CREATE_CMDREC_TABLE_SQL, 0, 1);
  database_sql_exec(db, DATABASE_CREATE_SRCINC_TABLE_SQL, 0, 1);
  return db;
}

void database_term(sqlite3* db)
{
  sqlite3_close(db);
}

void database_sql_exec(sqlite3* db, const char* sql,
                       sqlite3_callback callback, int this_too)
{
  int result;
  char *errmsg;

  result = sqlite3_exec(db, sql, callback, NULL, &errmsg);
  if ((result != 0) && (result != this_too))
    {
      fprintf(stderr, "sqlite3_exec(\"%s\"): (%d) %s\n", sql, result, errmsg);
      exit(EXIT_FAILURE);
    }
}

/* Local Variables:     */
/* mode: c              */
/* End:                 */
