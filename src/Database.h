/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 14:13:02 JST
 */

#ifndef __DATABASE_H__
#define __DATABASE_H__

#define USE_TRANSACTION 0

extern sqlite3* database_init(void);
extern void database_term(sqlite3* db);
extern void database_sql_exec(sqlite3* db, const char* sql,
                              sqlite3_callback callback, int this_too);

#endif /* __DATABASE_H__ */

/* Local Variables:	*/
/* mode: c++		*/
/* End:			*/
