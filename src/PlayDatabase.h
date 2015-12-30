/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:27:35 JST
 */

#ifndef __PLAYDATABASE_H__
#define __PLAYDATABASE_H__

#include "ParameterSet.h"

/* database_play() */
/* return code == 2 ...... なぜか2つ以上マッチした。parameter_set は変化しない。 */
/* return code == 1 ...... 1つマッチした。parameter_set に記録時パラメータをセット。 */
/* return code == 0 ...... マッチしなかった。parameter_set は変化しない。 */
extern int database_play(ParameterSet_s* parameter_set);

#endif /* __PLAYDATABASE_H__ */

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
