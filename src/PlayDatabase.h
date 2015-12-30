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
/* return code == 2 ...... �ʤ���2�İʾ�ޥå�������parameter_set ���Ѳ����ʤ��� */
/* return code == 1 ...... 1�ĥޥå�������parameter_set �˵�Ͽ���ѥ�᡼���򥻥åȡ� */
/* return code == 0 ...... �ޥå����ʤ��ä���parameter_set ���Ѳ����ʤ��� */
extern int database_play(ParameterSet_s* parameter_set);

#endif /* __PLAYDATABASE_H__ */

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
