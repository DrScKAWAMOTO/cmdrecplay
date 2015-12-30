/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/03 22:39:22 JST
 */

#ifndef __REGEXP_H__
#define __REGEXP_H__

typedef struct {
  unsigned int match_start_pos;
  unsigned int match_end_pos;
} MatchLocation_s;

typedef struct {
  int unmatched_count;
  char* unmatched_lines;
  int matched_count;
  char* matched_lines;
} MatchLines_s;

extern int regexp_search(MatchLocation_s* result, const char* pattern, const char* text,
                         unsigned int search_start_pos, unsigned int search_end_pos);
extern void regexp_search_all(MatchLines_s* result, const char* pattern,
                              const char* text);
extern void match_lines_term(MatchLines_s* me);

#endif /* __REGEXP_H__ */

/* Local Variables:	*/
/* mode: c++		*/
/* End:			*/
