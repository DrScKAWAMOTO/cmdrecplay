/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 11:08:33 JST
 */

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "RecPattern.h"
#include "Regexp.h"

void rec_pattern_init(RecPattern_s* me)
{
  definition_init(&(me->recfile));
  definition_init(&(me->recmatch));
}

void rec_pattern_read_config(RecPattern_s* me, ConfigFile_s* inpfile)
{
  config_file_read_keyword(inpfile, "rec");
  config_file_read_keyword(inpfile, "pattern");
  config_file_read_keyword(inpfile, "{");
  if (definition_read_config(&(me->recfile), DEFINITION_TYPE_RECFILE, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `recfile:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  if (definition_read_config(&(me->recmatch), DEFINITION_TYPE_RECMATCH, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `recmatch:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  config_file_read_keyword(inpfile, "}");
}

int rec_pattern_apply(const RecPattern_s* me, ParameterSet_s* parameter_set)
{
  MatchLines_s mli;
  MatchLocation_s mlo;
  unsigned int start_pos;
  unsigned int end_pos;
  const char* text;
  /* recfile: を適用する */
  assert(me->recfile.definition_type == DEFINITION_TYPE_RECFILE);
  assert(me->recfile.number_of_elements == 1);
  /* recargs =~ /.../ */
  /* recargs !~ /.../ */
  assert((me->recfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
         (me->recfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH));
  assert(me->recfile.elements[0].parameter_type == PARAMETER_TYPE_RECARGS);
  /* 適用 */
  regexp_search_all(&mli, me->recfile.elements[0].operand,
                    parameterSet_refer_string_value(parameter_set,
                                                    PARAMETER_TYPE_RECARGS));
  if (me->recfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED)
    {
      if (mli.matched_count == 1)
        {
          mli.matched_lines[strlen(mli.matched_lines) - 1] = '\0';
          parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_RECFILE,
                                               mli.matched_lines);
          parameterSet_set_string_value(parameter_set, PARAMETER_TYPE_RECARGS,
                                        mli.unmatched_lines);
          mli.matched_lines = NULL;
          mli.unmatched_lines = NULL;
        }
      else
        {
          match_lines_term(&mli);
          return -1;
        }
    }
  else
    {
      if (mli.unmatched_count == 1)
        {
          mli.unmatched_lines[strlen(mli.unmatched_lines) - 1] = '\0';
          parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_RECFILE,
                                               mli.unmatched_lines);
          parameterSet_set_string_value(parameter_set, PARAMETER_TYPE_RECARGS,
                                        mli.matched_lines);
          mli.unmatched_lines = NULL;
          match_lines_term(&mli);
        }
      else
        {
          match_lines_term(&mli);
          return -1;
        }
    }

  /* recmatch: を適用する */
  assert(me->recmatch.number_of_elements == 1);
  assert(me->recmatch.definition_type == DEFINITION_TYPE_RECMATCH);
  assert(me->recmatch.number_of_elements == 1);
  /* reccmd =~ /.../ */
  /* reccmd !~ /.../ */
  assert((me->recmatch.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
         (me->recmatch.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH));
  assert(me->recmatch.elements[0].parameter_type == PARAMETER_TYPE_RECCMD);
  /* 適用 */
  text = parameterSet_refer_string_value(parameter_set, PARAMETER_TYPE_RECCMD);
  start_pos = 0;
  end_pos = strlen(text);
  return regexp_search(&mlo, me->recmatch.elements[0].operand, text, start_pos, end_pos);
}

void rec_pattern_print(RecPattern_s* me)
{
  fprintf(stderr, "rec pattern {\n");
  definition_print(&(me->recfile));
  definition_print(&(me->recmatch));
  fprintf(stderr, "}\n");
}

void rec_pattern_term(RecPattern_s* me)
{
  definition_term(&me->recfile);
  definition_term(&me->recmatch);
}

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
