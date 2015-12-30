/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/01 23:55:21 JST
 */

#ifndef __PATTERN_H__
#define __PATTERN_H__

#include "ConfigFile.h"

#define NUMBER_OF_ELEMENTS 20
#define NUMBER_OF_PATTERNS 20

typedef enum {
  OPERATOR_TYPE_UNSPECIFIED = 0,
  OPERATOR_TYPE_PARAMETER = 1,
  OPERATOR_TYPE_PARAMETER_MATCHED = 2,
  OPERATOR_TYPE_PARAMETER_DIDNT_MATCH = 4,
  OPERATOR_TYPE_PARAMETER_EQUALS_TO_RECFILE = 8,
  OPERATOR_TYPE_STRING = 16,
} OperatorType_e;

typedef enum {
  PARAMETER_TYPE_UNSPECIFIED = 0,
  PARAMETER_TYPE_RECCMD = 1,
  PARAMETER_TYPE_RECFILE = 2,
  PARAMETER_TYPE_RECCWD = 4,
  PARAMETER_TYPE_RECARGS = 8,
  PARAMETER_TYPE_PLAYCMD = 16,
  PARAMETER_TYPE_PLAYFILE = 32,
  PARAMETER_TYPE_OPTFILE = 64,
  PARAMETER_TYPE_PLAYCWD = 128,
  PARAMETER_TYPE_PLAYARGS = 256,
  PARAMETER_TYPE_EXECCWD = 512,
  PARAMETER_TYPE_EXECCMD = 1024,
  PARAMETER_TYPE_EXECARGS = 2048,
  PARAMETER_TYPE_FUNCTION = 4096,
  PARAMETER_TYPE_PID = 8192,
  PARAMETER_TYPE_PPID = 16384,
} ParameterType_e;

typedef enum {
  DEFINITION_TYPE_UNSPECIFIED = 0,
  DEFINITION_TYPE_RECFILE = 1,
  DEFINITION_TYPE_RECMATCH = 2,
  DEFINITION_TYPE_PLAYFILE = 4,
  DEFINITION_TYPE_PLAYMATCH = 8,
  DEFINITION_TYPE_EXECCWD = 16,
  DEFINITION_TYPE_EXECCMD = 32,
  DEFINITION_TYPE_EXECARGS = 64,
  DEFINITION_TYPE_EXECENVS = 128,
} DefinitionType_e;

typedef struct {
  int line_no;
  OperatorType_e operator_type;
  ParameterType_e parameter_type;
  char* operand;
} Element_s;

typedef struct {
  DefinitionType_e definition_type;
  int line_no;
  int number_of_elements;
  Element_s elements[NUMBER_OF_ELEMENTS];
} Definition_s;

extern void definition_init(Definition_s* me);
extern void definition_term(Definition_s* me);
extern int definition_read_config(Definition_s* me,
                                  DefinitionType_e required_definition_type,
                                  ConfigFile_s* inpfile);
extern void definition_print(const Definition_s* me);

#endif /* __PATTERN_H__ */

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
