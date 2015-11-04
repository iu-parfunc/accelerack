/******************************************************************
* Header file for acc_cfile.c (for testing)                       *
*******************************************************************/

#include <stdio.h>
#include <stdbool.h>
#include <string.h>

typedef enum
{
  int_value,
  double_value,
  bool_value,
  acc_payload_ptr,
  scalar,
  tuple,
  gcpointer,
  rkt_payload_ptr
}types;

typedef struct
{
  int length;
  int type;
  void *data;
}cvector;

typedef struct
{
  int type;
  cvector *shape;
  cvector *data;
}carray;

void modify_vector (cvector *, int);
void modify_array (carray *, char *);
void rkt_handler (carray *, char *);
