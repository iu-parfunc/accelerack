#include <stdio.h>
#include <stdbool.h>
#include <string.h>

/*
#define C_INT 0
#define C_DOUBLE 1
#define C_BOOL 2
#define C_PTR 3
#define ACC_PAYLOAD_PTR 4
#define RKT_PAYLOAD_PTR 5
#define SCALAR_PAYLOAD 6
#define TUPLE_PAYLOAD 7*/


extern int C_INT;
extern int C_DOUBLE;
extern int C_BOOL;
extern int ACC_PAYLOAD_PTR;
extern int SCALAR_PAYLOAD;
extern int TUPLE_PAYLOAD;
extern int C_PTR;
extern int RKT_PAYLOAD_PTR;


typedef enum
{
  int_value,
  double_value,
  bool_value,
  gcpointer,
  acc_payload_ptr,
  rkt_payload_ptr,
  scalar,
  tuple
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
