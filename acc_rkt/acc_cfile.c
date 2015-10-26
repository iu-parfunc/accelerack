#include <stdio.h>
#include <stdbool.h>

/*
#define C_INT 0
#define C_DOUBLE 1
#define C_BOOL 2
#define C_PTR 3
#define ACC_PAYLOAD_PTR 4
#define RKT_PAYLOAD_PTR 5
#define SCALAR_PAYLOAD 6
#define TUPLE_PAYLOAD 7
*/

int C_INT = 0;
int C_DOUBLE = 1;
int C_BOOL = 2;
int C_PTR = 3;
int ACC_PAYLOAD_PTR = 4;
int RKT_PAYLOAD_PTR = 5;
int SCALAR_PAYLOAD = 6;
int TUPLE_PAYLOAD = 7;


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


void modify_vector (cvector *cv1, int value)
{
  int i = 0;

  switch (cv1->type)
  {
    case acc_payload_ptr:
    {
      cvector **data = (cvector**) cv1->data;

      for (i = 0; i < cv1->length; i++)
      {
        modify_vector (data[i], value);
      }

      break;
    }

    case double_value:
    {
      double *data = (double*) cv1->data;

      for (i = 0; i < cv1->length; i++)
      {
	data[i] += (		double) value;
      }

      break;
    }

    case int_value:
    {
      int *data = (int*) cv1->data;

      for (i = 0; i < cv1->length; i++)
      {
	data[i] += value;
      }

      break;
    }

    case bool_value:
    {
      bool *data = (bool*) cv1->data;
	
      //for (i = 0; i < cv1->length; i++)

      break;
    }
  }
}


void modify_array (carray *c1, char *expr)
{
  if (scalar == c1->type)
  {
    printf("Scalar data :\n");
  }
  else
  {
    printf("Tuple data :\n");
  }

  int data = 0;

  if (0 == strcmp("add1", expr))
  {
    modify_vector(c1->data, 1);
  }
  else if (0 == strcmp("sub1", expr))
  {
    modify_vector(c1->data, -1);
  }
}

void rkt_handler (carray *c1, char *expr)
{
  modify_array(c1, expr);
}
