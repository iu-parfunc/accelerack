#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
  char *type;
  int *shape;
  void *data;
} acc_data;


acc_data * createStruct (void *data, int *shape, char *str, char *type)
{
   acc_data *ac_data =  (acc_data*) malloc (1*sizeof(acc_data));
   int i = 0;

   if (strcmp(type, "Double"))
     ac_data->type = "Double";
   else if (strcmp(type, "Int")) 
     ac_data->type = "Int";

   ac_data->data = data;
   ac_data->shape = shape;

   return ac_data;
}

char* modify (acc_data *ac_data, int length, char *type)
{
   int i = 0;

   if (0 == (strncmp(type, "Double", 6)))
   {
     double *x = (double*)ac_data->data;
     for (i = 0; i < length; i++)
     {
       x[i] = x[i] * 4;
     }
   } 
   else if (0 == (strncmp(type, "Int", 3)))
   {
     int *x = (int*)ac_data->data; 
     for (i = 0; i < length; i++)
     {
       x[i] = x[i] * 4;
     }
   }

   return type;
}

