#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
  char *type;
  int *shape;
  void *data;
} acc_data;

int myfunc (int length, double *x, int (*handler) (int length, double *arr))
{
   printf("hai world\n");
   handler(length, x);
   int i = 0;

   for (i = 0; i < length; i++)
   {
     printf("%f\n", x[i]);
   }

   return 0;
}

acc_data * myfunc2 (int length, void *data, int *shape, char *str, char *type)
{
   acc_data *ac_data =  (acc_data*) malloc (1*sizeof(acc_data));
   int i = 0;

   //ac_data = (acc_data*) malloc (1*sizeof(acc_data));

   if (strcmp(type, "Double"))
     ac_data->type = "Double"; 

   ac_data->data = data;
   ac_data->shape = shape;

   double *x = (double*)ac_data->data;

   /*for (i = 0; i < length; i++)
   {
     x[i] = i*3;
   }*/

   return ac_data;
}

int myfunc3 (acc_data *ac_data, int length)
{
   int i = 0;

   double *x = (double*)ac_data->data; 

   for (i = 0; i < length; i++)
   {
     x[i] = i*4;
   }

   return 0;
}

/*int main()
{
  double x;
  myfunc(3, &x, handler);
  return 0;
}*/
