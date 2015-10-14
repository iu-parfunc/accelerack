#include <stdio.h>
#include <string.h>

char *acc_str = "Hello World!";
int argc_value = 100;

typedef struct
{
  char *str;
}accelerack;

void setString (char *str)
{
  acc_str = str;
}

void getString (char *str)
{
  strcpy(str, acc_str);
}

/*int main()
{
  char str[10];
  accelerack acc;
  setString("New String");
  getString(str);
  printf("%s\n", str);

  return 0;
}*/
