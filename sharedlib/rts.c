// Include Haskell FFI file, which we will use to initialize a Haskell runtime
#include "HsFFI.h"

#include "Example_stub.h"

int init(void)
{
  int argc = 2;
  char *argv[] = { "+RTS", "-A32m", 0 };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);
  //entrypoint(4);  //this can be used in place of the the below function.
}

int this_function_is_never_called(int x)
{
  return entrypoint(x);
}

int close(void)
{
  hs_exit();
}
