// Include Haskell FFI file, which we will use to initialize a Haskell runtime
#include "HsFFI.h"

void ark_init(void)
{
  int argc = 2;
  char *argv[] = { "+RTS", "-A32m", 0 };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);
}

int ark_exit(void)
{
  hs_exit();
}
