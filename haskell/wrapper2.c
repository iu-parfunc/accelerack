// This version builds a *library* in C.

// Include Haskell FFI file, which we will use to initialize a Haskell runtime
#include "HsFFI.h"

/* #ifdef __GLASGOW_HASKELL__ */
//#include "Example_stub.h"
/* #endif */

extern void entrypoint(void);

int wrap(void)
{
  int argc = 2;
  char *argv[] = { "+RTS", "-A32m", 0 };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);

  // Make a call to Haskell code
  entrypoint();

  hs_exit();
}
