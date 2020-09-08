#include <stdio.h>
#include <stdlib.h>

extern long effektMain();

int main(int argc, char** argv) {
  long result = effektMain();
  printf("%ld\n", result);
  return 0;
}
