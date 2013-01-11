#include <stdio.h>

#define UPPER_BOUND 232792560


long search (int limit)
{
  int   rest = 1;
  long  x = limit;
  int   i;

  while (1) {
    for (i = 1; i <= limit; i++) {
      rest = x % i;
      if (rest != 0) {
        x += limit;
        break;
      }
    }

    /* i iterated to the limit which means, all numbers are
     * multiplicates of limit */
    if ( i >= limit) {
      return x;
    }

    /* this is my emergency break */
    if (x == UPPER_BOUND) {
      return x;
    }
  }
}

int main(int argc, char *argv[]) {
  printf("10: %ld\n", search(10));
  printf("20: %ld\n", search(20));
  return 1;
}
