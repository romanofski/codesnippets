#include <stdio.h>
#include <stdlib.h>

#define UPPER_BOUND 232792560


/* prototypes */
void get_searchspace (int *space, int length);

void get_searchspace (int *space,
                      int length)
{
  int i;

  /* calculate all multiples of i and add it to our array if it's not in
   * there */
  for (i = 1; i <= length; i++) {
    space[i-1] = i;
  }
}

long search (int limit)
{
  int   rest = 1;
  long  x = limit;
  int  *j = malloc(limit * sizeof(int));
  int  *start;

  get_searchspace(j, limit);
  start = j;

  while (*j <= limit) {
    rest = x % *j;
    if (rest != 0) {
      x += limit;
      j = start;
      continue;
    }
    j++;

    if (*j == 0) {
      return x;
    }

    /* this is my emergency break */
    if (x == UPPER_BOUND) {
      return x;
    }
  }
  /* nothing found :( */
  return 0;
}

int main(int argc, char *argv[]) {
  printf("10: %ld\n", search(10));
  printf("20: %ld\n", search(20));
  return 1;
}
