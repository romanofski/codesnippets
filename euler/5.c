#include <stdio.h>
#include <stdlib.h>

#define UPPER_BOUND 232792560


/* prototypes */
void fill_searchspace (int *space, int length);

void fill_searchspace (int *space,
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
  int  *space = malloc(limit * sizeof(int));
  int  *start;

  fill_searchspace(space, limit);
  start = space;

  while (*space <= limit) {
    rest = x % *space;
    if (rest != 0) {
      x += limit;
      space = start;
      continue;
    }
    space++;

    if (*space == 0) {
      return x;
    }

    /* this is my emergency break */
    if (x == UPPER_BOUND) {
      return x;
    }
  }
  /* nothing found :( */
  free (space);
  return 0;
}

int main(int argc, char *argv[]) {
  printf("10: %ld\n", search(10));
  printf("20: %ld\n", search(20));
  return 1;
}
