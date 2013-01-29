#include <stdio.h>
#include <stdlib.h>

#define UPPER_BOUND 232792560


/* prototypes */
void fill_searchspace (int *space, int length);

int cmp(const void *a, const void *b)
{
  int *va = (int*) a;
  int *vb = (int*) b;
  if (*va == *vb)
    return 0;
  else
    return -1 ? (*va < *vb) : 1;
}


/* Here we calculate all multiples up to @length. What we want is all
 * numbers which are not multiples.
 * e.g. given the search space from 1 - 10. We know that everything
 * which is dividable by 10 is also dividable by 2. Therefore, ignore 2,
 * keep 10. Same for 9: everything which is dividable by 9 is also
 * dividable by 3. Keep 9, throw away 3.
 */
void fill_searchspace (int *space,
                       int length)
{
  int i, j;

  for (i=0; i <= length; i++) {
    space[i] = i;
  }

  j = 2;
  while (j <= length) {
    for (i=length; i >= 0; i--) {
      if (space[i] % j == 0 && !(i == j))
        space[j] = 0;
        j++;
    }
  }

  /* sort it, otherwise we have a few items with 0.
   * qsort will do the trick */
  qsort (space, length, sizeof (int), cmp);
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

    /* Either we were able to div all integers, which means we've found
     * a solution, or we hit the UPPER_BOUND, which means our
     * implementaiton is bugged.
     */
    if (*space == 0 || x == UPPER_BOUND) {
      break;
    }

  }
  /* nothing found :( */
  space = start;
  free (space);
  return x;
}

int main(int argc, char *argv[]) {
  printf("10: %ld\n", search(10));
  printf("20: %ld\n", search(20));
  return 1;
}
