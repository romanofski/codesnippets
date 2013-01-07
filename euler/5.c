#include <stdio.h>

#define UPPER_BOUND 232792560


long search(long x) {
  int search_range[] = {1, 2, 3, 4, 5, 6, 7, 8, 10};
  int rest = 1;
  int *y = search_range;
  int i = 0;
  int count = sizeof (search_range);

  while (1) {
    for (i = 0; i < count; i++) {
      rest = x % *(y + i);
      if (rest != 0) {
        x += 10;
        break;
      }

      if ( *(y) == count - 1) {
        return x;
      }
    }

    if (x == UPPER_BOUND) {
      return x;
    }
  }
}

int main(int argc, char *argv[]) {
  printf("10: %ld", search(10));
  return 1;
}
