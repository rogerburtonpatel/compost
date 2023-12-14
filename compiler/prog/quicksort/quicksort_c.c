#include <stdio.h>

typedef struct list * list;

list _nil();

list _cons(int, list);

list _quick_sort(list);

char _print_intlist(list);

int main(int argc, char** argv) {
  list l = _nil();

  int i;
  while (scanf("%d", &i) == 1) {
    l = _cons(i, l);
  }

  _print_intlist(_quick_sort(l));
}
