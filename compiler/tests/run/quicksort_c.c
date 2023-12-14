#include <stdio.h>

struct list;
typedef struct list * list;

list _nil();

list _cons(int, list);

list _quick_sort(list);

char _print_intlist(list);

int main(int argc, char** argv) {
  list l = _cons(4, _cons(3, _cons(2, _cons(1, _nil()))));
  _print_intlist(_quick_sort(l));
}
