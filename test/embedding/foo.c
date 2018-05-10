#include <stdio.h>
#include <chicken.h>

extern C_word sudoku_solve(char * sudoku);
extern void sudoku_map_result(C_word res,
                              void  (*t1)(int, int, int, void *),
                              void * user_data);

void test(int row, int col, int val, void * user_data)
{
    printf("test %d %d %d\n", row, col, val);
}

int main()
{
  char buffer[256];
  int status;

  CHICKEN_run(C_toplevel);

  C_word res1 = sudoku_solve("000704005020010070000080002090006250600070008053200010400090000030060090200407000");

  /* Keep res1 */
  void * gcroot = CHICKEN_new_gc_root();
  CHICKEN_gc_root_set(gcroot, res1);

  sudoku_map_result(res1, test, NULL);

  for (int i=0; i < 10; i++) {
      C_word res = sudoku_solve("700600008800030000090000310006740005005806900400092100087000020000060009600008001");
      sudoku_map_result(res, test, NULL);
  }

  res1 = CHICKEN_gc_root_ref(gcroot);
  /* Error unless res1 stored before */
  sudoku_map_result(res1, test, NULL);

  return 0;
}
