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

  C_word res = sudoku_solve("000704005020010070000080002090006250600070008053200010400090000030060090200407000");
  sudoku_map_result(res, test, NULL);

  return 0;
}
