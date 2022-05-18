/* matmul2.c  100*100 matrix element by element multiply */
#include <stdio.h>

int main()
{
  const int N = 100;
  double a[N][N]; /* input matrix */
  double b[N][N]; /* input matrix */
  double c[N][N]; /* result matrix */
  int i,j;

  printf("starting multiply\n");
  a[1][1] = 3.5;
  b[1][1] = 1.2; /* not a valid benchmark, most elements zero */

  for(i=0; i<N; i++){
    for(j=0; j<N; j++){
front:
        c[i][j] = a[i][j]*b[i][j]; /* most time spent here! */
back:;
    }
  }
  printf("a result %g \n", c[1][1]); /* prevent dead code elimination */
  return 0;
}

