#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

int k /* Colors */,
    n /* #Vertices */,
    e /* #Edges */;

#define ALPHA_SIZE (sizeof(int)*n*k)
#define CLAUSES ((k*(k-1))/2 + e)

int is_valid(int alpha[n][k]){
  for(int i = 0; i < n; i++){
    int ones = 0;
    for(int j = 0; j < n; j++){
      ones += alpha[i][j];
      if(ones > 1) return 0;
    }
  }
  return 1;
}

int satisfies(int F[CLAUSES][k][2], int alpha[n][k]){
  // printf("#%d\n", sizeof(F));
  // printf("#%d\n F[0][0][0] = %d\n", sizeof(F), F[0][0][0]);
  for(int i = 0; i < CLAUSES; i++){
    for(int j = 0; j < k; j++){
      printf("  i=%d j=%d CLAUES=%d k=%d\n", i, j, CLAUSES, k);
      int p0 = F[i][j][0],
          p1 = F[i][j][1];
      printf("  p0=%d p1=%d\n", p0, p1);
      if(alpha[p0][j] && alpha[p1][j]) return 0;
    }
  }
  return 1;
}

void color_sat(int F[CLAUSES][k][2], int alpha[n][k]){

  for(int t = 0; t < 10*e;t++){
    printf(" Testing Satisfiability\n");
    if(satisfies(F,alpha)) return;
    printf(" Choosing Clause\n");
    /* Choose C from F randomly
     *  with C alpha = 1
     *
    */
    int *p, color, edge;
    do{
      color = rand() % k;
      edge = rand() % CLAUSES;
      p = F[edge][color];
    }while(!alpha[p[0]][color] || !alpha[p[1]][color]);
    printf(" Clause (!(%d,%d) | !(%d,%d)) chosen\n", p[0], color, p[1], color);
    int *l = &alpha[p[rand() % 2]][color];
    *l = !(*l);
  }
}

int main(){

  srand(time(NULL));

  scanf("%d %d %d", &k, &n, &e);
  int F[CLAUSES][k][2] /* Formula */,
      alpha[n][k] /* Assignment */;
  // printf("#%d\n F[0][0][0] = %d\n", sizeof(F), F[0][0][0]);
  for(int i = 0; i < n; i++){
    for(int j = 0; j < k; j++){
      alpha[i][j] = 1;
    }
  }

  /* Read edges of graph and generate formula according to
   * {v1, v2} in E => {{v1i, v2i} | 1 <= i <= k} is in F
   * */
  printf("Generating Edge Clauses\n");
  for(int i = 0; i < e; i++){
    int v1, v2;
    scanf("%d %d", &v1, &v2);
    for(int j = 0; j < k; j++){
      F[i][j][0] = v1-1;
      F[i][j][1] = v2-1;
    }
  }

  printf("Generating Node Clauses\n");
  int clause = e;
  for(int i = 0; i < n; i++){
    for(int j = 0; j < k; j++){
      for(int l = j+1; l < k; l++){
        F[clause][j][0] = j;
        F[clause][j][1] = l;
        clause++;
      }
    }
  }
  printf("Asking Solver\n");
  color_sat(F,alpha);

  printf("Printing\n");
  for(int v = 0; v < n; v++){
    int color = 0;
    for(int i = 0; i < k; i++){
      if(alpha[v][i]){
        color = i+1;
        break;
      }
    }
    printf("%d %d\n", v+1, color);
  }
}


