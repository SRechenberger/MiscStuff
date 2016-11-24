#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

typedef struct LITERAL{
  int node;
  int color;
} Literal;

int k /* Colors */,
    n /* #Vertices */,
    e /* #Edges */;

#define ALPHA_SIZE (sizeof(int)*n*k)
#define CLAUSES (n*(k*(k-1))/2 + k*e)

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

int satisfies(Literal F[CLAUSES][2], int alpha[n][k]){
  // printf("#%d\n", sizeof(F));
  // printf("#%d\n F[0][0][0] = %d\n", sizeof(F), F[0][0][0]);
  for(int i = 0; i < CLAUSES; i++){
  //  printf("  i=%d j=%d CLAUES=%d k=%d n=%d\n", i, j, CLAUSES, k, n);
    Literal
      p0 = F[i][0],
      p1 = F[i][1];
   // printf("  p0=%d p1=%d\n", p0, p1);
    if(!(!alpha[p0.node][p0.color] || !alpha[p1.node][p1.color])) return 0;
  }
  return 1;
}

int color_sat(Literal F[CLAUSES][2], int alpha[n][k]){

  for(int t = 0; t < 100*e;t++){
    printf(" Testing Satisfiability\n");
    if(satisfies(F,alpha)) return 1;
    printf(" Choosing Clause\n");
    /* Choose C from F randomly
     *  with C alpha = 1
     *
    */
    int clause;
    Literal* p;
    do{
      clause = rand() % CLAUSES;
      p = F[clause];
    //  printf(" color = %d edge = %d p0 = %d p1 = %d\n", color, clause, p[0], p[1]);
    }while(!alpha[p[0].node][p[0].color] || !alpha[p[1].node][p[1].color]);
    //printf(" Clause (!(%d,%d) | !(%d,%d)) chosen\n", p[0], color, p[1], color);
    int r = rand() % 2;
    int *l = &alpha[p[r].node][p[r].color];
    *l = !(*l);
  }
  return 0;
}

int main(){

  srand(time(NULL));

  scanf("%d %d %d", &k, &n, &e);
  Literal F[CLAUSES][2]; /* Formula */
  int alpha[n][k] /* Assignment */;
     
  for(int i = 0; i < n; i++){
    for(int j = 0; j < k; j++){
      alpha[i][j] = 1;
    }
  }

  /* Read edges of graph and generate formula according to
   * {v1, v2} in E => {{v1i, v2i} | 1 <= i <= k} is in F
   * */
  int clause = 0;
  printf("Generating Edge Clauses\n");
  for(int i = 0; i < e; i++){
    int v1, v2;
    scanf("%d %d", &v1, &v2);
    for(int j = 0; j < k; j++){
      F[clause][0] = (Literal) {v1-1, k};
      F[clause][1] = (Literal) {v2-1, k};
      clause++;
    }
  }

  printf("Generating Node Clauses\n");
  for(int i = 0; i < n; i++){
    for(int j = 1; j < k+1; j++){
      for(int l = j+1; l < k+1; l++){
        printf("i=%d j=%d l=%d clause=%d CLAUSES=%d\n", i, j, l, clause, CLAUSES);
        F[clause][0] = (Literal) {i, j};
        F[clause][1] = (Literal) {i, l};
        clause++;
      }
    }
  }
  printf("Asking Solver\n");
  int sat = color_sat(F,alpha);
  printf("F in SAT?: %d\n", sat);
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


