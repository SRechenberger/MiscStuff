#include <stdio.h>
#include <stdlib.h>

#define N 0
#define E 1
#define S 2
#define W 3


int walk(int pos_x, int pos_y, int coming_from, char labyr[1000][1000], int* l);

int main(){
  char labyr[1000][1000];

  int t;
  scanf("%d", &t);
  for(int i = 0; i < t; i++){
    int C, R;
    scanf("%d", &C);
    scanf("%d", &R);
    for(int r = 0; r < R; r++){
      scanf("%s", labyr[r]);
      //fgets(&dump, 1, stdin);
    }
    /*
    for(int r = 0; r < R; r++){
      for(int c = 0; c < C; c++){
        printf("%c", labyr[r][c]);
      }
      printf("\n");
    }*/
    int y = 0, x = 0, run = 1;
    for(int y1 = 1; y1 < R-1 && run; y1++){
      for(int x1 = 1; x1 < C-1 && run; x1++){
        if(labyr[y1][x1] == '.'
            && (labyr[y1+1][x1] == '.'
            ||  labyr[y1-1][x1] == '.'
            ||  labyr[y1][x1+1] == '.'
            ||  labyr[y1][x1-1] == '.')){
            x = x1;
            y = y1;
            run = 0;
        }
      }
    }
    int l = 0;
    int m = walk(x,y,-1,labyr, &l);
    printf("Maximum rope length is %d.\n", m > l ? m : l);

  }


  return 0;
}

int walk(int pos_x, int pos_y, int coming_from, char labyr[1000][1000], int *l){
  int dirs[4] =
    { labyr[pos_y-1][pos_x] == '.' ,
      labyr[pos_y][pos_x+1] == '.' ,
      labyr[pos_y+1][pos_x] == '.' ,
      labyr[pos_y][pos_x-1] == '.' };
  int fs = 1;
  for(int i = 0; i < 4; i++){
    if(i != coming_from){
      fs = fs && !dirs[i];
    }
  }
  if(fs) {
    return 0;
  }
  /*for(int i = 0; i < 4; i++){
    printf("%d ", dirs[i]);
  }
  printf("\n");*/
  int ways[4] = {0, 0, 0, 0};

  if(dirs[N] && coming_from != N)
    ways[N] = walk(pos_x,pos_y-1, S, labyr, l);
  if(dirs[E] && coming_from != E)
    ways[E] = walk(pos_x+1,pos_y, W, labyr, l);
  if(dirs[S] && coming_from != S)
    ways[S] = walk(pos_x, pos_y+1, N, labyr, l);
  if(dirs[W] && coming_from != W)
    ways[W] = walk(pos_x-1, pos_y, E, labyr, l);


  int max1 = 0, max2 = 0, open = 0;
  for(int i = 0; i < 4; i++){
    open += dirs[i];
    if(ways[i] > max1)
      max1 = ways[i];
    else if(ways[i] > max2){
      max2 = ways[i];
    }
  }
  //printf("OPEN: %d\n", open);
  if(open > (coming_from < 0 ? 1 : 2) && *l < max1 + max2 + 2)
    *l = max1 + max2 + 2;
  return max1 + 1;
}
