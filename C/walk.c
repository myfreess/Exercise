#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>


#define N 3

int Matrix[N*N]; //不回收

const int LineN = 2 * N - 1; //实际对角线数

typedef struct pos
    { int x;
      int y;
    } Position;

Position* mk_Pos(int x, int y){
    Position* pos = malloc(sizeof(Position));
    assert(x > 0 && y > 0);
    pos->x = x;
    pos->y = y;
    return pos;
}

bool isEven(int n){
    assert(n > 0);
    return (n % 2 == 0);
}

bool isOdd(int n){
    return !isEven(n);
}


void setV(int val, Position* pos){
    Matrix[((pos->x - 1) + ((pos->y - 1) * N))] = val;
   return;
}

void moveL(Position* pos){
   // TODO
   --pos->x;
   ++pos->y;
}

void moveR(Position* pos){
    ++pos->x;
    --pos->y;
}

void exchange(Position* pos){
    int x = pos->x;
    int y = pos->y;
    pos->x = y;
    pos->y = x;
}



void INC(Position* pos){
    assert(pos->x <=N && pos->y <=N);
    if(pos->x > pos->y){
    if(N == pos->x){
    ++pos->y;
    } else {
    ++pos->x;
    }}
    if(pos->x < pos->y){
    if(N == pos->y){
    ++pos->x;
    } else {
    ++pos->y;
    }}
    if(pos->x == pos->y && 1 == pos->x){
    ++pos->x;
}
    exchange(pos);
}


Position* calculatePos(int L){
    Position* pos = mk_Pos(1,1);
    for(int i = 1; i < L; ++i){
    INC(pos);
    }
    return pos;
}

void printPos(Position* pos){
    printf("(%d, %d)\n", pos->x, pos->y);
}


insertL(int L, int ini){
    int acc = ini;
    Position* pos = calculatePos(L);
    //计算每行元素数量
    int elemN;
    if(N < L){
    elemN = 2 * N - L;
    } else {
    elemN = L;
    }
    //偶数行调用moveR
   //奇数行调用moveL
    if(isEven(L)){
    for(int i = 0; i < elemN; ++i){
    setV(acc, pos);
    ++acc;
    moveR(pos);
    }} else {
    for(int i = 0; i < elemN; ++i){
    setV(acc, pos);
    ++acc;
    moveL(pos);
   }}
    free(pos);
    return acc;
}

int main() {
    int Acc = 1;
    for(int i = 1; i <= LineN; ++i){
    Acc = insertL(i, Acc);
    }
    for(int i = 0; i < N*N; ++i){
    printf("%d",Matrix[i]);
    if((i + 1) % N == 0){
    printf("\n");
    } else {
    printf(" ");
    }}

    return 0;
}
