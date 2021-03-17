#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <stdint.h>

void swapI(int* arr, size_t i, size_t j){
    int tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
}

uint64_t rnd(void){
    static uint64_t seed = 0;
    seed = 0 == seed ? (unsigned)time(NULL) : (seed * 16807) % 2147483647;
/*
2 ** 64          === 18446744073709551616
16807*2147483647 === 36092757655129
*/
    return seed;
}

void Q(int* arr, size_t left, size_t right){
    if(right <= left){
       return;
    }
    size_t last = left;
    size_t pivot = rnd() % (right - left + 1) + left;
    swapI(arr,left,pivot);
    pivot = left;
    ++left;
    for(size_t i = left; i <= right; ++i){
    if(arr[pivot] <= arr[i]){
        continue;
    }
    if(arr[pivot] > arr[i]){
        ++last;
        swapI(arr,last,i);
    }
    }
    swapI(arr,last,pivot);
    Q(arr,pivot,last);
    Q(arr,last+1,right);
}

void testQ(void){
   size_t limit = 10;
   int* arr = malloc(sizeof(int) * limit);
   for(size_t i = 0; i < limit; ++i){
       scanf("%d", arr + i);
   }
   Q(arr, 0, limit - 1);
   for(size_t i = 0; i < limit; ++i){
      printf("%d ", arr[i]);
   }
   putchar('\n');
   free(arr);
}

int main(void){
    testQ();
    return 0;
}
