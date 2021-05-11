#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

// 2021.5.10 night 8:40
// longest common substring length
typedef uint32_t u32;

#define MAX(X, Y) ((X) > (Y) ? X : Y)

u32 dp[1024][1024];

u32 lcsl(char* s1, char* s2){
    u32 res = 0;
    for(size_t i = 0; i < strlen(s1); ++i){
       for(size_t j = 0; j < strlen(s2); ++j){
          if(s1[i] == s2[j]){
               //update res
               // 联想到KMP
               dp[i+1][j+1] = 1 + dp[i][j];
               res = MAX(res, dp[i+1][j+1]);
          }
       }
    }
    return res;
}



