#include <stdio.h>
#include <string.h>

void f(int *a){
    int b = *a;
    b = 20;
}

int main(){
    /*
    int a = 0;
    f(&a);
    printf("%d\n", a);
    return 0;
    */
    char s[256];
    strcpy(s, "one two         \t\t\t\t\t \t \t \n   three");
    char* token = strtok(s, " ");
    while (token) {
        printf("token: %s\n", token);
        token = strtok(NULL, " ");
    }
}
