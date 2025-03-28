#include "../out.h"
#include <stdio.h>

int main() {
    ACCOUNT acc = { .balance = 100 };

    deposit(&acc, 50);  // should succeed
    printf("New balance: %d\n", acc.balance);

    deposit(&acc, -10); // should fail
    return 0;
}
