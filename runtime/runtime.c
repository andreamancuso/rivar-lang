#include <stdio.h>
#include <stdlib.h>

void __contract_violation() {
    fprintf(stderr, "Contract violation\n");
    exit(1);
}
