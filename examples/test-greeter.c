#include "../out.h"
#include <stdio.h>

int main() {
    GREETER g = { .name = "" };

    // Try greeting without a name — should fail
    printf("Calling greet() without a name...\n");
    // greet(&g);  // should trigger precondition failure

    // Set a valid name
    printf("Calling set_name(\"Alice\")...\n");
    set_name(&g, "Alice");

    // Greet again — should succeed
    printf("Calling greet()...\n");
    greet(&g);  // prints: Hello, Alice

    return 0;
}
