// The von Neumann influence manifested in C
int sum(int n) {
    int result = 0;  // Mutable state
    for (int i = 1; i <= n; i++) {  // Sequential execution
        result += i;  // State mutation
    }
    return result;
}
