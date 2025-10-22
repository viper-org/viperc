int main() {
    int x = 41;
    int* z = &x;
    int** p = &z;
    *z = 67;
    return **p;
}