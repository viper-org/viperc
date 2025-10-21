int ident(int x) {
    return x;
}

int add(int a, int b) { return a + b; }

int main(int argc) {
    int ret = ident(add(argc, 1));
    return ret;
}