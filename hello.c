void puts(char* s);
void putchar(char c);
int printf(char* fmt, ...);

enum Test : long {
    YYY,
    XXX
};

int main(int argc, char** argv)
{
    enum Test x[5];
    x[argc] = 22;
    puts("Hello, world!");
    printf("x: %d, argc: %d", x[argc], argc);

    return x[argc];
}