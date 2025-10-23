void puts(char* s);
void putchar(char c);

enum Test : long {
    YYY,
    XXX
};

int main(int argc, char** argv)
{
    enum Test x[5];
    x[argc] = 22;
    puts("Hello, world!");

    return x[argc];
}