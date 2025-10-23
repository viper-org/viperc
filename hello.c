void puts(char* s);
void putchar(char c);

int main(int argc, char** argv)
{
    int x[5];
    x[argc] = 33;
    puts("Hello, world!");

    return x[argc];
}