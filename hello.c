void puts(char* s);
void putchar(char c);
int printf(char* fmt, ...);

int main(int argc, char** argv)
{
    void* q = argv;
    char** argv1 = q;
    puts(*argv1);
    q = nullptr;
    return 0;
}