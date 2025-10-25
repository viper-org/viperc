void puts(char* s);
void putchar(char c);
int printf(char* fmt, ...);

enum { XXX = 55 };

typedef struct { int a; char* b; } test_t;

int main(int argc, char* argv[])
{
    int a = 16;
    a %= 3;
    int b = 3;
    printf("%x", a);
    return 0;
}