void puts(char* s);
void putchar(char c);
int printf(char* fmt, ...);

enum { XXX = 55 };

typedef struct { int a; char* b; } test_t;

int main(int argc, char* argv[])
{
    int a[2][2];
    a[0][0] = 1;
    a[1][1] = 5;
    a[1][1] *= 5;

    printf("%d", a[1][1]);
    return 0;
}