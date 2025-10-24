void puts(char* s);
void putchar(char c);
int printf(char* fmt, ...);

enum { XXX = 55 };

typedef struct { int a; char* b; } testt;

int main(int argc, char* argv[])
{
    testt q;
    testt* p = &q;
    q.a = 123;
    p->a = XXX;
    p = (testt*)0;
    printf("%d", q.a);
    return sizeof((char)q.a);
}