void puts(char* s);
void putchar(char c);
int printf(char* fmt, ...);

enum { XXX = 55 };

typedef struct { int a; char* b; } test_t;

int main(int argc, char* argv[])
{
    test_t q;
    test_t* p = &q;
    q.a = 123;
    p->a = XXX;
    p = (test_t*)0;
    printf("q.a =" " %d" "\n", q.a);
    return sizeof((char)q.a);
}