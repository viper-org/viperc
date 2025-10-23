void puts(char* s);
void putchar(char c);
int printf(char* fmt, ...);

struct Test { int a; char* b; } getTest();

int main(int argc, char* argv[])
{
    struct Test q;
    struct Test* p = &q;
    q.a = 123;
    p->a = 55;
    printf("%d", q.a);
    return 0;
}