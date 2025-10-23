void puts(char* s);
void putchar(char c);
int printf(char* fmt, ...);

struct Test { int a; char* b; } getTest();

int main(int argc, char* argv[])
{
    struct Test q;
    q.a = 123;
    return q.a;
}