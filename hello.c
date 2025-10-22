void puts(char* s);

int x = 5;

int main(int argc, char** argv)
{
    char y = x;
    short z = y * 2;
    puts("Hello, world!");
    x = 12;
    return z;
}