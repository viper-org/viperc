void puts(char* s);
void putchar(char c);

int main(int argc, char** argv)
{
    0[argv] = "Replaced";
    for (char i = argc; i >= 0; --i) {
        puts(argv[i]);
    }

    if (!(2 == 1) && 2 == 2) puts("yes");
    putchar(65);

    return 0;
}