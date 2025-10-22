void puts(char* s);
void putchar(char c);

int main(int argc, char** argv)
{
    for (char i = argc; i != (0-1); --i) {
        puts(*(argv + i));
    }
    return 0;
}