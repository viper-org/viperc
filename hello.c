void puts(char* s);
void putchar(char c);

int main(int argc, char** argv)
{
    char p = 'H';
    putchar(p);
    char* msg = "ello, world!";
    while(0 != *msg) {
        putchar(*msg);
        msg += 1;
    }
    return 0;
}