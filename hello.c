void puts(char* s);
void putchar(char c);

int main(int argc, char** argv)
{
    int x = 3;
    switch (x) {
        case 1: puts("x>0"); puts("x=1");
        case 3: puts("x=3");
        default: puts("123");
    }

    return 0;
}