void puts(char* s);

int main(int argc, char** argv)
{
    if (argc == 1) {
        puts("argc: 1");
    }
    else {
        if (argc == 2)
            puts("argc: 2");
        else
            puts("argc > 2");
    }
    return 0;
}