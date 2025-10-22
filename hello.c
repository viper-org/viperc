void puts(char* s);

int main(int argc, char** argv)
{
    char** pArgv1 = argv + 2;
    puts(*pArgv1);
    return 0;
}