void puts(char* s);

int main(int argc, char** argv)
{
    int i = 0;
    while (i != argc) {
        char** p = argv + i;
        puts(*p);
        i = i + 1;
    }
    return 0;
}