void puts(char* s);

int main(int argc, char** argv)
{
    int i = 0;
    while (i != argc) {
        puts(*(argv + i));
        i += 1;
    }
    return 0;
}