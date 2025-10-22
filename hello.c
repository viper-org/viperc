void puts(char* s);

int main(int argc, char** argv)
{
    for (int i = 0; i != argc; i += 1) {
        if (i == 2) break;
        puts(*(argv + i));
    }
    return 0;
}