void puts(char* s);

int rfact(int n)
{
    if (n == 0) return 1;
    return n * rfact(n - 1);
}

int main(int argc, char** argv)
{
    return rfact(argc);
}