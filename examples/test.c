void main()
{
    int i;
    int *ptr;
    ptr = (int *)malloc(20 * sizeof(int));
    for (i = 0; i < 20; ++i)
    {
        ptr[i] = i + 1;
    }

    for (i = 0; i < 20; ++i)
    {
        printf("%d, ", ptr[i]);
    }
}
