/* extern void print_msg1(); */
//extern void quit();

int main()
{
    /* print_msg1(); */
    //quit();

    /* Write a '=' on the screen */
    char *screen;
    screen = (char *)0xB8000;
    *screen = 'Z';

    /* Hang! */
    while (1);

    return 0;
}
