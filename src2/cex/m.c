#include <stdio.h>
#include <unistd.h>

/* void char_pipe_reader(char * buffer, FILE *ptr){ */

void char_reader(char * buffer, FILE *ptr){
  buffer[0] = getc(ptr);
}

int  char_writer(char * buffer, FILE *ptr){
  return putc(buffer[0], ptr);
}

int main()
{
   FILE *ptr;
   ptr = fopen("t.txt","w");
   putc('\0',ptr);
   putc('h',ptr);
   putc('i',ptr);
   putc('\0',ptr);
   fclose(ptr);
   ptr = fopen("t.txt","r");
   printf("%u\n", getc(ptr) );
   printf("%u\n", getc(ptr) );
   printf("%u\n", getc(ptr) );
   printf("%u\n", getc(ptr) );
   fclose(ptr);
   printf("\n");
   return(0);
}
