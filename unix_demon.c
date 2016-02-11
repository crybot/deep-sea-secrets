#include <time.h>
#include <limits.h>
#include <stdio.h>

/* Take this out and a Unix Demon will dog your steps from 
 * now until the time_t's wrap around. */

/* When does 'time_t' wrap around? */

int main(void)
{
  // on most systems time_t is just a typedef of long 
  time_t max = LONG_MAX;
  printf("ctime(max) = %s \n", ctime(&max));
  return 0;
}
