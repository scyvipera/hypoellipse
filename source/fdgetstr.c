/* BEGIN fdgetstr.c     []  */

#include <stdio.h>

char *fdgetstr (fd, string, max_chars)
  int fd, max_chars;
  char string[];
{

  int i, n, read();
  char c;

  n = max_chars - 1;

  for (i=0; i<n; i++) {

    if (read(fd, &c, 1) > 0) {
      if (c != '\n') string[i] = c;
      else {
        string[i] = '\0';
        return (string);
        }
      } /* End if got a non-EOF character */

    else {
      if (i > 0) {
        string[i] = '\0';
        return (string);
        }
      else return (NULL);

      } /* End else we hit EOF */
    } /* End for i to n-1 characters */

  string[n] = '\0';
  return (string);

} /* End fdgetstr() */

