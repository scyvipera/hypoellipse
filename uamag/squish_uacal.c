#include <stdio.h>
#include <string.h>

void squish_uacal__ (old_string, new_string, old_length, new_length)
  char old_string[], new_string[];
  int old_length, new_length;
{
  int i, j, k, max_i, is_float;
  char decimal_string[20];

  i = j = 0;
  max_i = strlen(old_string);

#if DEBUG
  printf ("old: %s\n", old_string);
  fflush (stdout);
#endif

  do {

    if (old_string[i] != ' ') new_string[j++] = old_string[i++];
    else if (old_string[++i] != ' ') new_string[j++] = ' ';

    } while (i < max_i);

  new_string[j] = '\0';
#if DEBUG
  printf ("\ntmp0: %s\n", new_string);
  fflush (stdout);
#endif

  (void) strcpy (old_string, new_string);
  i = j = k = is_float = 0;
  max_i = strlen(old_string);

  do {

    if (old_string[i] == ' ') {
      if (is_float) {
        while (k > 0) {
          if (decimal_string[--k] != '0') break;
          else decimal_string[k] = '\0';
          }
        new_string[j] = '\0';
        if (decimal_string[0] != '\0') {
          (void) strcat (new_string, decimal_string);
          j += k+1;
          }
        }
      is_float = 0;
      new_string[j++] = old_string[i++];
      }
    else if (old_string[i] == '.') {
      new_string[j++] = old_string[i++];
      is_float = 1;
      k = 0;
      decimal_string[0] = '\0';
      }
    else if (is_float) {
      decimal_string[k++] = old_string[i++];
      decimal_string[k] = '\0';
      }
    else {
      new_string[j++] = old_string[i++];
      }

    } while (i < max_i);

  new_string[j] = '\0';
  if (is_float) {
    while (k > 0) {
      if (decimal_string[--k] != '0') break;
      else decimal_string[k] = '\0';
      }
    if (decimal_string[0] != '\0') {
      (void) strcat (new_string, decimal_string);
      j += k+1;
      }
    }

  for (; j<new_length; j++) new_string[j] = '\0'; 

#if DEBUG
  printf ("new: %s\n", new_string);
  fflush (stdout);
#endif

} /* End squish_uacal() */
