

#include "jwssetter.h"

#include <stdlib.h>
#include <string.h>

int
jws_set_wallpaper_from_file (const char *path)
{
  char cmd_prefix[] = "feh --bg-fill \"";
  char cmd_suffix[] = "\"";

  int str_len = (strlen (cmd_prefix)
                 + strlen (path)
                 + strlen (cmd_suffix));

  char *cmd_str;
  cmd_str = (char *) malloc (sizeof (char) * (str_len + 1));
  strcpy (cmd_str, cmd_prefix);
  strcat (cmd_str, path);
  strcat (cmd_str, cmd_suffix);

  int status;
  status = system (cmd_str);

  free (cmd_str);

  if (status == 0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}
