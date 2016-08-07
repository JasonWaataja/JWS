/* jwssetter.c - set wallpaper

This file is part of JWS.

JWS is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

JWS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with JWS.  If not, see <http://www.gnu.org/licenses/>.  */

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
