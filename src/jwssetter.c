/* jwssetter.c - set wallpaper

Copyright (C) 2016 Jason Waataja

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

gchar *
jws_feh_string_for_mode (JwsWallpaperMode mode)
{
  gchar *mode_str = NULL;

  switch (mode)
	{
	case JWS_WALLPAPER_MODE_FILL:
	  mode_str = g_strdup ("--bg-fill");
	  break;
	case JWS_WALLPAPER_MODE_CENTER:
	  mode_str = g_strdup ("--bg-center");
	  break;
	case JWS_WALLPAPER_MODE_MAX:
	  mode_str = g_strdup ("--bg-max");
	  break;
	case JWS_WALLPAPER_MODE_SCALE:
	  mode_str = g_strdup ("--bg-scale");
	  break;
	case JWS_WALLPAPER_MODE_TILE:
	  mode_str = g_strdup ("--bg-tile");
	  break;
	default:
	  /* This could go into an infinite loop but I sure hope not.  */
	  mode_str = jws_feh_string_for_mode (JWS_DEFAULT_WALLPAPER_MODE);
	  break;
	}

  return mode_str;
}

int
jws_set_wallpaper_from_file (const char *path, JwsWallpaperMode mode)
{
  gchar *mode_str = jws_feh_string_for_mode (mode);

  g_assert (mode_str);

  gchar *set_cmd = g_strconcat ("feh ", mode_str, " \"", path, "\"", NULL);

  int status;
  status = system (set_cmd);

  g_free (mode_str);
  g_free (set_cmd);

  return (status == 0);
}
