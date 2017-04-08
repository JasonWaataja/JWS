/*
 * Copyright (C) 2017 Jason Waataja

 * This file is part of JWS.

 * JWS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * JWS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with JWS.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef JWS_JWS_SETTER_H
#define JWS_JWS_SETTER_H

#include <glib.h>

typedef enum _JwsWallpaperMode JwsWallpaperMode;

/* Corresponds to feh's modes. */
enum _JwsWallpaperMode {
  JWS_WALLPAPER_MODE_FILL = 0,
  JWS_WALLPAPER_MODE_CENTER,
  JWS_WALLPAPER_MODE_MAX,
  JWS_WALLPAPER_MODE_SCALE,
  JWS_WALLPAPER_MODE_TILE
};

#define JWS_DEFAULT_WALLPAPER_MODE JWS_WALLPAPER_MODE_FILL

/* Free return value with g_free (). */
gchar *
jws_feh_string_for_mode(JwsWallpaperMode mode);


/*
 * Sets the current wallpaper to the file contained in path.  Returns 1 if
 * successful and 0 upon failure.
 */
int
jws_set_wallpaper_from_file(const char *path, JwsWallpaperMode mode);

#endif /* JWS_JWS_SETTER_H */
