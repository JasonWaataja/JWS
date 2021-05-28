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

#include <stdbool.h>

#include <glib.h>

typedef enum _JwsWallpaperMode JwsWallpaperMode;

// Corresponds to feh's background modes.
enum _JwsWallpaperMode {
	JWS_WALLPAPER_MODE_FILL = 0,
	JWS_WALLPAPER_MODE_CENTER,
	JWS_WALLPAPER_MODE_MAX,
	JWS_WALLPAPER_MODE_SCALE,
	JWS_WALLPAPER_MODE_TILE
};

#define JWS_DEFAULT_WALLPAPER_MODE JWS_WALLPAPER_MODE_FILL

// Returns a new a string representing mode in command line argument form for
// feh, e.g. "--bg-fill". Free the return values with g_free.
char *
jws_feh_string_for_mode(JwsWallpaperMode mode);

// Sets the current wallpaper to the file contained in path with the given
// mode. In cases where the image doesn't occupy the entire screen,
// background_color is used to fill in the empty space, if it's not NULl. It
// should be a hex color starting with a "#" sign.  Returns true on success,
// false on failure.
bool
jws_set_wallpaper_from_file(const char *path, JwsWallpaperMode mode,
	const char *background_color);

#endif /* JWS_JWS_SETTER_H */
