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

#ifndef JWS_JWSINFO_H
#define JWS_JWSINFO_H

#include <glib-object.h>

#include "jwssetter.h"
#include "jwstime.h"

#define JWS_TYPE_INFO (jws_info_get_type())
#define JWS_INFO(obj)                                                          \
	(G_TYPE_CHECK_INSTANCE_CAST((obj), JWS_TYPE_INFO, JwsInfo))

/*
 * A JwsInfo represents the information in a JWS configuration file, namely how
 * to display images, when to rotate them, and any other options.
 */
typedef struct _JwsInfo JwsInfo;
typedef struct _JwsInfoClass JwsInfoClass;

#define JWS_INFO_ERROR jws_info_error_quark()

GQuark
jws_info_error_quark(void);

enum JwsInfoError {
	JWS_INFO_ERROR_FILE,
	JWS_INFO_ERROR_FILE_FORMAT,
	JWS_INFO_ERROR_NO_FILES
};

/*
 * The strings representing each mode and how a user would reference them in a
 * configuration file.
 */
extern const char JWS_INFO_MODE_FILL[];
extern const char JWS_INFO_MODE_CENTER[];
extern const char JWS_INFO_MODE_MAX[];
extern const char JWS_INFO_MODE_SCALE[];
extern const char JWS_INFO_MODE_TILE[];

/*
 * Transforms mode_string representing a wallpaper mode into a mode value and
 * stores it in mode. Returns TRUE on success and FALSE on failure.  Stores
 * JWS_DEFAULT_WALLPAPER_MODE in mode on failure.
 */
gboolean
jws_wallpaper_mode_from_info_string(const gchar *mode_string,
	JwsWallpaperMode *mode);

GType
jws_info_get_type();

/* Returns a new JwsInfo. */
JwsInfo *
jws_info_new();

/*
 * Loads the info located in the file in path and returns a new JwsInfo with
 * that information. Returns if the new info if parsing the file succeeded or
 * NULL if not and sets err.
 */
JwsInfo *
jws_info_new_from_file(const gchar *path, GError **err);

/* Returns whether or not the image should be rotated for this info. */
gboolean
jws_info_get_rotate_image(JwsInfo *info);

/* Sets whether or not to rotate the image to rotate_image for this info. */
void
jws_info_set_rotate_image(JwsInfo *info, gboolean rotate_image);

/*
 * Returns a new time value representing how long between rotating images. Free
 * the result with jws_time_value_free(). */
JwsTimeValue *
jws_info_get_rotate_time(JwsInfo *info);

/*
 * Sets the time between rotating images for info to rotate_time. The
 * rotate_time is not stored internally.
 */
void
jws_info_set_rotate_time(JwsInfo *info, JwsTimeValue *rotate_time);

/* Returns whether or not to randomize the order of images for info. */
gboolean
jws_info_get_randomize_order(JwsInfo *info);

/*
 * Sets whether or not to randomize the order of images for inf to
 * randomize_order.
 */
void
jws_info_set_randomize_order(JwsInfo *info, gboolean randomize_order);

/*
 * Returns the background color to use if the wallpaper doesn't occupy the
 * whole screen, or NULL. Free the result with g_free.
 */
char *
jws_info_get_background_color(JwsInfo *info);

/**
 * Sets the background color to use if the wallpaper doesn't occupy the whole
 * screen. If passed NULL, uses feh's default, which is black.
 */
void
jws_info_set_background_color(JwsInfo *info, const char *background_color);

/*
 * Returns the list of files for info. This should be treated as an
 * unmodifiable snapshot and neither it nor its data should be modified for any
 * reason.
 */
GList *
jws_info_get_file_list(JwsInfo *info);

/* Sets the file list for info to a deep copy of file_list. */
void
jws_info_set_file_list(JwsInfo *info, GList *file_list);

/* Returns the wallpaper mode for info. */
JwsWallpaperMode
jws_info_get_mode(JwsInfo *info);

/* Sets the wallpaper mode for into to mode. */
void
jws_info_set_mode(JwsInfo *info, JwsWallpaperMode mode);

/* Adds a copy of path to the file list for info. */
void
jws_info_add_file(JwsInfo *info, const gchar *path);

/*
 * Removes a file path from the list of files for info. Returns whether or not
 * an item was removed.
 */
gboolean
jws_info_remove_file(JwsInfo *info, const gchar *path);

/*
 * Loads the information in path into file. Returns TRUE on success or FALSE on
 * failure and sets err.
 */
gboolean
jws_info_set_from_file(JwsInfo *info, const gchar *path, GError **err);

/* Prints info in a human readable format. */
void
print_jws_info(JwsInfo *info);

/*
 * Writes info to the path in file. Returns TRUE on success, FALSE on failure.
 */
gboolean
jws_info_write_to_file(JwsInfo *info, const gchar *path);

/*
 * Writes a NULL terminates string to the given channel not including the null
 * character and returns whether or not the operation was successful.
 */
gboolean
jws_write_line(GIOChannel *channel, const gchar *message);

/* Loads the default state for an info into info. */
void
jws_info_set_defaults(JwsInfo *info);

#endif /* JWS_JWSINFO_H */
