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

#include "jwsinfo.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>
#include <glib/gi18n.h>

const char JWS_INFO_MODE_FILL[] = "fill";
const char JWS_INFO_MODE_CENTER[] = "center";
const char JWS_INFO_MODE_MAX[] = "max";
const char JWS_INFO_MODE_SCALE[] = "scale";
const char JWS_INFO_MODE_TILE[] = "tile";

struct _JwsInfo {
	GObject parent;
};

struct _JwsInfoClass {
	GObjectClass parent_class;
};

typedef struct _JwsInfoPrivate JwsInfoPrivate;

struct _JwsInfoPrivate {
	gboolean rotate_image;
	JwsTimeValue *rotate_time;
	gboolean randomize_order;
	JwsWallpaperMode mode;

	GList *file_list;
};

G_DEFINE_TYPE_WITH_PRIVATE(JwsInfo, jws_info, G_TYPE_OBJECT);

static void
jws_info_dispose(GObject *obj)
{
	JwsInfoPrivate *priv = jws_info_get_instance_private(JWS_INFO(obj));
	G_OBJECT_CLASS(jws_info_parent_class)->dispose(obj);
}

static void
jws_info_finalize(GObject *obj)
{
	JwsInfoPrivate *priv = jws_info_get_instance_private(JWS_INFO(obj));
	for (GList *iter = priv->file_list; iter != NULL;
		iter = g_list_next(iter)) {
		g_free(iter->data);
		iter->data = NULL;
	}
	g_list_free(priv->file_list);
	priv->file_list = NULL;
	jws_time_value_free(priv->rotate_time);
	G_OBJECT_CLASS(jws_info_parent_class)->finalize(obj);
}

static void
jws_info_init(JwsInfo *self)
{
	JwsInfoPrivate *priv = jws_info_get_instance_private(self);
	priv->rotate_image = TRUE;
	priv->rotate_time = jws_time_value_new_for_values(0, 1, 0);
	priv->randomize_order = TRUE;
	priv->mode = JWS_DEFAULT_WALLPAPER_MODE;
	priv->file_list = NULL;
}

static void
jws_info_class_init(JwsInfoClass *kclass)
{
	GObjectClass *as_object_class = G_OBJECT_CLASS(kclass);
	as_object_class->dispose = jws_info_dispose;
	as_object_class->finalize = jws_info_finalize;
}

JwsInfo *
jws_info_new()
{
	return g_object_new(JWS_TYPE_INFO, NULL);
}

JwsInfo *
jws_info_new_from_file(const gchar *path, GError **err)
{
	JwsInfo *obj = jws_info_new();
	GError *tmp_err = NULL;
	gboolean status = jws_info_set_from_file(obj, path, &tmp_err);
	if (!status) {
		g_propagate_error(err, tmp_err);
		return NULL;
	}
	return obj;
}

gboolean
jws_info_get_rotate_image(JwsInfo *info)
{
	g_assert(info);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	return priv->rotate_image;
}

void
jws_info_set_rotate_image(JwsInfo *info, gboolean rotate_image)
{
	g_assert(info);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	priv->rotate_image = rotate_image;
}

JwsTimeValue *
jws_info_get_rotate_time(JwsInfo *info)
{
	g_assert(info);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	return jws_time_value_copy(priv->rotate_time);
}

void
jws_info_set_rotate_time(JwsInfo *info, JwsTimeValue *time)
{
	g_assert(info);
	g_assert(time);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	jws_time_value_free(priv->rotate_time);
	priv->rotate_time = jws_time_value_copy(time);
}

gboolean
jws_info_get_randomize_order(JwsInfo *info)
{
	g_assert(info);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	return priv->randomize_order;
}

void
jws_info_set_randomize_order(JwsInfo *info, gboolean randomize_order)
{
	g_assert(info);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	priv->randomize_order = randomize_order;
}


GList *
jws_info_get_file_list(JwsInfo *info)
{
	g_assert(info);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	return priv->file_list;
}

void
jws_info_set_file_list(JwsInfo *info, GList *file_list)
{
	g_assert(info);
	g_assert(file_list);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	g_list_free_full(priv->file_list, g_free);
	priv->file_list = g_list_copy(file_list);
}

void
jws_info_add_file(JwsInfo *info, const gchar *path)
{
	g_assert(info);
	g_assert(path);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	priv->file_list = g_list_append(priv->file_list, g_strdup(path));
}

gboolean
jws_info_remove_file(JwsInfo *info, const gchar *path)
{
	g_assert(info);
	g_assert(path);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	gboolean found_item = FALSE;
	GList *current = priv->file_list;
	while (current) {
		gchar *current_path = current->data;
		if (g_str_equal(current_path, path)) {
			priv->file_list = g_list_delete_link(priv->file_list,
				current);
			return TRUE;
		}
	}
	return FALSE;
}

/*
 * Sets the time value for info to the information contained in line. Returns
 * TRUE on success, false on failure and sets err
 */
static gboolean
jws_info_set_time_from_line(JwsInfo *info, const gchar *line, GError **err)
{
	g_assert(info);
	g_assert(line);
	GRegex *regex;
	regex = g_regex_new("^time\\s+(\\S.*)$", 0, 0, NULL);
	g_assert(regex);

	GMatchInfo *match_info = NULL;
	gboolean found_match = g_regex_match(regex, line, 0, &match_info);

	if (!found_match) {
		g_set_error(err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE_FORMAT,
			_("No argument was found to time in line: \"%s\"."),
			line);
		g_regex_unref(regex);
		g_match_info_free(match_info);
		return FALSE;
	}
	gchar *value = g_match_info_fetch(match_info, 1);
	if (!value || strlen(value) == 0) {
		g_set_error(err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE_FORMAT,
			_("Failed to find valid argument to time."));
		g_regex_unref(regex);
		g_match_info_free(match_info);
		g_free(value);
		return FALSE;
	}
	JwsTimeValue *rotate_time = jws_time_value_new_from_string(value);
	if (!rotate_time) {
		g_set_error(err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE_FORMAT,
			_("Failed to parse time sting: \"%s\"."), value);
		g_regex_unref(regex);
		g_match_info_free(match_info);
		g_free(value);
		return FALSE;
	}
	if (jws_time_value_total_seconds(rotate_time) <= 0) {
		g_set_error(err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE_FORMAT,
			_("Time must be greater than 0."));
		g_regex_unref(regex);
		g_match_info_free(match_info);
		g_free(value);
		return FALSE;
	}
	jws_info_set_rotate_time(info, rotate_time);
	jws_time_value_free(rotate_time);
	g_free(value);
	g_regex_unref(regex);
	g_match_info_free(match_info);
	return TRUE;
}

/*
 * Sets the wallpaper mode for info to the value stored in line. Returns TRUE
 * on success, FALSE on failure and sets err.
 */
static gboolean
jws_info_set_mode_from_line(JwsInfo *info, const gchar *line, GError **err)
{
	g_assert(info);
	g_assert(line);
	/*
	 * Match a line starting with mode, then an argument; capture the
	 * argument.
	 */
	GRegex *reg = g_regex_new("^mode\\s+(\\S+)$", 0, 0, NULL);
	g_assert(reg);
	GMatchInfo *match_info = NULL;
	gboolean found_match = g_regex_match(reg, line, 0, &match_info);
	if (!found_match) {
		g_set_error(err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE_FORMAT,
			_("Invalid mode format"));
		g_regex_unref(reg);
		g_match_info_free(match_info);
		return FALSE;
	}
	gchar *value = g_match_info_fetch(match_info, 1);
	if (!value || strlen(value) == 0) {
		g_set_error(err, JWS_INFO_ERROR,
			JWS_INFO_ERROR_FILE_FORMAT,
			_("Couldn't find argument for mode."));
		g_regex_unref(reg);
		g_match_info_free(match_info);
		g_free(value);
		return FALSE;
	}
	JwsWallpaperMode mode;
	gboolean is_mode = jws_wallpaper_mode_from_info_string(value, &mode);
	if (!is_mode) {
		g_set_error(err, JWS_INFO_ERROR,
			JWS_INFO_ERROR_FILE_FORMAT,
			_("Unrecognized mode \"%s\"."), value);
		g_regex_unref(reg);
		g_match_info_free(match_info);
		g_free(value);
		return FALSE;
	}
	jws_info_set_mode(info, mode);
	g_regex_unref(reg);
	g_match_info_free(match_info);
	g_free(value);
	return TRUE;
}

/*
 * Processes a single line from a configuration file in line and updates info
 * accordingly. If a files line is encountered, stores TRUE in  has_files .
 * Returns TRUE on success, FALSE on failure and sets err.
 */
static gboolean
jws_info_process_line(JwsInfo *info, const gchar *line, gboolean *has_files,
	GError **err)
{
	g_assert(info);
	g_assert(line);
	g_assert(has_files);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	if (g_str_has_prefix(line, "files"))
		*has_files = TRUE;
	else if (g_str_has_prefix(line, "rotate-image"))
		priv->rotate_image = TRUE;
	else if (g_str_has_prefix(line, "single-image"))
		priv->rotate_image = FALSE;
	else if (g_str_has_prefix(line, "time")) {
		GError *tmp_err = NULL;
		if (!jws_info_set_time_from_line(info, line, &tmp_err)) {
			g_propagate_error(err, tmp_err);
			return FALSE;
		}
	} else if (g_str_has_prefix(line, "randomize-order"))
		priv->randomize_order = TRUE;
	else if (g_str_has_prefix(line, "in-order"))
		priv->randomize_order = FALSE;
	else if (g_str_has_prefix(line, "mode")) {
		GError *tmp_err = NULL;
		if (!jws_info_set_mode_from_line(info, line, &tmp_err)) {
			g_propagate_error(err, tmp_err);
			return FALSE;
		}
	}
	return TRUE;
}

gboolean
jws_info_set_from_file(JwsInfo *info, const gchar *path, GError **err)
{
	g_assert(info);
	g_assert(path);
	jws_info_set_defaults(info);
	GIOChannel *channel = g_io_channel_new_file(path, "r", NULL);
	if (!channel) {
		g_set_error(err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE,
				_("Failed to open file: \"%s\"."), path);
		return FALSE;
	}
	GList *line_list = NULL;
	gchar *line = NULL;
	gsize line_length;
	gsize terminator_pos;
	GIOStatus status = g_io_channel_read_line(channel, &line,
		&line_length, &terminator_pos, NULL);
	while (status == G_IO_STATUS_NORMAL) {
		if (line && terminator_pos)
			line[terminator_pos] = '\0';
		if (!g_str_has_prefix(line, "#"))
			line_list = g_list_append(line_list, g_strdup(line));
		g_free(line);
		line = NULL;
		status = g_io_channel_read_line(channel, &line, &line_length,
			&terminator_pos, NULL);
	}
	g_free(line);
	line = NULL;

	g_io_channel_shutdown(channel, FALSE, NULL);
	g_io_channel_unref(channel);

	GList *iter;
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	gboolean has_files = FALSE;
	for (iter = line_list; iter && !has_files; iter = iter->next) {
		line = iter->data;
		GError *tmp_err = NULL;
		if (!jws_info_process_line(info, line, &has_files, &tmp_err)) {
			g_propagate_error(err, tmp_err);
			g_list_free_full(line_list, g_free);
			return FALSE;
		}
	}
	if (!has_files) {
		g_set_error(err, JWS_INFO_ERROR, JWS_INFO_ERROR_NO_FILES,
			_("No files found."));
		g_list_free_full(line_list, g_free);
		return FALSE;
	}
	gboolean found_file = FALSE;
	for (; iter != NULL; iter = iter->next) {
		line = iter->data;
		if (strlen(line) > 0) {
			jws_info_add_file(info, line);
			found_file = TRUE;
		}
	}
	if (!found_file) {
		g_set_error(err, JWS_INFO_ERROR, JWS_INFO_ERROR_NO_FILES,
			_("No files found."));
		g_list_free_full(line_list, g_free);
		return FALSE;
	}
	g_list_free_full(line_list, g_free);
	return TRUE;
}

void
print_jws_info(JwsInfo *info)
{
	g_assert(info);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	if (priv->rotate_image) {
		g_print(_("Rotate image\n"));
		g_print(_("Seconds between rotation: %i\n"),
			jws_time_value_total_seconds(priv->rotate_time));
		if (priv->randomize_order)
			g_print(_("Randomize order\n"));
		else
			g_print(_("In order"));

	} else
		g_print(_("Single image\n"));
	GList *file_list = priv->file_list;
	if (file_list) {
		g_print(_("Files:\n"));
		for (; file_list != NULL; file_list = g_list_next(file_list)) {
			g_print("%s\n", (char *) file_list->data);
		}
	} else
		g_print(_("No files to print\n"));
}

GQuark
jws_info_error_quark(void)
{
	return g_quark_from_static_string("jws-info-error-quark");
}

/*
 * Writes just the rotate image section of info to writer. If it just prints a
 * single image, then it writes that, otherwise it writes several other
 * settings. Returns TRUE on success, FALSE on failure. This function does not
 * close or unref the io channel on failure.
 */
static gboolean
jws_info_write_rotate_image(JwsInfo *info, GIOChannel *writer)
{
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	if (!priv->rotate_image)
		return jws_write_line(writer, "single-image");
	gboolean status = jws_write_line(writer, "rotate-image");
	if (!status)
		return FALSE;
	if (priv->randomize_order) {
		status = jws_write_line(writer, "randomize-order");
		if (!status)
			return FALSE;
	} else {
		status = jws_write_line(writer, "in-order");
		if (!status)
			return FALSE;
	}
	JwsTimeValue *simplest_form = jws_time_value_copy(priv->rotate_time);
	jws_time_value_to_simplest_form(simplest_form);
	gchar *time_str = g_strdup_printf("time %ih%im%is",
		simplest_form->hours, simplest_form->minutes,
		simplest_form->seconds);
	jws_time_value_free(simplest_form);
	status = jws_write_line(writer, time_str);
	g_free(time_str);
	return status;
}

gboolean
jws_info_write_to_file(JwsInfo *info, const gchar *path)
{
	g_assert(info);
	g_assert(path);
	JwsInfoPrivate *priv = jws_info_get_instance_private(info);
	GIOChannel *writer = g_io_channel_new_file(path, "w", NULL);
	if (!writer)
		return FALSE;
	gboolean status = jws_info_write_rotate_image(info, writer);
	if (!status) {
		g_io_channel_shutdown(writer, TRUE, NULL);
		g_io_channel_unref(writer);
		return FALSE;
	}
	status = jws_write_line(writer, "");
	if (!status) {
		g_io_channel_shutdown(writer, TRUE, NULL);
		g_io_channel_unref(writer);
		return FALSE;
	}
	status = jws_write_line(writer, "files");
	if (!status) {
		g_io_channel_shutdown(writer, TRUE, NULL);
		g_io_channel_unref(writer);
		return FALSE;
	}
	for (GList *iter = priv->file_list; iter; iter = iter->next) {
		status = jws_write_line(writer, iter->data);
		if (!status) {
			g_io_channel_shutdown(writer, TRUE, NULL);
			g_io_channel_unref(writer);
			return FALSE;
		}
	}
	g_io_channel_shutdown(writer, TRUE, NULL);
	g_io_channel_unref(writer);
	return TRUE;
}

gboolean
jws_write_line(GIOChannel *channel, const gchar *message)
{
	gchar *new_message = g_strconcat(message, "\n", NULL);
	gsize size;
	GIOStatus status = g_io_channel_write_chars(channel, new_message, -1,
		&size, NULL);
	g_free(new_message);
	return status == G_IO_STATUS_NORMAL;
}

void
jws_info_set_defaults(JwsInfo *info)
{
	g_assert(info);
	JwsInfoPrivate *priv = jws_info_get_instance_private (info);
	priv->rotate_image = TRUE;
	priv->randomize_order = FALSE;
	jws_time_value_free(priv->rotate_time);
	priv->rotate_time = jws_time_value_new_for_values(0, 1, 0);
	g_list_free_full(priv->file_list, g_free);
	priv->file_list = NULL;
	priv->mode = JWS_DEFAULT_WALLPAPER_MODE;
}

JwsWallpaperMode
jws_info_get_mode(JwsInfo *info)
{
	g_assert(info);
	JwsInfoPrivate *priv;
	priv = jws_info_get_instance_private(info);
	return priv->mode;
}

void
jws_info_set_mode(JwsInfo *info, JwsWallpaperMode mode)
{
	g_assert(info);
	JwsInfoPrivate *priv;
	priv = jws_info_get_instance_private(info);
	priv->mode = mode;
}

gboolean
jws_wallpaper_mode_from_info_string(const gchar *mode_string,
	JwsWallpaperMode *mode)
{
	g_assert(mode);
	g_assert(mode);
	if (g_str_equal(mode_string, JWS_INFO_MODE_FILL)) {
		*mode = JWS_WALLPAPER_MODE_FILL;
		return TRUE;
	}
	if (g_str_equal(mode_string, JWS_INFO_MODE_CENTER)) {
		*mode = JWS_WALLPAPER_MODE_CENTER;
		return TRUE;
	}
	if (g_str_equal(mode_string, JWS_INFO_MODE_MAX)) {
		*mode = JWS_WALLPAPER_MODE_MAX;
		return TRUE;
	}
	if (g_str_equal(mode_string, JWS_INFO_MODE_SCALE)) {
		*mode = JWS_WALLPAPER_MODE_SCALE;
		return TRUE;
	}
	if (g_str_equal(mode_string, JWS_INFO_MODE_TILE)) {
		*mode = JWS_WALLPAPER_MODE_TILE;
		return TRUE;
	}
	*mode = JWS_DEFAULT_WALLPAPER_MODE;
	return FALSE;
}
