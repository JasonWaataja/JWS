/* jwsinfo.h - header for JwsInfo class

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

#include "jwsinfo.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char JWS_INFO_MODE_FILL[] = "fill";
const char JWS_INFO_MODE_CENTER[] = "center";
const char JWS_INFO_MODE_MAX[] = "max";
const char JWS_INFO_MODE_SCALE[] = "scale";
const char JWS_INFO_MODE_TILE[] = "tile";

struct _JwsInfo
{
  GObject parent;
};

struct _JwsInfoClass
{
  GObjectClass parent_class;
};

typedef struct _JwsInfoPrivate JwsInfoPrivate;

struct _JwsInfoPrivate
{
  gboolean rotate_image;
  JwsTimeValue *rotate_time;
  gboolean randomize_order;
  JwsWallpaperMode mode;

  GList *file_list;
};

G_DEFINE_TYPE_WITH_PRIVATE (JwsInfo, jws_info, G_TYPE_OBJECT);

static void
jws_info_dispose (GObject *obj)
{
  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (JWS_INFO (obj));

  G_OBJECT_CLASS (jws_info_parent_class)->dispose (obj);
}

static void
jws_info_finalize (GObject *obj)
{
  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (JWS_INFO (obj));

  for (GList *iter = priv->file_list; iter != NULL; iter = g_list_next (iter))
    {
      g_free (iter->data);
      iter->data = NULL;
    }
  g_list_free (priv->file_list);
  priv->file_list = NULL;

  jws_time_value_free (priv->rotate_time);

  G_OBJECT_CLASS (jws_info_parent_class)->finalize (obj);
}

static void
jws_info_init (JwsInfo *self)
{
  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (self);

  priv->rotate_image = TRUE;
  priv->rotate_time = jws_time_value_new_for_values (0, 1, 0);
  priv->randomize_order = TRUE;
  priv->mode = JWS_DEFAULT_WALLPAPER_MODE;

  priv->file_list = NULL;
}

static void
jws_info_class_init (JwsInfoClass *kclass)
{
  GObjectClass *as_object_class;
  as_object_class = G_OBJECT_CLASS (kclass);

  as_object_class->dispose = jws_info_dispose;
  as_object_class->finalize = jws_info_finalize;
}


JwsInfo *
jws_info_new ()
{
  return g_object_new (JWS_TYPE_INFO, NULL);
}

JwsInfo *
jws_info_new_from_file (const gchar *path, GError **err)
{
  JwsInfo *obj;
  obj = jws_info_new ();
  GError *tmp_err = NULL;
  gboolean status;
  status = jws_info_set_from_file (obj, path, &tmp_err);
  if (!status)
    {
      g_propagate_error (err, tmp_err);
      return NULL;
    }

  return obj;
}

gboolean
jws_info_get_rotate_image (JwsInfo *info)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);
      return priv->rotate_image;
    }
  else
    {
      return FALSE;
    }
}

void
jws_info_set_rotate_image (JwsInfo *info, gboolean rotate_image)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);
      priv->rotate_image = rotate_image;
    }
}

JwsTimeValue *
jws_info_get_rotate_time (JwsInfo *info)
{
  if (!info)
    return NULL;

  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (info);
  
  return jws_time_value_copy (priv->rotate_time);
}

void
jws_info_set_rotate_time (JwsInfo *info, JwsTimeValue *time)
{
  if (!info || !time)
    return;

  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (info);
  
  jws_time_value_free (priv->rotate_time);
  priv->rotate_time = jws_time_value_copy (time);
}

gboolean
jws_info_get_randomize_order (JwsInfo *info)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);
      return priv->randomize_order;
    }
  else
    {
      return FALSE;
    }
}

void
jws_info_set_randomize_order (JwsInfo *info, gboolean randomize_order)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);
      priv->randomize_order = randomize_order;
    }
}


GList *
jws_info_get_file_list (JwsInfo *info)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);
      return priv->file_list;
    }
  else
    {
      return NULL;
    }
}

void
jws_info_set_file_list (JwsInfo *info, GList *file_list)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);

      for (GList *iter = priv->file_list; iter != NULL; iter = g_list_next (iter))
        {
          g_free (iter->data);
          iter->data = NULL;
        }
      g_list_free (priv->file_list);

      priv->file_list = file_list;
    }
}

void
jws_info_add_file (JwsInfo *info, const gchar *path)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);

      priv->file_list = g_list_append (priv->file_list, g_strdup (path));
    }
}

void
jws_info_remove_file (JwsInfo *info, const gchar *path)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);

      gboolean found_item;
      for (GList *iter = priv->file_list; iter != NULL && !found_item;
           iter = g_list_next (iter))
        {
          gchar *iter_path;
          iter_path = iter->data;
          if (g_str_equal (iter_path, path))
            {
              priv->file_list = g_list_remove (priv->file_list, iter_path);
              found_item = TRUE;
            }
        }
    }
}

gboolean
jws_info_set_from_file (JwsInfo *info, const gchar *path, GError **err)
{
  g_assert (info);

  jws_info_set_defaults (info);

  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (info);

  GList *line_list = NULL;

  GIOChannel *channel;
  channel = g_io_channel_new_file (path, "r", NULL);

  if (!channel)
    {
      g_set_error (err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE,
                   _("Failed to open file: \"%s\"."), path);
      return FALSE;
    }

  gchar *line = NULL;;
  gsize line_length;
  gsize terminator_pos;

  GIOStatus status;
  status = g_io_channel_read_line (channel, &line, &line_length,
                                   &terminator_pos, NULL);

  while (status == G_IO_STATUS_NORMAL)
    {
      if (line && terminator_pos)
        line[terminator_pos] = '\0';

      line_list = g_list_append (line_list, g_strdup (line));
      g_free (line);
      line = NULL;

      status = g_io_channel_read_line (channel, &line, &line_length,
                                       &terminator_pos, NULL);
    }
  g_free (line);
  line = NULL;

  g_io_channel_shutdown (channel, FALSE, NULL);
  g_io_channel_unref (channel);

  gboolean has_files = FALSE;

  GList *iter;

  for (iter = line_list;
       iter && !has_files;
       iter = g_list_next (iter))
    {
      line = iter->data;

      if (g_str_has_prefix (line, "files"))
        {
          has_files = TRUE;
        }
      else if (g_str_has_prefix (line, "rotate-image"))
        {
          priv->rotate_image = TRUE;
        }
      else if (g_str_has_prefix (line, "single-image"))
        {
          priv->rotate_image = FALSE;
        }
      else if (g_str_has_prefix (line, "time"))
        {
          GRegex *regex;
          regex = g_regex_new ("^time\\s+(\\S.*)$", 0, 0, NULL);
          g_assert (regex);

          GMatchInfo *match_info = NULL;
          gboolean found_match;
          found_match = g_regex_match (regex, line, 0, &match_info);

          if (!found_match)
            {
              g_set_error (err,
                           JWS_INFO_ERROR,
                           JWS_INFO_ERROR_FILE_FORMAT,
                           _("No argument was found to time in line: "
                             "\"%s\"."), line);
              g_regex_unref (regex);
              g_match_info_free (match_info);
              g_list_free_full (line_list, (GDestroyNotify) g_free);
              return FALSE;
            }
          gchar *value;
          value = g_match_info_fetch (match_info, 1);

          if (!value || strlen (value) <= 0)
            {
              g_set_error (err,
                           JWS_INFO_ERROR,
                           JWS_INFO_ERROR_FILE_FORMAT,
                           _("Failed to find valid argument to time."));
              g_regex_unref (regex);
              g_match_info_free (match_info);
              g_list_free_full (line_list, (GDestroyNotify) g_free);
			  /* If it was NULL but had a length of zero.  */
			  g_free (value);
              return FALSE;
            }

          JwsTimeValue *rotate_time;
          rotate_time = jws_time_value_new_from_string (value);

          if (!rotate_time)
            {
              g_set_error (err,
                           JWS_INFO_ERROR,
                           JWS_INFO_ERROR_FILE_FORMAT,
                           _("Failed to parse time sting: \"%s\"."),
                           value);
              g_regex_unref (regex);
              g_match_info_free (match_info);
              g_list_free_full (line_list, (GDestroyNotify) g_free);
			  g_free (value);
              return FALSE;
            }

          if (jws_time_value_total_seconds (rotate_time) <= 0)
            {
              g_set_error (err,
                           JWS_INFO_ERROR,
                           JWS_INFO_ERROR_FILE_FORMAT,
                           _("Time must be greater than 0."));
              g_regex_unref (regex);
              g_match_info_free (match_info);
              g_list_free_full (line_list, (GDestroyNotify) g_free);
			  g_free (value);
              return FALSE;
            }

          jws_info_set_rotate_time (info, rotate_time);

          jws_time_value_free (rotate_time);
          g_free (value);

          g_regex_unref (regex);
          g_match_info_free (match_info);
        }
      else if (g_str_has_prefix (line, "randomize-order"))
        {
          priv->randomize_order = TRUE;
        }
      else if (g_str_has_prefix (line, "in-order"))
        {
          priv->randomize_order = FALSE;
        }
	  else if (g_str_has_prefix (line, "mode"))
		{
		  GRegex *reg;
		  reg = g_regex_new ("^mode\\s+(\\S+)$", 0, 0, NULL);

		  g_assert (reg);

		  GMatchInfo *match_info = NULL;
		  gboolean found_match;

		  found_match = g_regex_match (reg, line, 0, &match_info);

		  if (!found_match)
			{
			  g_set_error (err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE_FORMAT,
						   _("Invalid mode format"));
			  g_list_free_full (line_list, (GDestroyNotify) g_free);
			  g_regex_unref (reg);
			  g_match_info_free (match_info);
			  return FALSE;
			}

		  gchar *value;
		  value = g_match_info_fetch (match_info, 1);

		  if (!value || strlen (value) == 0)
			{
			  g_set_error (err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE_FORMAT,
						   _("Couldn't find argument for mode."));
			  g_list_free_full (line_list, (GDestroyNotify) g_free);
			  g_regex_unref (reg);
			  g_match_info_free (match_info);
			  g_free (value);
			  return FALSE;
			}

		  gboolean is_mode;
		  JwsWallpaperMode mode;
		  is_mode = jws_wallpaper_mode_from_info_string (value, &mode);

		  if (!is_mode)
			{
			  g_set_error (err, JWS_INFO_ERROR, JWS_INFO_ERROR_FILE_FORMAT,
						   _("Unrecognized mode \"%s\"."), value);
			  g_list_free_full (line_list, (GDestroyNotify) g_free);
			  g_regex_unref (reg);
			  g_match_info_free (match_info);
			  g_free (value);
			  return FALSE;
			}

		  jws_info_set_mode (info, mode);
		  g_regex_unref (reg);
		  g_match_info_free (match_info);
		  g_free (value);
		}
    }

  if (!has_files)
    {
      g_set_error (err,
                   JWS_INFO_ERROR,
                   JWS_INFO_ERROR_NO_FILES,
                   _("No files found."));
      g_list_free_full (line_list, (GDestroyNotify) g_free);
      return FALSE;
    }


  gboolean found_file = FALSE;
  for (; iter != NULL; iter = g_list_next (iter))
    {
      line = iter->data;

      if (strlen (line) > 0)
        {
          jws_info_add_file (info, line);
          found_file = TRUE;
        }
    }

  if (!found_file)
    {
      g_set_error (err,
                   JWS_INFO_ERROR,
                   JWS_INFO_ERROR_NO_FILES,
                   _("No files found."));
      g_list_free_full (line_list, (GDestroyNotify) g_free);
      return FALSE;
    }

  g_list_free_full (line_list, (GDestroyNotify) g_free);

  return TRUE;
}

void
print_jws_info (JwsInfo *info)
{
  if (info)
    {
      JwsInfoPrivate *priv;
      priv = jws_info_get_instance_private (info);

      if (priv->rotate_image)
        {
          g_print (_("Rotate image\n"));
          
          g_print (_("Seconds between rotation: %i\n"),
                   jws_time_value_total_seconds (priv->rotate_time));

          if (priv->randomize_order)
            g_print (_("Randomize order\n"));
          else
            g_print (_("In order"));

        }
      else
        {
          g_print (_("Single image\n"));
        }

      GList *file_list = priv->file_list;

      if (file_list)
        {
          g_print (_("Files:\n"));

          for (; file_list != NULL; file_list = g_list_next (file_list))
            {
              g_print ("%s\n", file_list->data);
            }
        }
      else
        {
          g_print (_("No files to print\n"));
        }
    }
  else
    {
      g_print (_("Error: Trying to print null info\n"));
    }
}

int
jws_time_value_total_seconds (JwsTimeValue *time)
{
  int total = 0;
  if (time)
    {
      total += time->seconds;
      total += JWS_SECONDS_PER_MINUTE * time->minutes;
      total += JWS_SECONDS_PER_HOUR * time->hours;
    }
  return total;
}

gboolean
jws_time_value_equal (JwsTimeValue *a, JwsTimeValue *b)
{
  if (!a || !b)
    return FALSE;

  return (jws_time_value_total_seconds (a) == jws_time_value_total_seconds (b));
}

void
jws_time_value_to_simplest_form (JwsTimeValue *time)
{
  if (!time)
    return;

  int total_seconds;
  total_seconds = jws_time_value_total_seconds (time);

  if (total_seconds <= 0)
    return;

  int hours = total_seconds / JWS_SECONDS_PER_HOUR;
  total_seconds -= hours * JWS_SECONDS_PER_HOUR;

  int minutes = total_seconds / JWS_SECONDS_PER_MINUTE;
  total_seconds -= minutes * JWS_SECONDS_PER_MINUTE;

  time->hours = hours;
  time->minutes = minutes;
  time->seconds = total_seconds;
}

JwsTimeValue *
jws_time_value_new ()
{
  return jws_time_value_new_for_values (0, 0, 0);
}

JwsTimeValue *
jws_time_value_new_for_seconds (int seconds)
{
  JwsTimeValue *time;
  time = jws_time_value_new_for_values (0, 0, seconds);
  jws_time_value_to_simplest_form (time);

  return time;
}

JwsTimeValue *
jws_time_value_new_from_string (const char *string)
{
  GRegex *regex;
  regex = g_regex_new ("^(\?:(\\d+)h)\?(\?:(\\d+)m)\?(\?:(\\d+)s\?)\?$",
                       0,
                       0,
                       NULL);
  g_assert (regex);
  GMatchInfo *match_info = NULL;
  gboolean matched;
  matched = g_regex_match (regex, string, 0, &match_info);

  if (!matched)
    {
      return NULL;
    }

  JwsTimeValue *time = jws_time_value_new_for_values (0, 0, 0);

  gchar *value = NULL;
  gboolean found_num = FALSE;

  value = g_match_info_fetch (match_info, 1);
  if (value && strlen (value) > 0)
    {
      time->hours = atoi (value);
      found_num = TRUE;
    }
  g_free (value);

  value = g_match_info_fetch (match_info, 2);
  if (value && strlen (value) > 0)
    {
      time->minutes = atoi (value);
      found_num = TRUE;
    }
  g_free (value);

  value = g_match_info_fetch (match_info, 3);
  if (value && strlen (value) > 0)
    {
      time->seconds = atoi (value);
      found_num = TRUE;
    }

  g_regex_unref (regex);
  g_match_info_free (match_info);

  if (!found_num)
    {
      return NULL;
    }

  return time;
}

JwsTimeValue *
jws_time_value_new_for_values (int hours, int minutes, int seconds)
{
  JwsTimeValue *time = NULL;
  time = g_new (JwsTimeValue, 1);
  time->hours = hours;
  time->minutes = minutes;
  time->seconds = seconds;
  
  return time;
}

JwsTimeValue *
jws_time_value_copy (JwsTimeValue *time)
{
  if (!time)
    return NULL;

  return jws_time_value_new_for_values (time->hours,
                                        time->minutes,
                                        time->seconds);
}

void
jws_time_value_free (JwsTimeValue *value)
{
  g_free (value);
}

void
jws_time_value_set (JwsTimeValue *time, int hours, int minutes, int seconds)
{
  if (!time)
    return;

  time->hours = hours;
  time->minutes = minutes;
  time->seconds = seconds;
}

GQuark
jws_info_error_quark (void)
{
  return g_quark_from_static_string ("jws-info-error-quark");
}

gboolean
jws_info_write_to_file (JwsInfo *info, const gchar *path)
{
  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (info);
  
  GIOChannel *writer;
  writer = g_io_channel_new_file (path, "w", NULL);

  if (!writer)
    return FALSE;

  gboolean status;

  if (priv->rotate_image)
    {
      status = jws_write_line (writer, "rotate-image");
      if (!status)
        {
          g_io_channel_shutdown (writer, TRUE, NULL);
          g_io_channel_unref (writer);
          return FALSE;
        }

      if (priv->randomize_order)
        {
          status = jws_write_line (writer, "randomize-order");
          if (!status)
            {
              g_io_channel_shutdown (writer, TRUE, NULL);
              g_io_channel_unref (writer);
              return FALSE;
            }
        }
      else
        {
          status = jws_write_line (writer, "in-order");
          if (!status)
            {
              g_io_channel_shutdown (writer, TRUE, NULL);
              g_io_channel_unref (writer);
              return FALSE;
            }
        }
      size_t buf_size = 80;
      char buf[buf_size];
      int bytes_written;
      JwsTimeValue *simplest_form;
      simplest_form = jws_time_value_copy (priv->rotate_time);
      jws_time_value_to_simplest_form (simplest_form);
      bytes_written = snprintf (buf, 80, "%ih%im%is",
                                simplest_form->hours,
                                simplest_form->minutes,
                                simplest_form->seconds);
      jws_time_value_free (simplest_form);

      if (bytes_written <= 0 || bytes_written >= buf_size)
        {
          g_io_channel_shutdown (writer, TRUE, NULL);
          g_io_channel_unref (writer);
          return FALSE;
        }
      gchar *time_line;
      time_line = g_strconcat ("time ", buf, NULL);

      status = jws_write_line (writer, time_line);
      g_free (time_line);
      if (!status)
        {
          g_io_channel_shutdown (writer, TRUE, NULL);
          g_io_channel_unref (writer);
          return FALSE;
        }
    }
  else
    {
      status = jws_write_line (writer, "single-image");
      if (!status)
        {
          g_io_channel_shutdown (writer, TRUE, NULL);
          g_io_channel_unref (writer);
          return FALSE;
        }
    }

  status = jws_write_line (writer, "");
  if (!status)
    {
      g_io_channel_shutdown (writer, TRUE, NULL);
      g_io_channel_unref (writer);
      return FALSE;
    }
  status = jws_write_line (writer, "files");
  if (!status)
    {
      g_io_channel_shutdown (writer, TRUE, NULL);
      g_io_channel_unref (writer);
      return FALSE;
    }
  GList *iter;
  for (iter = g_list_first (priv->file_list); iter; iter = g_list_next (iter))
    {
      status = jws_write_line (writer, iter->data);
      if (!status)
        {
          g_io_channel_shutdown (writer, TRUE, NULL);
          g_io_channel_unref (writer);
          return FALSE;
        }
    }
  g_io_channel_shutdown (writer, TRUE, NULL);
  g_io_channel_unref (writer);
  return TRUE;
}

gboolean
jws_write_line (GIOChannel *channel, const gchar *message)
{
  gchar *new_message;
  new_message = g_strconcat (message, "\n", NULL);

  GIOStatus status;
  gsize size;
  status = g_io_channel_write_chars (channel,
                                     new_message,
                                     -1,
                                     &size,
                                     NULL);
  g_free (new_message);
  return (status == G_IO_STATUS_NORMAL);
}

void
jws_info_set_defaults (JwsInfo *info)
{
  g_assert (info);

  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (info);
  
  priv->rotate_image = TRUE;
  priv->randomize_order = FALSE;
  jws_time_value_free (priv->rotate_time);
  priv->rotate_time = jws_time_value_new_for_values (0, 1, 0);

  g_list_free_full (priv->file_list, (GDestroyNotify) g_free);
  priv->file_list = NULL;

  priv->mode = JWS_DEFAULT_WALLPAPER_MODE;
}

JwsWallpaperMode
jws_info_get_mode (JwsInfo *info)
{
  g_assert (info);

  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (info);
  
  return priv->mode;
}

void
jws_info_set_mode (JwsInfo *info, JwsWallpaperMode mode)
{
  g_assert (info);

  JwsInfoPrivate *priv;
  priv = jws_info_get_instance_private (info);
  
  priv->mode = mode;
}

gboolean
jws_wallpaper_mode_from_info_string (const gchar *mode_string,
									 JwsWallpaperMode *mode)
{
  g_assert (mode);
  if (g_strcmp0 (mode_string, JWS_INFO_MODE_FILL) == 0)
	{
	  *mode = JWS_WALLPAPER_MODE_FILL;
	  return TRUE;
	}
  if (g_strcmp0 (mode_string, JWS_INFO_MODE_CENTER) == 0)
	{
	  *mode = JWS_WALLPAPER_MODE_CENTER;
	  return TRUE;
	}
  if (g_strcmp0 (mode_string, JWS_INFO_MODE_MAX) == 0)
	{
	  *mode = JWS_WALLPAPER_MODE_MAX;
	  return TRUE;
	}
  if (g_strcmp0 (mode_string, JWS_INFO_MODE_SCALE) == 0)
	{
	  *mode = JWS_WALLPAPER_MODE_SCALE;
	  return TRUE;
	}
  if (g_strcmp0 (mode_string, JWS_INFO_MODE_TILE) == 0)
	{
	  *mode = JWS_WALLPAPER_MODE_TILE;
	  return TRUE;
	}

  *mode = JWS_DEFAULT_WALLPAPER_MODE;
  return FALSE;
}
