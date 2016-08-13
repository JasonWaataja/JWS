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

#ifndef JWSINFO_H
#define JWSINFO_H

#include <glib-object.h>

#define JWS_TYPE_INFO (jws_info_get_type ())
#define JWS_INFO(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), JWS_TYPE_INFO, JwsInfo))

typedef struct _JwsInfo JwsInfo;
typedef struct _JwsInfoClass JwsInfoClass;

typedef struct _JwsTimeValue JwsTimeValue;

struct _JwsTimeValue
{
  int hours;
  int minutes;
  int seconds;
};

#define JWS_SECONDS_PER_MINUTE 60
#define JWS_MINUTES_PER_HOUR 60
#define JWS_SECONDS_PER_HOUR (JWS_SECONDS_PER_MINUTE * JWS_MINUTES_PER_HOUR)

JwsTimeValue *
jws_time_value_new ();

JwsTimeValue *
jws_time_value_new_for_seconds (int seconds);

JwsTimeValue *
jws_time_value_new_for_values (int hours, int minutes, int seconds);

void
jws_time_value_free (JwsTimeValue *time);

JwsTimeValue *
jws_time_value_copy (JwsTimeValue *time);

int
jws_time_value_total_seconds (JwsTimeValue *time);

/* False if either is null.  */
gboolean
jws_time_value_equal (JwsTimeValue *a, JwsTimeValue *b);

/* Does nothing if the time value is less than or equal to zero.  */
void
jws_time_value_to_simplest_form (JwsTimeValue *time);

GType
jws_info_get_type ();

JwsInfo *
jws_info_new ();

JwsInfo *
jws_info_new_from_file (const gchar *path);


gboolean
jws_info_get_rotate_image (JwsInfo *info);

void
jws_info_set_rotate_image (JwsInfo *info, gboolean rotate_image);

guint
jws_info_get_rotate_seconds (JwsInfo *info);

void
jws_info_set_rotate_seconds (JwsInfo *info, guint rotate_seconds);

gboolean
jws_info_get_randomize_order (JwsInfo *info);

void
jws_info_set_randomize_order (JwsInfo *info, gboolean randomize_order);

GList *
jws_info_get_file_list (JwsInfo *info);

void
jws_info_set_file_list (JwsInfo *info, GList *file_list);

void
jws_info_add_file (JwsInfo *info, const gchar *path);

void
jws_info_remove_file (JwsInfo *info, const gchar *path);

gboolean
jws_info_set_from_file (JwsInfo *info, const gchar *path);

void
print_jws_info (JwsInfo *info);

#endif
