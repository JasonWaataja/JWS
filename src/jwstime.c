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

#include "jwstime.h"

#include <errno.h>
#include <limits.h>
#include <string.h>

int
jws_time_value_total_seconds(JwsTimeValue *time)
{
	g_assert(time);
	int total = 0;
	if (time) {
		total += time->seconds;
		total += JWS_SECONDS_PER_MINUTE * time->minutes;
		total += JWS_SECONDS_PER_HOUR * time->hours;
	}
	return total;
}

gboolean
jws_time_value_equal(JwsTimeValue *time1, JwsTimeValue *time2)
{
	g_assert(time1);
	g_assert(time2);
	return jws_time_value_total_seconds(time1) ==
		jws_time_value_total_seconds(time2);
}

void
jws_time_value_to_simplest_form(JwsTimeValue *time)
{
	g_assert(time);
	int total_seconds = jws_time_value_total_seconds(time);

	int hours = total_seconds / JWS_SECONDS_PER_HOUR;
	total_seconds = total_seconds % JWS_SECONDS_PER_HOUR;
	int minutes = total_seconds / JWS_SECONDS_PER_MINUTE;
	total_seconds = total_seconds % JWS_SECONDS_PER_MINUTE;

	time->hours = hours;
	time->minutes = minutes;
	time->seconds = total_seconds;
}

JwsTimeValue *
jws_time_value_new()
{
	return jws_time_value_new_for_values(0, 0, 0);
}

JwsTimeValue *
jws_time_value_new_for_seconds(int seconds)
{
	JwsTimeValue *time = jws_time_value_new_for_values(0, 0, seconds);
	jws_time_value_to_simplest_form(time);
	return time;
}

JwsTimeValue *
jws_time_value_new_from_string(const char *string)
{
	/*
	 * Match an entire line numbers followed by an h, m, or s which each
	 * being optional. Captures numbers of the form specified in this
	 * method's header.
	 */
	GRegex *regex = g_regex_new(
		"^(\?:(\\d+)h)\?(\?:(\\d+)m)\?(\?:(\\d+)s\?)\?$", 0, 0, NULL);
	g_assert(regex);
	GMatchInfo *match_info = NULL;
	gboolean matched = g_regex_match(regex, string, 0, &match_info);
	if (!matched)
		return NULL;

	JwsTimeValue *time = jws_time_value_new_for_values(0, 0, 0);

	gboolean found_num = FALSE;
	gchar *value = g_match_info_fetch(match_info, 1);
	if (value && strlen(value) > 0) {
		errno = 0;
		time->hours = g_ascii_strtoll(value, NULL, 10);
		if (errno == 0)
			found_num = TRUE;
	}
	g_free(value);

	value = g_match_info_fetch(match_info, 2);
	if (value && strlen(value) > 0) {
		errno = 0;
		time->minutes = g_ascii_strtoll(value, NULL, 10);
		if (errno == 0)
			found_num = TRUE;
	}
	g_free(value);

	value = g_match_info_fetch(match_info, 3);
	if (value && strlen(value) > 0) {
		errno = 0;
		time->seconds = g_ascii_strtoll(value, NULL, 10);
		if (errno == 0)
			found_num = TRUE;
	}
	g_regex_unref(regex);
	g_match_info_free(match_info);

	return (found_num) ? time : NULL;
}

JwsTimeValue *
jws_time_value_new_for_values(int hours, int minutes, int seconds)
{
	JwsTimeValue *time = g_new(JwsTimeValue, 1);
	time->hours = hours;
	time->minutes = minutes;
	time->seconds = seconds;
	return time;
}

JwsTimeValue *
jws_time_value_copy(JwsTimeValue *time)
{
	g_assert(time);
	return jws_time_value_new_for_values(time->hours, time->minutes,
		time->seconds);
}

void
jws_time_value_free(JwsTimeValue *value)
{
	g_free(value);
}

void
jws_time_value_set(JwsTimeValue *time, int hours, int minutes, int seconds)
{
	g_assert(time);
	time->hours = hours;
	time->minutes = minutes;
	time->seconds = seconds;
}
