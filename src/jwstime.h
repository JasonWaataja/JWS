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

#include <glib-object.h>

/*
 * A JwsTimeValue is an amount of time represented by hours, minutes, and
 * seconds.
 */
typedef struct _JwsTimeValue JwsTimeValue;

struct _JwsTimeValue {
	int hours;
	int minutes;
	int seconds;
};

#define JWS_SECONDS_PER_MINUTE 60
#define JWS_MINUTES_PER_HOUR 60
#define JWS_SECONDS_PER_HOUR (JWS_SECONDS_PER_MINUTE * JWS_MINUTES_PER_HOUR)

/* Returns a new time value representing zero seconds. */
JwsTimeValue *
jws_time_value_new();

/*
 * Returns a new time value in simplified form for the given number of seconds.
 */
JwsTimeValue *
jws_time_value_new_for_seconds(int seconds);

/* Returns a new time value with the given hours, minutes, and seconds. */
JwsTimeValue *
jws_time_value_new_for_values(int hours, int minutes, int seconds);

/* Returns NULL upon failure. Takes args of the form xxHyyMkkS. */
JwsTimeValue *
jws_time_value_new_from_string(const char *string);

/* Frees the given time value. */
void
jws_time_value_free(JwsTimeValue *time);

/* Returns a new time value with the same values as the given one. */
JwsTimeValue *
jws_time_value_copy(JwsTimeValue *time);

/*
 * Set the hours, minutes, and seconds for the given time. */
void
jws_time_value_set(JwsTimeValue *time, int hours, int minutes, int seconds);

/* Returns the total time in seconds of the given time . */
int
jws_time_value_total_seconds(JwsTimeValue *time);

/* Returns whether or not time1 and time2 represent the same total time. */
gboolean
jws_time_value_equal(JwsTimeValue *time1, JwsTimeValue *time2);

/*
 * Transforms time into its unique most reduced form where hours, minutes, and
 * seconds are maximized in that order.
 */
void
jws_time_value_to_simplest_form(JwsTimeValue *time);
