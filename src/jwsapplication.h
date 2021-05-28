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

#ifndef JWS_JWS_APPLICATION_H
#define JWS_JWS_APPLICATION_H

#include <stdbool.h>

#include <gio/gio.h>
#include <glib-object.h>

#include "jwsinfo.h"

#define JWS_TYPE_APPLICATION (jws_application_get_type())
#define JWS_APPLICATION(obj)                                                   \
	(G_TYPE_CHECK_INSTANCE_CAST((obj), JWS_TYPE_APPLICATION,               \
		JwsApplication))

// An application that manages wallpapers from the command line.
typedef struct _JwsApplication JwsApplication;
typedef struct _JwsApplicationClass JwsApplicationClass;

GType
jws_application_get_type();

JwsApplication *
jws_application_new();

// Handler for the handle-local-options signal. Always returns -1 to continue
// processing options.
int
handle_local_options(GApplication *application, GVariantDict *options,
	gpointer user_data);

// Represents the options that the user can pass to jws.
typedef struct _JwsCommandLineOptions JwsCommandLineOptions;

struct _JwsCommandLineOptions {
	char *config_file;
	bool rotate_image;
	bool single_image;
	bool randomize_order;
	bool in_order;
	char *rotate_time;
	char *mode;
};

// Puts a shallow copy of src into dest.
void
jws_command_line_options_copy(JwsCommandLineOptions *dest,
	JwsCommandLineOptions *src);

// Returns the current info for app.
JwsInfo *
jws_application_get_current_info(JwsApplication *app);

// Sets the info for app to info, incrementing its reference count.
void
jws_application_set_current_info(JwsApplication *app, JwsInfo *info);

// Returns a pointer to the command line arguments that app is using.
JwsCommandLineOptions *
jws_application_get_command_line_options(JwsApplication *app);

// Sets the command line options app uses to a copy of options.
void
jws_application_set_command_line_options(JwsApplication *app,
	JwsCommandLineOptions *options);

// Returns the list of files to display for app. Do not modify.
GList *
jws_application_get_file_list(JwsApplication *app);

// Sets the file list to use for app to a copy of file_list.
void
jws_application_set_file_list(JwsApplication *app, GList *file_list);

// Returns whether or not app should exit its main loop. Thread safe.
bool
jws_application_get_should_exit_loop(JwsApplication *app);

// Sets whether or not app should exit its main loop to should_exit. Thread
// safe.
void
jws_application_set_should_exit_loop(JwsApplication *app, bool should_exit);

// Sets app to stop its main loop. Thread safe.
void
jws_application_stop_main_loop(JwsApplication *app);

// Display the images for app. This will enter an infinite loop if the app
// should rotate its images so that it can keep changing them accordingly.
void
jws_application_display_images(JwsApplication *app);

// Enters infinite loop of displaying each image in the file list.
void
jws_application_display_rotate_images(JwsApplication *app);

// Randomizes the order of list.
void
jws_shuffle_list(GList *list);

// Uses info's list of paths to create a new list of strings representing the
// paths in these files. Specifically, directories are recursively expanded to
// all their regular files.
GList *
jws_create_file_list_for_info(JwsInfo *info);

// Adds all regular files in path and any subdirectories to list recursively
// and returns the new start of the list.
GList *
jws_add_path_to_list(GList *list, const char *path);

// Checks for the existence config files and returns the first one found. The
// order is $XDG_CONFIG_HOME/jws, then $HOME/.jws. If no file is found then
// return NULL. The result should be freed with g_free().
char *
jws_get_default_config_file();

#endif // JWS_JWS_APPLICATION_H
