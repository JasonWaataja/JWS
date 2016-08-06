

#ifndef JWSAPPLICATION_H
#define JWSAPPLICATION_H

#include <glib-object.h>
#include <gio/gio.h>

#include "jwsinfo.h"

#define JWS_TYPE_APPLICATION (jws_application_get_type ())
#define JWS_APPLICATION(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), JWS_TYPE_APPLICATION, JwsApplication))

typedef struct _JwsApplication JwsApplication;
typedef struct _JwsApplicationClass JwsApplicationClass;

GType
jws_application_get_type (void);

JwsApplication *
jws_application_new (void);

gint
handle_local_options (GApplication *application,
                      GVariantDict *options,
                      gpointer      user_data);

typedef struct _jws_command_line_options jws_command_line_options;

struct _jws_command_line_options
{
  gchar *config_file;
  gboolean rotate_image;
  gboolean single_image;
  gboolean randomize_order;
  gboolean in_order;
  gint time;
};

JwsInfo *
jws_application_get_current_info (JwsApplication *app);

void
jws_application_set_current_info (JwsApplication *app, JwsInfo *info);

jws_command_line_options *
jws_application_get_command_line_options (JwsApplication *app);

void
jws_application_set_command_line_options (JwsApplication *app,
                                          jws_command_line_options *options);

GList *
jws_application_get_file_list (JwsApplication *app);

void
jws_application_set_file_list (JwsApplication *app, GList *file_list);

/* Thread safe.  */
gboolean
jws_application_get_should_exit_loop (JwsApplication *app);

/* Thread safe.  */
void
jws_application_set_should_exit_loop (JwsApplication *app,
                                      gboolean shoudl_exit);

/* Should be thread safe, locked with a mutex so it can be called safely from a
 * signal handler.  To do this, calls jws_application_set_should_exit_loop
 * which has this built in.  */
void
jws_application_stop_main_loop (JwsApplication *app);

void
jws_application_display_images (JwsApplication *app);

GList *
jws_create_shuffled_list (GList *list);

void
jws_shuffle_list (GList *list);

GList *
jws_create_file_list_for_info (JwsInfo *info);

GList *
jws_collect_regulare_files_in_directory (const char *path);

GList *
jws_add_path_to_list_recursive (GList *list, const char *path);

/* Checks for various config files and returns the first one found.  Currently,
 * it searches for $HOME/.jws, then $XDG_CONFIG_HOME/jws.  If no file is found,
 * then it returns NULL.  The result should be freed with g_free ().  */
gchar *
jws_get_default_config_file ();

#endif
