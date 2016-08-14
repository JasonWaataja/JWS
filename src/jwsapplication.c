/* jwsapplication.c - application for jws

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

#include "jwsapplication.h"

#include <glib.h>
#include <gio/gio.h>
#include <glib/gi18n.h>

#include "jwsinfo.h"
#include "jwssetter.h"

struct _JwsApplication
{
  GApplication parent;
};

struct _JwsApplicationClass
{
  GApplicationClass parent_class;
};

typedef struct _JwsApplicationPrivate JwsApplicationPrivate;

struct _JwsApplicationPrivate
{
  gchar *config_file;
  JwsInfo *current_info;
  JwsCommandLineOptions cmd_options;
  GList *file_list;

  /* To get out of the main loop on the unix signal.  */
  gboolean received_stop_signal;
  /* Mutex to do this thread safely.  */
  GMutex signal_mutex;
};

G_DEFINE_TYPE_WITH_PRIVATE (JwsApplication, jws_application,
                            G_TYPE_APPLICATION);

static void
jws_application_init (JwsApplication *self)
{
  JwsApplicationPrivate *priv;
  priv = jws_application_get_instance_private (self);

  priv->config_file = NULL;

  priv->current_info = jws_info_new ();

  priv->cmd_options.config_file = NULL;
  priv->cmd_options.rotate_image = FALSE;
  priv->cmd_options.single_image = FALSE;
  priv->cmd_options.randomize_order = FALSE;
  priv->cmd_options.in_order = FALSE;
  priv->cmd_options.rotate_time = NULL;

  priv->file_list = NULL;

  priv->received_stop_signal = FALSE;
  g_mutex_init (&priv->signal_mutex);
  
  g_signal_connect (G_OBJECT (self), "handle-local-options",
                    G_CALLBACK (handle_local_options), &priv->cmd_options);
}

static void
jws_application_startup (GApplication *app)
{
  G_APPLICATION_CLASS (jws_application_parent_class)->startup (app);
}

static void
jws_application_activate (GApplication *app)
{
  JwsApplicationPrivate *priv;
  priv = jws_application_get_instance_private (JWS_APPLICATION (app));

  jws_application_display_images (JWS_APPLICATION (app));
}

static void
jws_application_open (GApplication *app,
                      GFile **files,
                      gint n_files,
                      const gchar *hint)
{
  JwsApplicationPrivate *priv;
  priv = jws_application_get_instance_private (JWS_APPLICATION (app));
  
  for (int i = 0; i < n_files; i++)
    {
      gchar *path;
      path = g_file_get_path (files[i]);
      priv->file_list = jws_add_path_to_list_recursive (priv->file_list,
                                                        path);
      g_free (path);
      path = NULL;
    }

  jws_application_display_images (JWS_APPLICATION (app));
}

static void
jws_application_dispose (GObject *obj)
{
  JwsApplicationPrivate *priv;
  priv = jws_application_get_instance_private (JWS_APPLICATION (obj));

  g_clear_object (&(priv->current_info));

  g_mutex_clear (&priv->signal_mutex);

  G_OBJECT_CLASS (jws_application_parent_class)->dispose (obj);
}

static void
jws_application_finalize (GObject *obj)
{
  JwsApplicationPrivate *priv;
  priv = jws_application_get_instance_private (JWS_APPLICATION (obj));

  g_free (priv->config_file);

  GList *iter;

  for (iter = g_list_first (priv->file_list); iter != NULL;
       iter = g_list_next (iter))
    {
      g_free (iter->data);
      iter->data = NULL;
    }
  g_list_free (priv->file_list);

  G_OBJECT_CLASS (jws_application_parent_class)->finalize (obj);
}

static void
jws_application_class_init (JwsApplicationClass *kclass)
{
  G_OBJECT_CLASS (kclass)->dispose = jws_application_dispose;
  G_OBJECT_CLASS (kclass)->finalize = jws_application_finalize;
  G_APPLICATION_CLASS (kclass)->activate = jws_application_activate;
  G_APPLICATION_CLASS (kclass)->startup = jws_application_startup;
  G_APPLICATION_CLASS (kclass)->open = jws_application_open;
}

JwsApplication *
jws_application_new ()
{
  return g_object_new (JWS_TYPE_APPLICATION,
                       "application-id", "com.waataja.jws",
                       "flags", G_APPLICATION_HANDLES_OPEN,
                       NULL);
}

gint
handle_local_options (GApplication *app,
                      GVariantDict *options,
                      gpointer      cmd_options)
{
  JwsApplicationPrivate *priv;
  priv = jws_application_get_instance_private (JWS_APPLICATION (app));

  JwsCommandLineOptions *as_cmd_options;
  as_cmd_options = (JwsCommandLineOptions *) cmd_options;

  if (as_cmd_options->config_file)
    {
      priv->config_file = g_strdup (as_cmd_options->config_file);
    }

  if (!priv->config_file)
    {
      priv->config_file = jws_get_default_config_file ();
    }
  /* This extra statement is just in case getting the default config file
   * returns NULL in which case it won't set anything.  */
  if (priv->config_file)
    {
      jws_info_set_from_file (priv->current_info, priv->config_file);
    }

  if (as_cmd_options->rotate_image)
    jws_info_set_rotate_image (priv->current_info, TRUE);

  if (as_cmd_options->single_image)
    jws_info_set_rotate_image (priv->current_info, FALSE);

  if (as_cmd_options->randomize_order)
    jws_info_set_randomize_order (priv->current_info, TRUE);

  if (as_cmd_options->in_order)
    jws_info_set_randomize_order (priv->current_info, FALSE);

  if (as_cmd_options->rotate_time)
    {
      JwsTimeValue *rotate_time;
      rotate_time = jws_time_value_new_from_string
        (as_cmd_options->rotate_time);

      if (!rotate_time)
        {
          g_printerr (_("Error, invalid time format: \"%s\".\n"),
                      as_cmd_options->rotate_time);
        }
      else if (jws_time_value_total_seconds (rotate_time) <= 0)
        {
          g_printerr (_("Error, must use a time value greater than zero.\n"));
        }
      else
        {
          jws_info_set_rotate_time (priv->current_info, rotate_time);
        }

      jws_time_value_free (rotate_time);
    }
  return -1;
}

JwsInfo *
jws_application_get_current_info (JwsApplication *app)
{
  if (app)
    {
      JwsApplicationPrivate *priv;
      priv = jws_application_get_instance_private (app);
      return priv->current_info;
    }
  else
    {
      return NULL;
    }
}

void
jws_application_set_current_info (JwsApplication *app, JwsInfo *info)
{
  if (app)
    {
      JwsApplicationPrivate *priv;
      priv = jws_application_get_instance_private (app);

      g_object_unref (G_OBJECT (priv->current_info));
      priv->current_info = info;
      g_object_ref (priv->current_info);
    }
}

JwsCommandLineOptions *
jws_application_get_command_line_options (JwsApplication *app)
{
  if (app)
    {
      JwsApplicationPrivate *priv;
      priv = jws_application_get_instance_private (app);
      
      return &(priv->cmd_options);
    }
  else
    {
      return NULL;
    }
}

void
jws_application_set_command_line_options (JwsApplication *app,
                                          JwsCommandLineOptions *options)
{
  if (app && options)
    {
      JwsApplicationPrivate *priv;
      priv = jws_application_get_instance_private (app);
      priv->cmd_options = *options;
    }
}

GList *
jws_application_get_file_list (JwsApplication *app)
{
  if (app)
    {
      JwsApplicationPrivate *priv;
      priv = jws_application_get_instance_private (app);
      
      return priv->file_list;
    }
  else
    {
      return NULL;
    }
}

void
jws_application_set_file_list (JwsApplication *app, GList *file_list)
{
  if (app)
    {
      JwsApplicationPrivate *priv;
      priv = jws_application_get_instance_private (app);

      GList *iter;

      for (iter = g_list_first (priv->file_list);
           iter != NULL;
           iter = g_list_next (iter))
        {
          g_free (iter->data);
          iter->data = NULL;
        }
      g_list_free (priv->file_list);

      priv->file_list = file_list;
    }
}

gboolean
jws_application_get_should_exit_loop (JwsApplication *app)
{
  gboolean should_exit = FALSE;
  if (app)
    {
      JwsApplicationPrivate *priv;
      priv = jws_application_get_instance_private (app);

      g_mutex_lock (&priv->signal_mutex);
      should_exit = priv->received_stop_signal;
      g_mutex_unlock (&priv->signal_mutex);
    }

  return should_exit;
}

void
jws_application_set_should_exit_loop (JwsApplication *app,
                                      gboolean should_exit)
{
  if (app)
    {
      JwsApplicationPrivate *priv;
      priv = jws_application_get_instance_private (app);

      g_mutex_lock (&priv->signal_mutex);
      priv->received_stop_signal = should_exit;
      g_mutex_unlock (&priv->signal_mutex);
    }
}

void
jws_application_stop_main_loop (JwsApplication *app)
{
  if (app)
    {
      jws_application_set_should_exit_loop (app, TRUE);
    }
}

void
jws_application_display_images (JwsApplication *app)
{
  JwsApplicationPrivate *priv;
  priv = jws_application_get_instance_private (app);

  GList *file_list;
  file_list = jws_create_file_list_for_info (priv->current_info);


  /*jws_application_set_file_list (app, file_list);*/
  priv->file_list = g_list_concat (priv->file_list, file_list);

  if (priv->file_list == NULL)
    {
      g_printerr (_("Error: No images to display.\n"));
      return;
    }

  if (jws_info_get_rotate_image (priv->current_info))
    {
      jws_application_set_should_exit_loop (app, FALSE);

      /* Loop forever.  It doesn't matter because the way to start the progam
       * is by running it in the background.  You can kill it by sending
       * SIGINT, though.  */
      while (!jws_application_get_should_exit_loop (app))
        {
          if (jws_info_get_randomize_order (priv->current_info))
            {
              jws_shuffle_list (priv->file_list);
            }
          GList *iter;
          for (iter = priv->file_list;
               iter != NULL && !jws_application_get_should_exit_loop (app);
               iter = g_list_next (iter))
            {
              char *path;
              path = iter->data;
              jws_set_wallpaper_from_file (path);
              JwsTimeValue *rotate_time;
              rotate_time = jws_info_get_rotate_time (priv->current_info);
              int rotate_seconds = jws_time_value_total_seconds (rotate_time);
              gulong sleep_time = rotate_seconds * G_USEC_PER_SEC;
              g_usleep (sleep_time);
            }
        }
    }
  else
    {
      char *path;
      path = g_list_first (priv->file_list)->data;

      jws_set_wallpaper_from_file (path);
    }
}

GList *
jws_create_shuffled_list (GList *list)
{
  guint length = g_list_length (list);

  gpointer *data;
  data = g_new (gpointer, length);

  GList *iter = list;
  for (int i = 0;
       i < length;
       i++, iter = g_list_next (iter))
    {
      data[i] = iter->data;
    }

  for (int i = 0; i < length - 1; i++)
    {
      int rand_ind = g_random_int_range (i, length);
      gpointer temp = data[i];
      data[i] = data[rand_ind];
      data[rand_ind] = temp;
    }

  GList *new_list = NULL;
  for (int i = 0; i < length; i++)
    {
      new_list = g_list_append (new_list, data[i]);
    }

  return new_list;
}

void
jws_shuffle_list (GList *list)
{
  guint length = g_list_length (list);

  gpointer *data;
  data = g_new (gpointer, length);

  GList *iter = list;
  for (int i = 0;
       i < length;
       i++, iter = g_list_next (iter))
    {
      data[i] = iter->data;
    }

  for (int i = 0; i < length - 1; i++)
    {
      int rand_ind = g_random_int_range (i, length);
      gpointer temp = data[i];
      data[i] = data[rand_ind];
      data[rand_ind] = temp;
    }

  iter = list;
  for (int i = 0; i < length; i++, iter = g_list_next (iter))
    {
      iter->data = data[i];
    }
}

GList *
jws_add_path_to_list_recursive (GList *list, const char *path)
{
  GList *new_list = list;

  if (path)
    {
      GFile *as_file = g_file_new_for_path (path);

      GFileType file_type;
      file_type = g_file_query_file_type (as_file,
                                          G_FILE_QUERY_INFO_NONE, NULL);
      if (file_type == G_FILE_TYPE_DIRECTORY)
        {
          GFileEnumerator *en;
          en = (g_file_enumerate_children (as_file,
                                           "*",
                                           G_FILE_QUERY_INFO_NONE,
                                           NULL,
                                           NULL));
          GFileInfo *dirent_info;
          GFile *dirent_file;

          gboolean result;
          result = (g_file_enumerator_iterate (en,
                                               &dirent_info,
                                               &dirent_file,
                                               NULL,
                                               NULL));
          
          while (result && dirent_info)
            {
              gchar *dirent_path = g_file_get_path (dirent_file);
              new_list = (jws_add_path_to_list_recursive (new_list,
                                                          dirent_path));
              result = (g_file_enumerator_iterate (en,
                                                   &dirent_info,
                                                   &dirent_file,
                                                   NULL,
                                                   NULL));
              g_free (dirent_path);
            }
          g_object_unref (en);
        }
      else if (file_type == G_FILE_TYPE_REGULAR)
        {
          gchar *file_path;
          file_path = g_file_get_path (as_file);
          new_list = g_list_append (new_list, g_strdup (file_path));
          g_free (file_path);
        }
      else if (file_type == G_FILE_TYPE_UNKNOWN)
        {
        }
      g_object_unref (as_file);
    }
  return new_list;
}

GList *
jws_collect_regular_files_in_directory (const char *path)
{
  /* I don't think I even have to create a variable here, because I can just
   * pass NULL.  */
  GList *list = NULL;
  list = jws_add_path_to_list_recursive (list, path);
  return list;
}

GList *
jws_create_file_list_for_info (JwsInfo *info)
{
  GList *file_list = NULL;
  
  GList *iter;
  for (iter = jws_info_get_file_list (info); iter != NULL;
       iter = g_list_next (iter))
    {
      file_list = jws_add_path_to_list_recursive (file_list, iter->data);
    }

  return file_list;
}

gchar *
jws_get_default_config_file ()
{
  gchar *config_file_path = NULL;

  GFile *home_dir;
  home_dir = g_file_new_for_path (g_getenv ("HOME"));
  GFile *config_file;
  config_file = g_file_get_child (home_dir, ".jws");

  /* This variable and system is unnecessary, but was included so more can be
   * added later with a check to found_file.  For now, it's just a
   * complication.  */
  gboolean found_file = FALSE;

  if (g_file_query_exists (config_file, NULL))
    {
      config_file_path = g_file_get_path (config_file);
      found_file = TRUE;
    }

  if (!found_file)
    {
      GFile *config_dir;
      config_dir = g_file_new_for_path (g_get_user_config_dir ());

      config_file = g_file_get_child (config_dir, "jws");

      if (g_file_query_exists (config_file, NULL))
        {
          config_file_path = g_file_get_path (config_file);
          found_file = TRUE;
        }
    }

  /* Add more statements here if desired.  */

  g_object_unref (G_OBJECT (home_dir));
  g_object_unref (G_OBJECT (config_file));

  return config_file_path;
}
