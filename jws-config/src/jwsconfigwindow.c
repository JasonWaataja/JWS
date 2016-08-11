/* jwsconfigwindow.c - window for jws-config

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

#include "jwsconfigwindow.h"

#include <glib/gi18n.h>

#include "jwsconfigimageviewer.h"
#include "jwsinfo.h"
#include "jwssetter.h"

struct _JwsConfigWindow
{
  GtkApplicationWindow parent;
};

struct _JwsConfigWindowClass
{
  GtkApplicationWindowClass parent_class;
};

typedef struct _JwsConfigWindowPrivate JwsConfigWindowPrivate;

struct _JwsConfigWindowPrivate
{
  GtkWidget *rotate_button;
  GtkWidget *time_button;
  GtkWidget *time_unit_box;
  GtkWidget *randomize_button;
  GtkWidget *apply_button;
  GtkWidget *add_button;
  GtkWidget *add_directory_button;
  GtkWidget *tree_view;

  GtkWidget *remove_button;
  GtkWidget *up_button;
  GtkWidget *down_button;
  GtkWidget *cancel_button;

  GtkWidget *rotate_items_box;

  GtkTreeStore *tree_store;
  GtkTreeSelection *tree_selection;

  /* Always use and accessors for this which use the mutex.  */
  gboolean should_exit_thread;
  GMutex should_exit_thread_mutex;

  GThread *preview_thread;
  GAsyncQueue *preview_queue;

  JwsInfo *current_info;
};

G_DEFINE_TYPE_WITH_PRIVATE (JwsConfigWindow, jws_config_window,
                            GTK_TYPE_APPLICATION_WINDOW);

static void
jws_config_window_dispose (GObject *obj);

static void
jws_config_window_finalize (GObject *obj);

static void
on_rotate_button_toggled (JwsConfigWindow *win,
                          GtkToggleButton *rotate_button);

static void
jws_config_window_set_up_tree_view (JwsConfigWindow *win);

static void
type_column_data_func (GtkTreeViewColumn *tree_column,
                       GtkCellRenderer *cell,
                       GtkTreeModel *tree_model,
                       GtkTreeIter *iter,
                       gpointer data);

static void
jws_config_window_add_file_for_iter_recurse (JwsConfigWindow *win,
                                             const char *path,
                                             GtkTreeIter *parent_iter);

static void
destroy_row_reference (gpointer row_ref);

static void *
preview_thread_run (gpointer win);

/* Free with g_free when done.  */
static gchar *
get_home_directory ();

static void
on_row_activated (GtkTreeView *view,
                  GtkTreePath *path,
                  GtkTreeViewColumn *column,
                  gpointer user_data);

static void
on_selection_changed (GtkTreeSelection *selection,
                      JwsConfigWindow *win);

static GSList *
add_tree_path_to_slist (GSList *list, GtkTreeModel *model, GtkTreeIter *iter);

static void
on_remove_button_clicked (JwsConfigWindow *win);

static int
tree_path_sort_func (GtkTreePath *a, GtkTreePath *b);

static void
on_up_button_clicked (JwsConfigWindow *win);

static void
on_down_button_clicked (JwsConfigWindow *win);

static gboolean
on_tree_view_button_press (GtkWidget *tree_view,
                           GdkEvent *event,
                           gpointer win);

static void
open_activated (GSimpleAction *action,
                GVariant *parameter,
                gpointer win);

static void
save_activated (GSimpleAction *action,
                GVariant *parameter,
                gpointer win);

static void
save_as_activated (GSimpleAction *action,
                   GVariant *parameter,
                   gpointer win);

static void
quit_activated (GSimpleAction *action,
                GVariant *parameter,
                gpointer win);

static void
about_activated (GSimpleAction *action,
       GVariant *parameter,
       gpointer win);

typedef struct _WindowRowEntry WindowRowEntry;

struct _WindowRowEntry
{
  JwsConfigWindow *win;
  GtkTreeRowReference *row_ref;
};

static void
window_row_entry_free (WindowRowEntry *entry);

static void
on_open_menu_activated (WindowRowEntry *entry);

static void
on_up_menu_activated (WindowRowEntry *entry);

static void
on_down_menu_activated (WindowRowEntry *entry);

static void
on_remove_menu_activated (WindowRowEntry *entry);

static void
on_set_menu_activated (WindowRowEntry *entry);

static GActionEntry win_entries[] =
{
    {"open", open_activated, NULL, NULL, NULL},
    {"save", save_activated, NULL, NULL, NULL},
    {"save-as", save_as_activated, NULL, NULL, NULL},
    {"quit", quit_activated, NULL, NULL, NULL},
    {"about", about_activated, NULL, NULL, NULL}
};

enum tree_store_columns
{
  PATH_COLUMN = 0,
  NAME_COLUMN,
  IS_DIRECTORY_COLUMN,
  PREVIEW_COLUMN,
  N_COLUMNS
};

void
jws_config_window_init (JwsConfigWindow *self)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (self);

  gtk_widget_init_template (GTK_WIDGET (self));

  gtk_spin_button_set_range (GTK_SPIN_BUTTON (priv->time_button), 0, 99999);
  gtk_spin_button_set_increments (GTK_SPIN_BUTTON (priv->time_button),
                                  1, 10);

  priv->tree_store = gtk_tree_store_new (N_COLUMNS,
                                         G_TYPE_STRING, /* 0, path */
                                         G_TYPE_STRING, /* 1, name */
                                         G_TYPE_BOOLEAN,/* 2, is directory */
                                         GDK_TYPE_PIXBUF);/* 3, preview */

  jws_config_window_set_up_tree_view (self);

  priv->preview_queue = g_async_queue_new_full (destroy_row_reference);

  g_mutex_init (&priv->should_exit_thread_mutex);
  jws_config_window_set_should_exit_thread (self, FALSE);

  priv->preview_thread = g_thread_new ("Preview Thread",
                                       preview_thread_run,
                                       self);

  priv->current_info = jws_info_new ();

  g_signal_connect_swapped (priv->rotate_button, "toggled",
                            G_CALLBACK (on_rotate_button_toggled),
                            self);
  g_signal_connect_swapped (priv->add_button, "clicked",
                            G_CALLBACK (jws_config_window_add_file_selection),
                            self);
  g_signal_connect_swapped (priv->add_directory_button, "clicked",
                            G_CALLBACK
                            (jws_config_window_add_directory_selection),
                            self);
  g_signal_connect_swapped (priv->remove_button, "clicked",
                            G_CALLBACK (on_remove_button_clicked),
                            self);
  g_signal_connect_swapped (priv->up_button, "clicked",
                            G_CALLBACK (on_up_button_clicked),
                            self);
  g_signal_connect_swapped (priv->down_button, "clicked",
                            G_CALLBACK (on_down_button_clicked),
                            self);
  g_signal_connect_swapped (priv->cancel_button, "clicked",
                            G_CALLBACK (gtk_tree_selection_unselect_all),
                            priv->tree_selection);
  g_signal_connect (priv->tree_view, "row-activated",
                    G_CALLBACK (on_row_activated), self);
  g_signal_connect (priv->tree_view, "button-press-event",
                    G_CALLBACK (on_tree_view_button_press), self);
  g_signal_connect (priv->tree_selection, "changed",
                    G_CALLBACK (on_selection_changed), self);

  g_action_map_add_action_entries (G_ACTION_MAP (self),
                                   win_entries,
                                   G_N_ELEMENTS (win_entries),
                                   self);

  jws_config_window_show_optional_side_buttons (self, FALSE);
  jws_config_window_show_rotate_items (self, FALSE);
}

void
jws_config_window_class_init (JwsConfigWindowClass *kclass)
{
  gtk_widget_class_set_template_from_resource (GTK_WIDGET_CLASS (kclass),
                                               "/com/waataja/jwsconfig/"
                                               "ui/jwswindow.ui");
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                apply_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                time_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                time_unit_box);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                randomize_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                remove_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                up_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                down_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                cancel_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                rotate_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                rotate_items_box);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                tree_view);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                add_button);
  gtk_widget_class_bind_template_child_private (GTK_WIDGET_CLASS (kclass),
                                                JwsConfigWindow,
                                                add_directory_button);

  G_OBJECT_CLASS (kclass)->dispose = jws_config_window_dispose;
  G_OBJECT_CLASS (kclass)->finalize = jws_config_window_finalize;
}

static void
jws_config_window_dispose (GObject *obj)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (JWS_CONFIG_WINDOW (obj));

  /* Stopping the thread should happen first because operations inside inside
   * it might cause errors if objects don't exist.  */
  jws_config_window_set_should_exit_thread (JWS_CONFIG_WINDOW (obj), TRUE);
  g_thread_join (priv->preview_thread);

  g_mutex_clear (&priv->should_exit_thread_mutex);

  if (priv->preview_queue)
    g_async_queue_unref (priv->preview_queue);
  priv->preview_queue = NULL;

  g_clear_object (&priv->tree_store);



  G_OBJECT_CLASS (jws_config_window_parent_class)->dispose (obj);
}

static void
jws_config_window_finalize (GObject *obj)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (JWS_CONFIG_WINDOW (obj));
  
  G_OBJECT_CLASS (jws_config_window_parent_class)->finalize (obj);
}

JwsConfigWindow *
jws_config_window_new (JwsConfigApplication *app)
{
  return g_object_new (JWS_TYPE_CONFIG_WINDOW,
                       "application", app,
                       NULL);
}

void
jws_config_window_show_optional_side_buttons (JwsConfigWindow *win,
                                              gboolean visible)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkWidget *side_buttons[] = 
    {
      priv->remove_button,
      priv->up_button,
      priv->down_button,
      priv->cancel_button
    };

  for (int i = 0; i < (sizeof (side_buttons) / sizeof (side_buttons[0])); i++)
    {
      gtk_widget_set_visible (side_buttons[i], visible);
    }
}

void
jws_config_window_show_rotate_items (JwsConfigWindow *win,
                                     gboolean visible)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  gtk_widget_set_visible (GTK_WIDGET (priv->rotate_items_box), visible);
}

static void
on_rotate_button_toggled (JwsConfigWindow *win,
                          GtkToggleButton *rotate_button)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  gboolean is_active;
  is_active = gtk_toggle_button_get_active (rotate_button);

  jws_config_window_show_rotate_items (win, is_active);
}

static void
type_column_data_func (GtkTreeViewColumn *tree_column,
                       GtkCellRenderer *cell,
                       GtkTreeModel *tree_model,
                       GtkTreeIter *iter,
                       gpointer data)
{
  gboolean is_directory;
  gtk_tree_model_get (tree_model, iter, IS_DIRECTORY_COLUMN, &is_directory,
                      -1);

  gchar *type_string;
  type_string = jws_get_type_string (is_directory);
  g_object_set (cell, "text", type_string, NULL);
  g_free (type_string);
}

static void
jws_config_window_set_up_tree_view (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeView *as_view = GTK_TREE_VIEW (priv->tree_view);
  gtk_tree_view_set_model (as_view, GTK_TREE_MODEL (priv->tree_store));

  GtkTreeViewColumn *name_column;
  GtkTreeViewColumn *type_column;
  GtkTreeViewColumn *preview_column;

  GtkCellRenderer *text_renderer;
  text_renderer = GTK_CELL_RENDERER (gtk_cell_renderer_text_new ());
  GtkCellRenderer *pixbuf_renderer;
  pixbuf_renderer = GTK_CELL_RENDERER (gtk_cell_renderer_pixbuf_new ());


  name_column = gtk_tree_view_column_new_with_attributes (_("Name"),
                                                          text_renderer,
                                                          "text", 1,
                                                          NULL);
  gtk_tree_view_insert_column (as_view, name_column, NAME_COLUMN);

  type_column = gtk_tree_view_column_new ();
  gtk_tree_view_column_set_title (type_column, _("Type"));
  gtk_tree_view_column_pack_start (type_column, text_renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (type_column, text_renderer,
                                           type_column_data_func, NULL, NULL);
  gtk_tree_view_append_column (as_view, type_column);

  preview_column = gtk_tree_view_column_new_with_attributes (_("Preview"),
                                                             pixbuf_renderer,
                                                             "pixbuf",
                                                             PREVIEW_COLUMN,
                                                             NULL);
  gtk_tree_view_insert_column (as_view, preview_column, PREVIEW_COLUMN);

  priv->tree_selection = gtk_tree_view_get_selection
    (GTK_TREE_VIEW (priv->tree_view));
  gtk_tree_selection_set_mode (priv->tree_selection, GTK_SELECTION_MULTIPLE);
}

static gchar *
get_home_directory ()
{
  return g_strdup (g_getenv ("HOME"));
}

void
jws_config_window_add_file_selection (JwsConfigWindow *win)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (_("Choose File"),
                                        GTK_WINDOW (win),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        _("Add"), GTK_RESPONSE_ACCEPT,
                                        _("Cancel"), GTK_RESPONSE_CANCEL,
                                        NULL);
  gchar *home_dir;
  home_dir = get_home_directory ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), home_dir);
  g_free (home_dir);

  gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (dialog), TRUE);

  GtkFileFilter *filter;
  filter = gtk_file_filter_new ();

  gtk_file_filter_add_pixbuf_formats (filter);

  /* I don't think I have to free this because it's a floating reference.  */
  gtk_file_chooser_set_filter (GTK_FILE_CHOOSER (dialog), filter);

  int response;
  response = gtk_dialog_run (GTK_DIALOG (dialog));

  GSList *file_list = NULL;

  if (response == GTK_RESPONSE_ACCEPT)
    {
      file_list = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));
    }

  gtk_widget_destroy (dialog);

  GSList *iter;
  for (iter = file_list; iter != NULL; iter = g_slist_next (iter))
    {
      jws_config_window_add_file (win, iter->data);
      g_free (iter->data);
    }
  g_slist_free (file_list);
}

void
jws_config_window_add_directory_selection (JwsConfigWindow *win)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (_("Choose Directory"),
                                        GTK_WINDOW (win),
                                        GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
                                        _("Add"), GTK_RESPONSE_ACCEPT,
                                        _("Cancel"), GTK_RESPONSE_CANCEL,
                                        NULL);
  gchar *home_dir;
  home_dir = get_home_directory ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), home_dir);
  g_free (home_dir);

  gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (dialog), TRUE);

  int response;
  response = gtk_dialog_run (GTK_DIALOG (dialog));

  GSList *file_list = NULL;

  if (response == GTK_RESPONSE_ACCEPT)
    {
      file_list = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));
    }

  gtk_widget_destroy (dialog);

  GSList *iter;
  for (iter = file_list; iter != NULL; iter = g_slist_next (iter))
    {
      jws_config_window_add_file (win, iter->data);
      g_free (iter->data);
    }
  g_slist_free (file_list);
}

static void
jws_config_window_add_file_for_iter_recurse (JwsConfigWindow *win,
                                             const char *path,
                                             GtkTreeIter *parent_iter)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GFile *file;
  file = g_file_new_for_path (path);

  GFileType file_type;
  file_type = g_file_query_file_type (file, G_FILE_QUERY_INFO_NONE, NULL);

  gchar *basename;
  basename = g_file_get_basename (file);

  /* This variable is redundant becuase I could just use path from the
   * arguments list but I felt like doing it this way just in case path is
   * formatted weirdly or something.  */
  gchar *file_path;
  file_path = g_file_get_path (file);

  gchar *type_string;

  GtkTreeIter iter;
  gtk_tree_store_append (priv->tree_store, &iter, parent_iter);

  if (file_type == G_FILE_TYPE_REGULAR)
    {
      type_string = jws_get_type_string (FALSE);

      gtk_tree_store_set (priv->tree_store, &iter,
                          PATH_COLUMN, file_path,
                          NAME_COLUMN, basename,
                          IS_DIRECTORY_COLUMN, FALSE,
                          PREVIEW_COLUMN, NULL,
                          -1);

      GtkTreePath *as_path;
      as_path = gtk_tree_model_get_path (GTK_TREE_MODEL (priv->tree_store),
                                         &iter);
      GtkTreeRowReference *row_ref;
      row_ref = gtk_tree_row_reference_new (GTK_TREE_MODEL (priv->tree_store),
                                            as_path);
      gtk_tree_path_free (as_path);

      g_async_queue_push (priv->preview_queue, row_ref);
    }
  else if (file_type == G_FILE_TYPE_DIRECTORY)
    {
      type_string = jws_get_type_string (TRUE);


      gtk_tree_store_set (priv->tree_store, &iter,
                          PATH_COLUMN, file_path,
                          NAME_COLUMN, basename,
                          IS_DIRECTORY_COLUMN, TRUE,
                          PREVIEW_COLUMN, NULL,
                          -1);

      GFileEnumerator *enumerator;
      enumerator = g_file_enumerate_children (file,
                                              "*",
                                              G_FILE_QUERY_INFO_NONE,
                                              NULL,
                                              NULL);

      if (enumerator)
        {
          GFile *child_file;

          gboolean iter_err = TRUE;
          iter_err = g_file_enumerator_iterate (enumerator,
                                                NULL,
                                                &child_file,
                                                NULL,
                                                NULL);

          while (iter_err && child_file != NULL)
            {
              gchar *child_path;
              child_path = g_file_get_path (child_file);
              jws_config_window_add_file_for_iter_recurse (win,
                                                           child_path,
                                                           &iter);
              g_free (child_path);
              iter_err = g_file_enumerator_iterate (enumerator,
                                                    NULL,
                                                    &child_file,
                                                    NULL,
                                                    NULL);
            }
        }
      g_object_unref (enumerator);
    }

  g_object_unref (file);
  g_free (type_string);
  g_free (basename);
  g_free (file_path);
}

void
jws_config_window_add_file (JwsConfigWindow *win, const char *path)
{
  jws_config_window_add_file_for_iter_recurse (win, path, NULL);
}

gchar *
jws_get_type_string (gboolean is_directory)
{
  gchar *type_string = NULL;
  if (is_directory)
    {
      type_string = g_strdup (_("Directory"));
    }
  else
    {
      type_string = g_strdup (_("File"));
    }
  return type_string;
}

static void
destroy_row_reference (gpointer row_ref)
{
  gtk_tree_row_reference_free (((GtkTreeRowReference *) row_ref));
}

static void *
preview_thread_run (gpointer win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (JWS_CONFIG_WINDOW (win));

  GtkTreePath *tree_path;
  GtkTreeIter iter;

  gchar *path;

  while (!jws_config_window_get_should_exit_thread (JWS_CONFIG_WINDOW (win)))
    {
      GtkTreeRowReference *row_ref;
      row_ref = g_async_queue_try_pop (priv->preview_queue);

      if (row_ref && gtk_tree_row_reference_valid (row_ref))
        {
          tree_path = gtk_tree_row_reference_get_path (row_ref);
          if (tree_path)
            {
              gtk_tree_model_get_iter (GTK_TREE_MODEL (priv->tree_store),
                                       &iter,
                                       tree_path);
              gtk_tree_model_get (GTK_TREE_MODEL (priv->tree_store),
                                  &iter,
                                  PATH_COLUMN, &path,
                                  -1);
              GdkPixbuf *preview_src = gdk_pixbuf_new_from_file (path,
                                                                 NULL);
              g_free (path);
              if (preview_src)
                {
                  GdkPixbuf *preview;
                  int height = JWS_CONFIG_WINDOW_PREVIEW_HEIGHT;
                  preview =
                    jws_create_scaled_pixbuf (preview_src,
                                              -1,
                                              height);
                  g_object_unref (preview_src);
                  /* I need to rewrite this part so that it uses something like
                   * a mutex instead to make sure that it wasn't removed.  */
                  if (gtk_tree_row_reference_valid (row_ref))
                    {
                      gtk_tree_store_set (priv->tree_store, &iter,
                                          PREVIEW_COLUMN, preview,
                                          -1);
                    }
                  g_object_unref (preview);
                }
            }
          gtk_tree_row_reference_free (row_ref);
          gtk_tree_path_free (tree_path);
        }
    }
  return NULL;
}

GdkPixbuf *
jws_create_scaled_pixbuf (GdkPixbuf *src,
                          int width,
                          int height)
{
  int src_width;
  int src_height;

  GdkPixbuf *dest = NULL;;

  if (src)
    {
      src_width = gdk_pixbuf_get_width (src);
      src_height = gdk_pixbuf_get_height (src);

      int dest_width = src_width;
      int dest_height = src_height;

      if (width > 0)
        {
          dest_width = width;
          if (height <= 0)
            {
              dest_height = width * src_height / src_width;
            }
        }
      if (height > 0)
        {
          dest_height = height;
          if (width <= 0)
            {
              dest_width = height * src_width / src_height;
            }
        }

      dest = gdk_pixbuf_scale_simple (src, dest_width, dest_height,
                                      GDK_INTERP_HYPER);

    }

  return dest;
}

gboolean
jws_config_window_get_should_exit_thread (JwsConfigWindow *win)
{
  gboolean should_exit = FALSE;
  if (win)
    {
      JwsConfigWindowPrivate *priv;
      priv = jws_config_window_get_instance_private (win);

      g_mutex_lock (&priv->should_exit_thread_mutex);
      should_exit = priv->should_exit_thread;
      g_mutex_unlock (&priv->should_exit_thread_mutex);
    }

  return should_exit;
}

void
jws_config_window_set_should_exit_thread (JwsConfigWindow *win,
                                          gboolean should_exit_thread)
{
  if (win)
    {
      JwsConfigWindowPrivate *priv;
      priv = jws_config_window_get_instance_private (win);
      
      g_mutex_lock (&priv->should_exit_thread_mutex);
      priv->should_exit_thread = should_exit_thread;
      g_mutex_unlock (&priv->should_exit_thread_mutex);
    }
}

void
jws_config_window_show_image_for_row (JwsConfigWindow *win,
                                      GtkTreeRowReference *row_ref)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  JwsConfigImageViewer *viewer;
  viewer = jws_config_image_viewer_new (win, row_ref);
  gtk_widget_show (GTK_WIDGET (viewer));
}

gchar *
jws_config_window_get_path_for_row (JwsConfigWindow *win,
                                    GtkTreeRowReference *row_ref)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  gchar *path = NULL;

  GtkTreePath *tree_path;
  GtkTreeIter iter;

  tree_path = gtk_tree_row_reference_get_path (row_ref);
  gtk_tree_model_get_iter (GTK_TREE_MODEL (priv->tree_store), &iter,
                           tree_path);

  gtk_tree_model_get (GTK_TREE_MODEL (priv->tree_store), &iter,
                      PATH_COLUMN, &path,
                      -1);

  gtk_tree_path_free (tree_path);

  return path;
}

static void
on_row_activated (GtkTreeView *view,
                  GtkTreePath *path,
                  GtkTreeViewColumn *column,
                  gpointer win)
{
   GtkTreeModel *model;
   model = gtk_tree_view_get_model (view);

  GtkTreeRowReference *row_ref;
  row_ref = gtk_tree_row_reference_new (model,
                                        path);

  GtkTreeIter iter;
  gtk_tree_model_get_iter (model, &iter, path);

  gboolean is_directory;
  gtk_tree_model_get (model, &iter,
                      IS_DIRECTORY_COLUMN, &is_directory,
                      -1);

  if (!is_directory)
    jws_config_window_show_image_for_row (win, row_ref);
}

static void
on_selection_changed (GtkTreeSelection *selection,
                      JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GtkTreeModel *model;
  model = GTK_TREE_MODEL (priv->tree_store);
  
  GList *selected_paths;
  /* This isn't actually a list of row references.  This function returns a
   * list of paths.  */
  selected_paths = gtk_tree_selection_get_selected_rows (selection,
                                                         &model);

  gboolean show_side_buttons = FALSE;
  if (selected_paths)
    {
      show_side_buttons = TRUE;
    }
  else
    {
      show_side_buttons = FALSE;
    }
  jws_config_window_show_optional_side_buttons (win, show_side_buttons);

  g_list_free_full (selected_paths, (GDestroyNotify) gtk_tree_path_free);
}

static GSList *
add_tree_path_to_slist (GSList *list, GtkTreeModel *model, GtkTreeIter *iter)
{
  gboolean is_directory;

  gtk_tree_model_get (model, iter,
                      IS_DIRECTORY_COLUMN, &is_directory,
                      -1);

  GSList *new_list = list;

  if (!is_directory)
    {
      GtkTreePath *tree_path;
      tree_path = gtk_tree_model_get_path (model, iter);
      new_list = g_slist_append (new_list, tree_path);
    }
  else
    {
      if (gtk_tree_model_iter_has_child (model, iter))
        {
          GtkTreeIter child_iter;
          gboolean is_valid;
          is_valid = gtk_tree_model_iter_children (model,
                                                   &child_iter,
                                                   iter);
          for (; is_valid; is_valid = gtk_tree_model_iter_next (model,
                                                                &child_iter))
            {
              new_list = add_tree_path_to_slist (new_list,
                                                 model,
                                                 &child_iter);
            }
        }
    }
  return new_list;
}

GtkTreeRowReference *
jws_config_window_get_next_image_row (JwsConfigWindow *win,
                                      GtkTreeRowReference *row_ref)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GtkTreeRowReference *new_ref = NULL;
  
  GtkTreeModel *tree_model;
  tree_model = GTK_TREE_MODEL (priv->tree_store);

  GSList *image_list = NULL;

  GtkTreeIter iter;
  gboolean is_empty;

  is_empty = !gtk_tree_model_get_iter_first(tree_model, &iter);

  if (!is_empty)
    {
      gboolean has_next;
      has_next = gtk_tree_model_get_iter_first (tree_model, &iter);
      for (; has_next;
           has_next = gtk_tree_model_iter_next (tree_model, &iter))
        {
          image_list = add_tree_path_to_slist (image_list, tree_model, &iter);
        }
      if (image_list)
        {
          if (gtk_tree_row_reference_valid (row_ref))
            {
              GtkTreePath *start_path;
              start_path = gtk_tree_row_reference_get_path (row_ref);
              GtkTreePath *current_path = NULL;
              GSList *start_node = NULL;

              GSList *list_iter = NULL;
              gboolean found_start = FALSE;
              for (list_iter = image_list; list_iter != NULL && !found_start;)
                {
                  current_path = list_iter->data;
                  if (gtk_tree_path_compare (start_path, current_path) == 0)
                    {
                      found_start = TRUE;
                      start_node = list_iter;
                    }
                  else
                    {
                      list_iter = g_slist_next (list_iter);
                    }
                }
              if (found_start)
                {
                  list_iter = start_node;
                  list_iter = g_slist_next (list_iter);

                  if (list_iter == NULL)
                    list_iter = image_list;

                  current_path = list_iter->data;

                  if (gtk_tree_path_compare (start_path, current_path) != 0)
                    {
                      new_ref = gtk_tree_row_reference_new (tree_model,
                                                            current_path);
                    }
                }
            }
          for (GSList *list_iter = image_list; list_iter;
               list_iter = g_slist_next (list_iter))
            {
              gtk_tree_path_free (list_iter->data);
            }
          g_slist_free (image_list);
        }
    }
  return new_ref;
}

GtkTreeRowReference *
jws_config_window_get_previous_image_row (JwsConfigWindow *win,
                                      GtkTreeRowReference *row_ref)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GtkTreeRowReference *new_ref = NULL;
  
  GtkTreeModel *tree_model;
  tree_model = GTK_TREE_MODEL (priv->tree_store);

  GSList *image_list = NULL;

  GtkTreeIter iter;
  gboolean is_empty;

  is_empty = !gtk_tree_model_get_iter_first(tree_model, &iter);

  if (!is_empty)
    {
      gboolean has_next;
      has_next = gtk_tree_model_get_iter_first (tree_model, &iter);
      for (; has_next;
           has_next = gtk_tree_model_iter_next (tree_model, &iter))
        {
          image_list = add_tree_path_to_slist (image_list, tree_model, &iter);
        }
      if (image_list)
        {
          image_list = g_slist_reverse (image_list);
          if (gtk_tree_row_reference_valid (row_ref))
            {
              GtkTreePath *start_path;
              start_path = gtk_tree_row_reference_get_path (row_ref);
              GtkTreePath *current_path = NULL;
              GSList *start_node = NULL;

              GSList *list_iter = NULL;
              gboolean found_start = FALSE;
              for (list_iter = image_list; list_iter != NULL && !found_start;)
                {
                  current_path = list_iter->data;
                  if (gtk_tree_path_compare (start_path, current_path) == 0)
                    {
                      found_start = TRUE;
                      start_node = list_iter;
                    }
                  else
                    {
                      list_iter = g_slist_next (list_iter);
                    }
                }
              if (found_start)
                {
                  list_iter = start_node;
                  list_iter = g_slist_next (list_iter);

                  if (list_iter == NULL)
                    list_iter = image_list;

                  current_path = list_iter->data;

                  if (gtk_tree_path_compare (start_path, current_path) != 0)
                    {
                      new_ref = gtk_tree_row_reference_new (tree_model,
                                                            current_path);
                    }
                }
            }
          for (GSList *list_iter = image_list; list_iter;
               list_iter = g_slist_next (list_iter))
            {
              gtk_tree_path_free (list_iter->data);
            }
          g_slist_free (image_list);
        }
    }
  return new_ref;
}

static void
on_remove_button_clicked (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);

  GtkTreeModel *as_model = GTK_TREE_MODEL (priv->tree_store);
  
  GList *selected_list;
  selected_list = gtk_tree_selection_get_selected_rows (priv->tree_selection,
                                                        &as_model);
  GSList *row_list = NULL;

  gboolean all_valid = TRUE;
  
  GList *list_iter;
  for (list_iter = selected_list; list_iter && all_valid;
       list_iter = g_list_next (list_iter))
    {
      GtkTreePath *tree_path;
      tree_path = list_iter->data;
      
      int depth;
      depth = gtk_tree_path_get_depth (tree_path);

      if (depth > 1)
        {
          all_valid = FALSE;

          GtkWidget *dialog;
          dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                           GTK_DIALOG_MODAL,
                                           GTK_MESSAGE_WARNING,
                                           GTK_BUTTONS_OK,
                                           _("Can only remove items at "
                                             "the top level."));
          gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);
        }
      else
        {
          GtkTreeRowReference *row_ref;
          row_ref = gtk_tree_row_reference_new (as_model, list_iter->data);
          row_list = g_slist_append (row_list, row_ref);
        }
      /* I could free the path here but it's going to iterate again anyways
       * when it's free I think so I'll just do it the way it says online.  */
    }

  g_list_free_full (selected_list, (GDestroyNotify) gtk_tree_path_free);

  GtkTreeIter iter;

  GSList *slist_iter;
  for (slist_iter = row_list; slist_iter;
       slist_iter = g_slist_next (slist_iter))
    {
      if (all_valid)
        {
          GtkTreePath *tree_path;
          tree_path = gtk_tree_row_reference_get_path (slist_iter->data);

          gtk_tree_model_get_iter (as_model, &iter, tree_path);
          gtk_tree_store_remove (priv->tree_store, &iter);

          gtk_tree_path_free (tree_path);
        }

      gtk_tree_row_reference_free (slist_iter->data);
    }

  g_slist_free (row_list);
}

static int
tree_path_sort_func (GtkTreePath *a, GtkTreePath *b)
{
  int *a_indices;
  a_indices = gtk_tree_path_get_indices (a);

  int *b_indices;
  b_indices = gtk_tree_path_get_indices (b);

  /* Returns negative if the first value of a is less than the first value of b
   * and vica versa.  I also don't think I have to free these because the
   * documentation says so.  */
  return (*a_indices - *b_indices);
}

static void
on_up_button_clicked (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeModel *as_model;
  as_model = GTK_TREE_MODEL (priv->tree_store);

  GList *selected_list;
  selected_list = gtk_tree_selection_get_selected_rows (priv->tree_selection,
                                                        &as_model);

  gboolean all_valid = TRUE;
  GList *list_iter;
  for (list_iter = selected_list; list_iter && all_valid;
       list_iter = g_list_next (list_iter))
    {
      int depth;
      depth = gtk_tree_path_get_depth (list_iter->data);

      if (depth > 1)
        {
          all_valid = FALSE;
          GtkWidget *dialog;
          dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                           GTK_DIALOG_MODAL,
                                           GTK_MESSAGE_WARNING,
                                           GTK_BUTTONS_OK,
                                           _("Cannot move child item."));
          gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);
        }
      else
        {
          int *indices;
          indices = gtk_tree_path_get_indices (list_iter->data);

          if (*indices == 0)
            {
              all_valid = FALSE;
              GtkWidget *dialog;
              dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                               GTK_DIALOG_MODAL,
                                               GTK_MESSAGE_WARNING,
                                               GTK_BUTTONS_OK,
                                               _("Cannot move up item that's "
                                                 "first."));
              gtk_dialog_run (GTK_DIALOG (dialog));
              gtk_widget_destroy (dialog);
            }
        }
    }

  if (all_valid)
    {

      selected_list = g_list_sort (selected_list,
                                   (GCompareFunc) tree_path_sort_func);

      GtkTreeIter src;
      GtkTreeIter dest;

      for (list_iter = selected_list; list_iter;
           list_iter = g_list_next (list_iter))
        {
          GtkTreePath *tree_path;
          tree_path = list_iter->data;
          gtk_tree_model_get_iter (as_model, &src, tree_path);
          /* I'm not checking to see if it worked becuase that should've been
           * prevented from with the check from earlier.  */
          gtk_tree_path_prev (tree_path);
          gtk_tree_model_get_iter (as_model, &dest, tree_path);
          gtk_tree_store_move_before (priv->tree_store, &src, &dest);
        }
    }

  g_list_free_full (selected_list, (GDestroyNotify) gtk_tree_path_free);
}

static void
on_down_button_clicked (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeModel *as_model;
  as_model = GTK_TREE_MODEL (priv->tree_store);

  GList *selected_list;
  selected_list = gtk_tree_selection_get_selected_rows (priv->tree_selection,
                                                        &as_model);

  gboolean all_valid = TRUE;
  GList *list_iter;
  for (list_iter = selected_list; list_iter && all_valid;
       list_iter = g_list_next (list_iter))
    {
      int depth;
      depth = gtk_tree_path_get_depth (list_iter->data);

      if (depth > 1)
        {
          all_valid = FALSE;
          GtkWidget *dialog;
          dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                           GTK_DIALOG_MODAL,
                                           GTK_MESSAGE_WARNING,
                                           GTK_BUTTONS_OK,
                                           _("Cannot move child item."));
          gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);
        }
      else
        {
          int *indices;
          indices = gtk_tree_path_get_indices (list_iter->data);

          int length;
          length = gtk_tree_model_iter_n_children (as_model,
                                                   NULL);

          if (*indices >= length - 1)
            {
              all_valid = FALSE;
              GtkWidget *dialog;
              dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                               GTK_DIALOG_MODAL,
                                               GTK_MESSAGE_WARNING,
                                               GTK_BUTTONS_OK,
                                               _("Cannot move down item "
                                                 "that's last."));
              gtk_dialog_run (GTK_DIALOG (dialog));
              gtk_widget_destroy (dialog);
            }
        }
    }

  if (all_valid)
    {

      selected_list = g_list_sort (selected_list,
                                   (GCompareFunc) tree_path_sort_func);
      selected_list = g_list_reverse (selected_list);

      GtkTreeIter src;
      GtkTreeIter dest;

      for (list_iter = selected_list; list_iter;
           list_iter = g_list_next (list_iter))
        {
          GtkTreePath *tree_path;
          tree_path = list_iter->data;
          gtk_tree_model_get_iter (as_model, &src, tree_path);
          /* I'm not checking to see if it worked becuase that should've been
           * prevented from with the check from earlier.  */
          gtk_tree_path_next (tree_path);
          gtk_tree_model_get_iter (as_model, &dest, tree_path);
          gtk_tree_store_move_after (priv->tree_store, &src, &dest);
        }
    }

  g_list_free_full (selected_list, (GDestroyNotify) gtk_tree_path_free);
}

void
jws_config_window_load_file (JwsConfigWindow *win,
                             const gchar *path)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  gboolean status;
  status = jws_info_set_from_file (priv->current_info, path);

  if (status)
    {
      jws_config_window_set_gui_from_info (win);
    }
  else
    {
      GtkWidget *dialog;
      dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                       GTK_DIALOG_MODAL,
                                       GTK_MESSAGE_ERROR,
                                       GTK_BUTTONS_OK,
                                       _("Error loading file %s."),
                                       path);
      gtk_dialog_run (GTK_DIALOG (dialog));
      gtk_widget_destroy (dialog);
    }
}

void
jws_config_window_set_gui_from_info (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  gboolean rotate_image;
  rotate_image = jws_info_get_rotate_image (priv->current_info);

  int rotate_seconds;
  rotate_seconds = jws_info_get_rotate_seconds (priv->current_info);

  gboolean randomize_order;
  randomize_order = jws_info_get_randomize_order (priv->current_info);

  GList *file_list;
  file_list = jws_info_get_file_list (priv->current_info);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->rotate_button),
                                rotate_image);
  jws_config_window_show_rotate_items (win, rotate_image);

  gtk_spin_button_set_value (GTK_SPIN_BUTTON (priv->time_button),
                             rotate_seconds);
  /* 0 should be the index for seconds.  */
  gtk_combo_box_set_active (GTK_COMBO_BOX (priv->time_unit_box), 0);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->randomize_button),
                                randomize_order);

  gtk_tree_store_clear (priv->tree_store);
  GList *iter;
  for (iter = file_list; iter; iter = g_list_next (iter))
    {
      jws_config_window_add_file (win, iter->data);
    }
}

void
jws_config_window_set_info_from_gui (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  gboolean rotate_image;
  rotate_image = gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON (priv->rotate_button));

  int seconds_multiplier = 1;
  gchar *current_text;
  current_text = gtk_combo_box_text_get_active_text
    (GTK_COMBO_BOX_TEXT (priv->time_unit_box));

  if (g_str_equal (current_text, "Seconds"))
    {
      seconds_multiplier = 1;
    }
  else if (g_str_equal (current_text, "Minutes"))
    {
      seconds_multiplier = 60;
    }
  else if (g_str_equal (current_text, "Hours"))
    {
      seconds_multiplier = 60 * 60;
    }

  int time_value;
  time_value = gtk_spin_button_get_value_as_int
    (GTK_SPIN_BUTTON (priv->time_button));

  int rotate_seconds;
  rotate_seconds = time_value * seconds_multiplier;

  gboolean randomize_order;
  randomize_order = gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON (priv->randomize_button));

  GtkTreeModel *as_model;
  as_model = GTK_TREE_MODEL (priv->tree_store);

  GList *file_list = NULL;
  GtkTreeIter iter;
  gboolean is_valid;
  is_valid = gtk_tree_model_get_iter_first (as_model, &iter);

  for (; is_valid; is_valid = gtk_tree_model_iter_next (as_model, &iter))
    {
      gchar *path;
      gtk_tree_model_get (as_model, &iter,
                          PATH_COLUMN, &path,
                          -1);
      file_list = g_list_append (file_list, path);
    }
  
  jws_info_set_rotate_image (priv->current_info, rotate_image);
  jws_info_set_rotate_seconds (priv->current_info, rotate_seconds);
  jws_info_set_randomize_order (priv->current_info, rotate_seconds);
  jws_info_set_file_list (priv->current_info, file_list);
}

static void
open_activated (GSimpleAction *action,
                GVariant *parameter,
                gpointer win)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (_("Choose File"),
                                        GTK_WINDOW (win),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        _("Open"),
                                        GTK_RESPONSE_ACCEPT,
                                        NULL);
  gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (dialog), FALSE);
  gchar *home_dir;
  home_dir = get_home_directory ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), home_dir);
  g_free (home_dir);

  int response;
  response = gtk_dialog_run (GTK_DIALOG (dialog));

  gchar *path = NULL;
  
  if (response == GTK_RESPONSE_ACCEPT)
    {
      path = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    }

  if (path)
    {
      jws_config_window_load_file (JWS_CONFIG_WINDOW (win), path);
    }
  g_free (path);

  gtk_widget_destroy (dialog);
}

static void
save_activated (GSimpleAction *action,
                GVariant *parameter,
                gpointer win)
{
  gchar *config_path;
  config_path = jws_get_default_config_file ();
  jws_config_window_save_to_file (JWS_CONFIG_WINDOW (win), config_path);
  g_free (config_path);
}

static void
save_as_activated (GSimpleAction *action,
                   GVariant *parameter,
                   gpointer win)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (_("Save as"),
                                        GTK_WINDOW (win),
                                        GTK_FILE_CHOOSER_ACTION_SAVE,
                                        _("Save"), GTK_RESPONSE_ACCEPT,
                                        NULL);

  gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (dialog), FALSE);

  gchar *home_dir;
  home_dir = get_home_directory ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), home_dir);
  g_free (home_dir);

  int response;
  response = gtk_dialog_run (GTK_DIALOG (dialog));

  gchar *path = NULL;

  if (response == GTK_RESPONSE_ACCEPT)
    {
      path = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    }

  gtk_widget_destroy (dialog);

  if (path)
    {
      jws_config_window_save_to_file (JWS_CONFIG_WINDOW (win), path);
      g_free (path);
    }
}

static void
quit_activated (GSimpleAction *action,
                GVariant *parameter,
                gpointer win)
{
  gtk_widget_destroy (win);
}

static void
about_activated (GSimpleAction *action,
       GVariant *parameter,
       gpointer win)
{
  const gchar *authors[] = 
    {
      "Jason Waataja",
      NULL
    };

  const gchar *license = 
    _("This program is free software: you can redistribute it and/or modify\n"
      "it under the terms of the GNU General Public License as published by\n"
      "the Free Software Foundation, either version 3 of the License, or\n"
      "(at your option) any later version.\n"
      "\n"
      "This program is distributed in the hope that it will be useful,\n"
      "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
      "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
      "GNU General Public License for more details.\n"
      "\n"
      "You should have received a copy of the GNU General Public License\n"
      "along with this program.  " /* I broke here to stay under 80.  */
      "If not, see <http://www.gnu.org/licenses/>.");

  gtk_show_about_dialog (GTK_WINDOW (win),
                         "program-name", "JWS-Config",
                         "title", _("About JWS-Config"),
                         "authors", authors,
                         "copyright", "Copyright (C) 2016 Jason Waataja",
                         "license", license,
                         NULL);

}

gboolean
jws_config_window_check_gui_consistency (JwsConfigWindow *win)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  int time_value;
  time_value = gtk_spin_button_get_value_as_int
    (GTK_SPIN_BUTTON (priv->time_button));

  gboolean rotate_image;
  rotate_image = gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON (priv->rotate_button));

  if (rotate_image && time_value <= 0)
    {
      GtkWidget *dialog;
      dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                       GTK_DIALOG_MODAL,
                                       GTK_MESSAGE_WARNING,
                                       GTK_BUTTONS_OK,
                                       _("Error, time is non-positive."));
      gtk_dialog_run (GTK_DIALOG (dialog));
      gtk_widget_destroy (dialog);
      return FALSE;
    }

  int file_count;
  file_count = gtk_tree_model_iter_n_children
    (GTK_TREE_MODEL (priv->tree_store),
     NULL);

  if (file_count <= 0)
    {
      GtkWidget *dialog;
      dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                       GTK_DIALOG_MODAL,
                                       GTK_MESSAGE_WARNING,
                                       GTK_BUTTONS_OK,
                                       _("Error, you must select a file."));
      gtk_dialog_run (GTK_DIALOG (dialog));
      gtk_widget_destroy (dialog);
      return FALSE;
    }

  if (!rotate_image && file_count > 1)
    {
      GtkWidget *dialog;
      dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                       GTK_DIALOG_MODAL,
                                       GTK_MESSAGE_WARNING,
                                       GTK_BUTTONS_OK,
                                       _("Warning, you don't have rotate "
                                         "image selected but multiple files "
                                         "are selected. Only the first one "
                                         "will be used."));
      gtk_dialog_run (GTK_DIALOG (dialog));
      gtk_widget_destroy (dialog);
      return FALSE;
    }

  return TRUE;
}

void
jws_config_window_save_to_file (JwsConfigWindow *win, const char *path)
{
  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  

  gboolean is_valid;
  is_valid = jws_config_window_check_gui_consistency (win);

  if (is_valid)
    {
      jws_config_window_set_info_from_gui (win);
      gboolean status;
      status = jws_info_write_to_file (priv->current_info, path);

      if (!status)
        {
          GtkWidget *dialog;
          dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                           GTK_DIALOG_MODAL,
                                           GTK_MESSAGE_ERROR,
                                           GTK_BUTTONS_OK,
                                           _("Error writing file to %s.\n"),
                                           path);
          gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);
        }
      else
        {
          GtkWidget *dialog;
          dialog = gtk_message_dialog_new (GTK_WINDOW (win),
                                           GTK_DIALOG_MODAL,
                                           GTK_MESSAGE_INFO,
                                           GTK_BUTTONS_OK,
                                           _("Wrote to file %s.\n"),
                                           path);
          gtk_dialog_run (GTK_DIALOG (dialog));
          gtk_widget_destroy (dialog);
        }
    }
}

static gboolean
on_tree_view_button_press (GtkWidget *tree_view,
                           GdkEvent *event,
                           gpointer win)
{
  if (event->type != GDK_BUTTON_PRESS)
    return FALSE;

  GdkEventButton *button_event;
  button_event = (GdkEventButton *) event;

  if (button_event->button != GDK_BUTTON_SECONDARY)
      return FALSE;

  GtkTreePath *tree_path;
  GtkTreeViewColumn *tree_column;

  gboolean row_exists;
  row_exists = gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (tree_view),
                                              button_event->x,
                                              button_event->y,
                                              &tree_path,
                                              &tree_column,
                                              NULL,
                                              NULL);

  if (!row_exists || tree_path == NULL || tree_column == NULL)
    {
      gtk_tree_path_free (tree_path);
      return FALSE;
    }

  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (JWS_CONFIG_WINDOW (win));

  GtkTreeModel *model;
  model = GTK_TREE_MODEL (priv->tree_store);

  GtkTreeRowReference *row_ref;
  row_ref = gtk_tree_row_reference_new (model,
                                        tree_path);
  GtkTreeIter iter;
  gtk_tree_model_get_iter (model, &iter, tree_path);

  GtkWidget *item_menu;
  item_menu = gtk_menu_new ();

  int depth;
  depth = gtk_tree_path_get_depth (tree_path);

  gboolean is_directory;
  gtk_tree_model_get (model, &iter,
                      IS_DIRECTORY_COLUMN, &is_directory,
                      -1);

  int item_count = 0;

  if (!is_directory)
    {
      GtkWidget *open_item;
      open_item = gtk_menu_item_new_with_label (_("Open"));
      WindowRowEntry *open_entry = g_new (WindowRowEntry, 1);
      open_entry->win = win;
      open_entry->row_ref = gtk_tree_row_reference_copy (row_ref);
      g_signal_connect_data (open_item, "activate",
                             G_CALLBACK (on_open_menu_activated),
                             open_entry,
                             (GClosureNotify) window_row_entry_free,
                             G_CONNECT_SWAPPED);
      gtk_menu_shell_append (GTK_MENU_SHELL (item_menu), open_item);
      item_count++;

      GtkWidget *set_item;
      set_item = gtk_menu_item_new_with_label (_("Try as wallpaper"));
      WindowRowEntry *set_entry = g_new (WindowRowEntry, 1);
      set_entry->win = win;
      set_entry->row_ref = gtk_tree_row_reference_copy (row_ref);
      g_signal_connect_data (set_item, "activate",
                             G_CALLBACK (on_set_menu_activated),
                             set_entry,
                             (GClosureNotify) window_row_entry_free,
                             G_CONNECT_SWAPPED);
      gtk_menu_shell_append (GTK_MENU_SHELL (item_menu), set_item);
      item_count++;
    }

  if (depth == 1)
    {
      WindowRowEntry *remove_entry;
      remove_entry = g_new (WindowRowEntry, 1);
      remove_entry->win = win;
      remove_entry->row_ref = gtk_tree_row_reference_copy (row_ref);

      GtkWidget *remove_item;
      remove_item = gtk_menu_item_new_with_label (_("Remove"));
      g_signal_connect_data (remove_item, "activate",
                             G_CALLBACK (on_remove_menu_activated),
                             remove_entry,
                             (GClosureNotify) window_row_entry_free,
                             G_CONNECT_SWAPPED);
      gtk_menu_shell_append (GTK_MENU_SHELL (item_menu), remove_item);
      item_count++;

      gint *indices;
      indices = gtk_tree_path_get_indices (tree_path);

      int position;
      position = indices[0];

      if (position > 0)
        {
          WindowRowEntry *up_entry;
          up_entry = g_new (WindowRowEntry, 1);
          up_entry->win = win;
          up_entry->row_ref = gtk_tree_row_reference_copy (row_ref);
          GtkWidget *up_item;
          up_item = gtk_menu_item_new_with_label (_("Move up"));
          g_signal_connect_data (up_item, "activate",
                                 G_CALLBACK (on_up_menu_activated),
                                 up_entry,
                                 (GClosureNotify) window_row_entry_free,
                                 G_CONNECT_SWAPPED);
          gtk_menu_shell_append (GTK_MENU_SHELL (item_menu), up_item);
          item_count++;
        }

      int child_count;
      child_count = gtk_tree_model_iter_n_children (model, NULL);

      if (position < child_count - 1)
        {
          WindowRowEntry *down_entry;
          down_entry = g_new (WindowRowEntry, 1);
          down_entry->win = win;
          down_entry->row_ref = gtk_tree_row_reference_copy (row_ref);
          GtkWidget *down_item;
          down_item = gtk_menu_item_new_with_label (_("Move down"));
          g_signal_connect_data (down_item, "activate",
                                 G_CALLBACK (on_down_menu_activated),
                                 down_entry,
                                 (GClosureNotify) window_row_entry_free,
                                 G_CONNECT_SWAPPED);
          gtk_menu_shell_append (GTK_MENU_SHELL (item_menu), down_item);
          item_count++;
        }
    }
  gtk_tree_path_free (tree_path);
  gtk_tree_row_reference_free (row_ref);

  if (item_count > 0)
    {
      gtk_widget_show_all (item_menu);

      gtk_menu_popup (GTK_MENU (item_menu),
                      NULL,
                      NULL,
                      NULL,
                      NULL,
                      button_event->button,
                      button_event->time);
    }

  return FALSE;
}

void
jws_config_window_move_row_up (JwsConfigWindow *win,
                               GtkTreeRowReference *row_ref)
{
  if (!gtk_tree_row_reference_valid (row_ref))
    return;

  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeModel *model;
  model = GTK_TREE_MODEL (priv->tree_store);

  GtkTreeIter start_iter;
  GtkTreeIter new_iter;

  GtkTreePath *tree_path;
  tree_path = gtk_tree_row_reference_get_path (row_ref);

  gtk_tree_model_get_iter (model, &start_iter, tree_path);

  gboolean move_valid;
  move_valid = gtk_tree_path_prev (tree_path);

  if (move_valid)
    {
      gtk_tree_model_get_iter (model, &new_iter, tree_path);
      gtk_tree_store_move_before (priv->tree_store, &start_iter, &new_iter);
    }
  gtk_tree_path_free (tree_path);
}

void
jws_config_window_move_row_down (JwsConfigWindow *win,
                                 GtkTreeRowReference *row_ref)
{
  if (!gtk_tree_row_reference_valid (row_ref))
    return;

  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeModel *model;
  model = GTK_TREE_MODEL (priv->tree_store);

  GtkTreeIter start_iter;
  GtkTreeIter new_iter;
  GtkTreeIter parent_iter;

  GtkTreePath *tree_path;
  tree_path = gtk_tree_row_reference_get_path (row_ref);

  gtk_tree_model_get_iter (model, &start_iter, tree_path);

  int depth;
  depth = gtk_tree_path_get_depth (tree_path);

  gboolean is_valid = TRUE;
  int child_count = 0;

  if (depth == 1)
    {
      child_count = gtk_tree_model_iter_n_children (model, NULL);
    }
  else
    {
      gtk_tree_model_iter_parent (model, &parent_iter, &start_iter);
      child_count = gtk_tree_model_iter_n_children (model, &parent_iter);
    }

  int position;
  position = gtk_tree_path_get_indices (tree_path)[depth - 1];

  if (position >= child_count - 1)
    is_valid = FALSE;

  if (is_valid)
    {
      gtk_tree_path_next (tree_path);
      gtk_tree_model_get_iter (model, &new_iter, tree_path);

      gtk_tree_store_move_after (priv->tree_store, &start_iter, &new_iter);
    }
  
  gtk_tree_path_free (tree_path);
}

void
jws_config_window_remove_row (JwsConfigWindow *win,
                              GtkTreeRowReference *row_ref)
{
  if (!gtk_tree_row_reference_valid (row_ref))
    return;

  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreePath *tree_path;
  tree_path = gtk_tree_row_reference_get_path (row_ref);

  GtkTreeIter iter;
  gtk_tree_model_get_iter (GTK_TREE_MODEL (priv->tree_store), &iter,
                           tree_path);

  gtk_tree_store_remove (priv->tree_store, &iter);

  gtk_tree_path_free (tree_path);
}

static void
on_open_menu_activated (WindowRowEntry *entry)
{
  jws_config_window_show_image_for_row (entry->win, entry->row_ref);
}

static void
on_up_menu_activated (WindowRowEntry *entry)
{
  jws_config_window_move_row_up (entry->win, entry->row_ref);
}

static void
on_down_menu_activated (WindowRowEntry *entry)
{
  jws_config_window_move_row_down (entry->win, entry->row_ref);
}

static void
on_remove_menu_activated (WindowRowEntry *entry)
{
  jws_config_window_remove_row (entry->win, entry->row_ref);
}

static void
on_set_menu_activated (WindowRowEntry *entry)
{
  jws_config_window_set_wallpaper_for_row (entry->win,
                                           entry->row_ref);
}

static void
window_row_entry_free (WindowRowEntry *entry)
{
  gtk_tree_row_reference_free (entry->row_ref);
  g_free (entry);
}

void
jws_config_window_set_wallpaper_for_row (JwsConfigWindow *win,
                                         GtkTreeRowReference *row_ref)
{
  if (!gtk_tree_row_reference_valid (row_ref))
    return;

  JwsConfigWindowPrivate *priv;
  priv = jws_config_window_get_instance_private (win);
  
  GtkTreeModel *model;
  model = GTK_TREE_MODEL (priv->tree_store);

  GtkTreePath *tree_path;
  tree_path = gtk_tree_row_reference_get_path (row_ref);

  GtkTreeIter iter;
  gtk_tree_model_get_iter (model, &iter, tree_path);

  gchar *path = NULL;
  gtk_tree_model_get (model, &iter,
                      PATH_COLUMN, &path,
                      -1);

  jws_set_wallpaper_from_file (path);

  g_free (path);
}
