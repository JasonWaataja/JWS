

#ifndef JWSINFO_H
#define JWSINFO_H

#include <glib-object.h>

#define JWS_TYPE_INFO (jws_info_get_type ())
#define JWS_INFO(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), JWS_TYPE_INFO, JwsInfo))

typedef struct _JwsInfo JwsInfo;
typedef struct _JwsInfoClass JwsInfoClass;

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
