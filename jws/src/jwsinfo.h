

#ifndef JWSINFO_H
#define JWSINFO_H

#include <glib-object.h>

#define JWS_TYPE_INFO (jws_info_get_type ())
#define JWS_INFO(obj) (G_CHECK_INSTANCE_CAST ((obj), JWS_TYPE_INFO, JwsInfo))

typedef struct _JwsInfo JwsInfo;
typedef struct _JwsInfoClass JwsInfoClass;

GType
jws_info_get_type ();

JwsInfo *
jws_info_new ();

JwsInfo *
jws_info_new_from_file (gchar *path);

#endif
