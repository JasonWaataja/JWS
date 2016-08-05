

#ifndef JWSAPPLICATION_H
#define JWSAPPLICATION_H

#include <glib-object.h>

#define JWS_TYPE_APPLICATION (jws_application_get_type ())
#define JWS_APPLICATION(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                                          JWS_TYPE_APPLICATION \
                                                          JwsApplication))

typedef struct _JwsApplication JwsApplication;
typedef struct _JwsApplicationClass JwsApplicationClass;

GType
jws_application_get_type (void);

JwsApplication *
jws_application_new (void);

#endif
