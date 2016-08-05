

#include "jwsapplication.h"

#include <glib.h>
#include <gio/gio.h>

struct _JwsApplication
{
  GApplication parent;
};

struct _JwsApplicationClass
{
  GApplicationClass parent_class;
};

G_DEFINE_TYPE (JwsApplication, jws_application, G_TYPE_APPLICATION);

static void
jws_application_init (JwsApplication *self)
{
}

static void
jws_application_activate (GApplication *app)
{
  g_print ("JwsApplication\n");
}

static void
jws_application_class_init (JwsApplicationClass *kclass)
{
  G_APPLICATION_CLASS (kclass)->activate = jws_application_activate;
}

JwsApplication *
jws_application_new ()
{
  return g_object_new (JWS_TYPE_APPLICATION,
                       "application-id", "com.waataja.jws",
                       NULL);
}
