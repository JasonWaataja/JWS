
AM_CFLAGS = $(GTK_CFLAGS)
bin_PROGRAMS = jws-config
jws_config_SOURCES = main.c jwsconfigapplication.c jwsconfigwindow.c resources.c jwsconfigimageviewer.c jwsinfo.c
jws_config_LDADD = $(GTK_LIBS)

BUILT_SOURCES = resources.c

resources.c: $(srcdir)/resources/jwsconfig.gresource.xml $(srcdir)/resources/ui/jwswindow.ui $(srcdir)/resources/ui/imageviewer.ui
	$(AM_V_GEN) $(GLIB_COMPILE_RESOURCES) \
	$(srcdir)/resources/jwsconfig.gresource.xml \
		--target=$@ --sourcedir=$(srcdir)/resources --generate-source
