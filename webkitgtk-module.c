#include <gtk/gtk.h>
#include <webkit2/webkit2.h>

#include "emacs-module.h"

int plugin_is_GPL_compatible;

/* Frequently-used symbols. */
static emacs_value Qnil;
static emacs_value Qt;

typedef struct Client {
	GtkWidget *win;
	WebKitWebView *view;
	WebKitWebInspector *inspector;
	WebKitFindController *finder;
	WebKitHitTestResult *mousepos;
	GTlsCertificate *cert, *failedcert;
	GTlsCertificateFlags tlserr;
  //	Window xid;
	unsigned long pageid;
	int progress, fullscreen, https, insecure, errorpage;
	const char *title, *overtitle, *targeturi;
	const char *needle;
	struct Client *next;
} Client;

static Client *c;

static gboolean closeWebViewCb(WebKitWebView* webView, GtkWidget* window);

static gboolean closeWebViewCb(WebKitWebView* webView, GtkWidget* window)
{
    gtk_widget_destroy(window);
    return TRUE;
}

static emacs_value
webkitgtk_load_uri(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  /*
    (void)ptr;
    (void)n;
    int id = env->extract_integer(env, args[0]);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
        return nil;

  */
  int MAX_LEN = 256;
  char buf[MAX_LEN];
  env->copy_string_contents (env, args[0], buf, &MAX_LEN);

  webkit_web_view_load_uri(c->view, buf);

  return Qnil;
}


void bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  env->funcall(env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}

void print_widgets(GList *widgets)
{
  for (GList *l = widgets; l != NULL; l = l->next)
    {
      char *path = gtk_widget_path_to_string(gtk_widget_get_path(l->data));
      printf("widget %s\n", path);
      if (GTK_IS_FIXED(l->data))
        printf("found fixed!\n");
      if (GTK_IS_WINDOW(l->data))
        {
          printf("  window: %s, focus: %d\n", gtk_window_get_title(l->data),
                 gtk_window_has_toplevel_focus(l->data));
          print_widgets(gtk_container_get_children(GTK_CONTAINER(l->data)));
        }
      else if (GTK_IS_CONTAINER(l->data))
        {
          printf("  container\n");
          print_widgets(gtk_container_get_children(GTK_CONTAINER(l->data)));
        }
    }
}

GtkWidget *find_fixed_widget(GList *widgets)
{
  for (GList *l = widgets; l != NULL; l = l->next)
    {
      if (GTK_IS_FIXED(l->data))
        return l->data;
      if (GTK_IS_CONTAINER(l->data))
        return find_fixed_widget(gtk_container_get_children(GTK_CONTAINER(l->data)));
    }
  return NULL;
}

int
emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);

  // Symbols;
  Qt = env->make_global_ref(env, env->intern(env, "t"));
  Qnil = env->make_global_ref(env, env->intern(env, "nil"));
  emacs_value fun;
  fun = env->make_function(env, 1, 1, webkitgtk_load_uri,
                           "load uri", NULL);
  bind_function(env, "webkitgtk-load-uri", fun);

  if (!(c = calloc(1, sizeof(Client))))
    {
      printf("Cannot malloc!\n");
      return 1;
    }
  //gtk_init_check(&argc, &argv);

  print_widgets(gtk_window_list_toplevels());

  // Create an 800x600 window that will contain the browser instance
  //GtkWidget *main_window = gtk_window_new(GTK_WINDOW_POPUP);
  //gtk_window_set_default_size(GTK_WINDOW(main_window), 800, 600);

  // Create a browser instance
  //WebKitWebView *web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());
  c->view = WEBKIT_WEB_VIEW(webkit_web_view_new());

  gtk_widget_set_size_request(GTK_WIDGET(c->view), 200, 200);
  // Put the browser area into the main window
  //gtk_container_add(GTK_CONTAINER(main_window), GTK_WIDGET(c->view));
  GtkWidget *fixed = find_fixed_widget(gtk_window_list_toplevels());
  if (fixed == NULL)
    {
      printf("counldn't find fixed widget!\n");
      return 1;
    }
  gtk_fixed_put(GTK_FIXED(fixed), GTK_WIDGET(c->view), 200, 200);

  // Set up callbacks so that if either the main window or the browser instance is
  // closed, the program will exit
  //g_signal_connect(main_window, "destroy", G_CALLBACK(destroyWindowCb), NULL);
  //g_signal_connect(c->view, "close", G_CALLBACK(closeWebViewCb), main_window);

  // Load a web page into the browser instance
  webkit_web_view_load_uri(c->view, "http://www.webkitgtk.org/");

  // Make sure that when the browser area becomes visible, it will get mouse
  // and keyboard events
  //gtk_widget_grab_focus(GTK_WIDGET(c->view));

  // Make sure the main window and all its contents are visible
  //gtk_widget_show_all(main_window);
  gtk_widget_show_all(GTK_WIDGET(c->view));

  // Run the main GTK+ event loop
  //gtk_main();

  emacs_value Qfeat = env->intern(env, "webkitgtk-module");
  emacs_value Qprovide = env->intern(env, "provide");
  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
  return 0;
}

