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
  unsigned long pageid;
  int progress, fullscreen, https, insecure, errorpage;
  const char *title, *overtitle, *targeturi;
  const char *needle;
  //int x, y, w, h;
} Client;

//static Client *c;
static GtkFixed *fixed;

/*
static gboolean closeWebViewCb(WebKitWebView* webView, GtkWidget* window);

static gboolean closeWebViewCb(WebKitWebView* webView, GtkWidget* window)
{
    gtk_widget_destroy(window);
    return TRUE;
}
*/

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

GtkFixed *find_fixed_widget(GList *widgets)
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

static emacs_value
webkitgtk_load_uri(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);

  ptrdiff_t MAX_LEN = 256;
  char buf[MAX_LEN];
  env->copy_string_contents (env, args[1], buf, &MAX_LEN);
  printf("loading %p uri: %s\n", c, buf);

  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    webkit_web_view_load_uri(c->view, buf);
  return Qnil;
}

static emacs_value
webkitgtk_hide(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  printf("hiding %p\n", c);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    gtk_widget_hide(GTK_WIDGET(c->view));
  return Qnil;
}

static emacs_value
webkitgtk_show(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  printf("showing %p\n", c);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    gtk_widget_show(GTK_WIDGET(c->view));
  return Qnil;
}

static emacs_value
webkitgtk_focus(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  printf("focusing %p\n", c);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    gtk_widget_grab_focus(GTK_WIDGET(c->view));
  return Qnil;
}

static emacs_value
webkitgtk_unfocus(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  printf("unfocusing %p\n", c);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    gtk_widget_grab_focus(GTK_WIDGET(fixed));
  return Qnil;
}

static emacs_value
webkitgtk_resize(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  int x = env->extract_integer(env, args[1]);
  int y = env->extract_integer(env, args[2]);
  int w = env->extract_integer(env, args[3]);
  int h = env->extract_integer(env, args[4]);

  printf("resize %p to (x:%d y:%d w:%d h:%d)\n", c, x, y, w, h);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    {
      gtk_fixed_move(fixed, GTK_WIDGET(c->view), x, y);
      gtk_widget_set_size_request(GTK_WIDGET(c->view), w, h);
    }
  return Qnil;
}

//static emacs_value webkitgtk_destroy(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
void
webkitgtk_destroy(void *ptr)
{
  printf("destroying %p\n", ptr);
  //gtk_widget_destroy(GTK_WIDGET(c->view));
  Client *c = (Client *)ptr;
  gtk_container_remove(GTK_CONTAINER(fixed), GTK_WIDGET(c->view));

  free(c);
}

static emacs_value
webkitgtk_new(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c;
  if (!(c = calloc(1, sizeof(Client))))
    {
      printf("Cannot malloc!\n");
      return Qnil;
    }

  //gtk_init_check(&argc, &argv);
  c->view = WEBKIT_WEB_VIEW(webkit_web_view_new());
  gtk_fixed_put(fixed, GTK_WIDGET(c->view), 0, 0);
  gtk_widget_show_all(GTK_WIDGET(c->view));
  //gtk_widget_set_focus_on_click (GTK_WIDGET(c->view), FALSE);
  //g_signal_connect(c->view, "close", G_CALLBACK(close_web_view_cb), main_window);

  return env->make_user_ptr(env, webkitgtk_destroy, (void *)c);
}

void bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  env->funcall(env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}

int
emacs_module_init(struct emacs_runtime *ert)
{
  print_widgets(gtk_window_list_toplevels());

  fixed = find_fixed_widget(gtk_window_list_toplevels());
  if (fixed == NULL)
    {
      printf("counldn't find fixed widget!\n");
      return 1;
    }

  emacs_env *env = ert->get_environment(ert);

  // Symbols
  Qt = env->make_global_ref(env, env->intern(env, "t"));
  Qnil = env->make_global_ref(env, env->intern(env, "nil"));
  emacs_value fun;
  fun = env->make_function(env, 0, 0, webkitgtk_new, "", NULL);
  bind_function(env, "webkitgtk-new", fun);

  fun = env->make_function(env, 5, 5, webkitgtk_resize, "", NULL);
  bind_function(env, "webkitgtk-resize", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_hide, "", NULL);
  bind_function(env, "webkitgtk-hide", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_show, "", NULL);
  bind_function(env, "webkitgtk-show", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_focus, "", NULL);
  bind_function(env, "webkitgtk-focus", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_unfocus, "", NULL);
  bind_function(env, "webkitgtk-unfocus", fun);

  fun = env->make_function(env, 2, 2, webkitgtk_load_uri, "", NULL);
  bind_function(env, "webkitgtk-load-uri", fun);

  emacs_value Qfeat = env->intern(env, "webkitgtk-module");
  emacs_value Qprovide = env->intern(env, "provide");
  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
  return 0;
}

