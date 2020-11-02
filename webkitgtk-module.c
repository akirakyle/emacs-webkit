#include <gtk/gtk.h>
#include <webkit2/webkit2.h>
#include <assert.h>
#include <errno.h>

#include "emacs-module.h"

int plugin_is_GPL_compatible;

/* Frequently-used symbols. */
static emacs_value Qnil;
static emacs_value Qt;

typedef struct Client {
  GtkWidget *win;
  WebKitWebView *view;
} Client;

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

static bool
copy_string_contents (emacs_env *env, emacs_value value,
                      char **buffer, size_t *size)
{
  ptrdiff_t buffer_size;
  if (!env->copy_string_contents (env, value, NULL, &buffer_size))
    return false;
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  assert (buffer_size > 0);
  *buffer = malloc ((size_t) buffer_size);
  if (*buffer == NULL)
    {
      env->non_local_exit_signal (env, env->intern (env, "memory-full"),
                                  env->intern (env, "nil"));
      return false;
    }
  ptrdiff_t old_buffer_size = buffer_size;
  if (!env->copy_string_contents (env, value, *buffer, &buffer_size))
    {
      free (*buffer);
      *buffer = NULL;
      return false;
    }
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  assert (buffer_size == old_buffer_size);
  *size = (size_t) (buffer_size - 1);
  return true;
}

ssize_t rio_writen(int fd, void *usrbuf, size_t n) 
{
    size_t nleft = n;
    ssize_t nwritten;
    char *bufp = usrbuf;

    while (nleft > 0) {
	if ((nwritten = write(fd, bufp, nleft)) <= 0) {
	    if (errno == EINTR)  /* Interrupted by sig handler return */
		nwritten = 0;    /* and call write() again */
	    else
		return -1;       /* errno set by write() */
	}
	nleft -= nwritten;
	bufp += nwritten;
    }
    return n;
}

static void
webkitgtk_js_finished(GObject *web_view, GAsyncResult *result, gpointer arg)
{
  GError *error = NULL;
  int fd = (ptrdiff_t) arg;

  WebKitJavascriptResult *js_result =
    webkit_web_view_run_javascript_finish(WEBKIT_WEB_VIEW(web_view),
                                          result, &error);

  if (!js_result)
    {
      g_warning ("Error running javascript: %s", error->message);
      g_error_free (error);
      return;
    }

  JSCValue *value = webkit_javascript_result_get_js_value (js_result);
  if (!jsc_value_is_string(value))
    {
      g_warning ("Error running javascript: return value was not a string");
      webkit_javascript_result_unref (js_result);
      return;
    }

  gchar *str_value = jsc_value_to_string(value);
  JSCException *exception = jsc_context_get_exception(jsc_value_get_context(value));
  if (exception)
    {
    g_warning ("Error running javascript: %s", jsc_exception_get_message (exception));
    webkit_javascript_result_unref (js_result);
    return;
    }

  printf("Script result: %s\n", str_value);
  if (rio_writen(fd, str_value, strlen(str_value)+1) < 0)
    g_warning ("Error writing javascript result to fd: %d", fd);

  g_free (str_value);
  webkit_javascript_result_unref (js_result);
}

static emacs_value
webkitgtk_execute_js(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  size_t size;
  char *script = NULL;
  if (copy_string_contents(env, args[1], &script, &size))
    {
      if (n == 3)
        {
          ptrdiff_t fd = env->open_channel(env, args[2]);
          webkit_web_view_run_javascript(c->view, script, NULL,
                                         webkitgtk_js_finished, (gpointer)fd);
        }
      else {
        webkit_web_view_run_javascript(c->view, script, NULL, NULL, NULL);
      }
    }
  printf("executing %p script: %s\n", c, script);
  free(script);
  return Qnil;
}

static emacs_value
webkitgtk_set_zoom(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  double zoom = env->extract_float(env, args[1]);
  printf("zoom %p to: %f\n", c, zoom);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    webkit_web_view_set_zoom_level(c->view, (gdouble)zoom);
  return Qnil;
}

static emacs_value
webkitgtk_get_zoom(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    {
      gdouble zoom = webkit_web_view_get_zoom_level(c->view);
      printf("zoom %p to: %f\n", c, zoom);
      return env->make_float(env, (double)zoom);
    }
  return Qnil;
}

static emacs_value
webkitgtk_forward(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    webkit_web_view_go_forward(c->view);
  return Qnil;
}

static emacs_value
webkitgtk_back(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    webkit_web_view_go_back(c->view);
  return Qnil;
}

static emacs_value
webkitgtk_reload(emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = (Client *)env->get_user_ptr(env, args[0]);
  if (env->non_local_exit_check(env) == emacs_funcall_exit_return)
    webkit_web_view_reload(c->view);
  return Qnil;
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

gboolean
key_press_event(GtkWidget *w, GdkEvent *e, Client *c)
{
  printf("key_press_event: %p\n", c);
  switch (e->type) {
  case GDK_KEY_PRESS:
    printf("key.keyval = %d\n", e->key.keyval);
    if (e->key.keyval == GDK_KEY_Escape && e->key.state == 0)
      {
        gtk_widget_grab_focus(GTK_WIDGET(fixed));
        return TRUE;
      }
  default:
    break;
  }
  return FALSE;
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
  /*
  printf("can focus %d\n", gtk_widget_get_can_focus(GTK_WIDGET(c->view)));
  gtk_widget_set_can_focus(GTK_WIDGET(c->view), FALSE);
  printf("can focus %d\n", gtk_widget_get_can_focus(GTK_WIDGET(c->view)));
  printf("has window %d\n", gtk_widget_get_has_window(GTK_WIDGET(c->view)));
  */
  //gtk_widget_set_focus_on_click (GTK_WIDGET(c->view), FALSE);
  //g_signal_connect(c->view, "close", G_CALLBACK(close_web_view_cb), main_window);
  g_signal_connect(G_OBJECT(c->view), "key-press-event",
                   G_CALLBACK(key_press_event), c);

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
  bind_function(env, "webkitgtk--new", fun);

  fun = env->make_function(env, 5, 5, webkitgtk_resize, "", NULL);
  bind_function(env, "webkitgtk--resize", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_hide, "", NULL);
  bind_function(env, "webkitgtk--hide", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_show, "", NULL);
  bind_function(env, "webkitgtk--show", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_focus, "", NULL);
  bind_function(env, "webkitgtk--focus", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_unfocus, "", NULL);
  bind_function(env, "webkitgtk--unfocus", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_forward, "", NULL);
  bind_function(env, "webkitgtk--forward", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_back, "", NULL);
  bind_function(env, "webkitgtk--back", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_reload, "", NULL);
  bind_function(env, "webkitgtk--reload", fun);

  fun = env->make_function(env, 2, 2, webkitgtk_load_uri, "", NULL);
  bind_function(env, "webkitgtk--load-uri", fun);

  fun = env->make_function(env, 2, 3, webkitgtk_execute_js, "", NULL);
  bind_function(env, "webkitgtk--execute-js", fun);

  fun = env->make_function(env, 2, 2, webkitgtk_set_zoom, "", NULL);
  bind_function(env, "webkitgtk--set-zoom", fun);

  fun = env->make_function(env, 1, 1, webkitgtk_get_zoom, "", NULL);
  bind_function(env, "webkitgtk--get-zoom", fun);

  emacs_value Qfeat = env->intern(env, "webkitgtk-module");
  emacs_value Qprovide = env->intern(env, "provide");
  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
  return 0;
}

