#define _POSIX_SOURCE 1

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>
#include <stdbool.h>
#include <signal.h>
#include <assert.h>
#include <errno.h>

#include "emacs-module.h"

int plugin_is_GPL_compatible;

/* Frequently-used symbols. */
static emacs_value Qnil;
static emacs_value Qt;

typedef struct Client {
  GtkWidget *container;
  WebKitWebView *view;
  int fd;
} Client;

typedef struct Callback {
  Client *c;
  char *id;
} Callback;

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

Client *
get_client (emacs_env *env, emacs_value value)
{
  Client *c = (Client *)env->get_user_ptr(env, value);
  if ((env->non_local_exit_check(env) == emacs_funcall_exit_return)
      && c->container != NULL)
    return c;
  return NULL;
}

static emacs_value
webkit_set_zoom (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  double zoom = env->extract_float (env, args[1]);
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    webkit_web_view_set_zoom_level (c->view, (gdouble)zoom);
  return Qnil;
}

static emacs_value
webkit_get_zoom (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      gdouble zoom = webkit_web_view_get_zoom_level (c->view);
      return env->make_float (env, (double)zoom);
    }
  return Qnil;
}

static emacs_value
webkit_forward (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    webkit_web_view_go_forward (c->view);
  return Qnil;
}

static emacs_value
webkit_back (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    webkit_web_view_go_back (c->view);
  return Qnil;
}

static emacs_value
webkit_reload (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    webkit_web_view_reload (c->view);
  return Qnil;
}

static emacs_value
webkit_get_title (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      const gchar *title = webkit_web_view_get_title (c->view);
      return env->make_string (env, title, strlen (title));
    }
  return Qnil;
}

static emacs_value
webkit_get_uri (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      const gchar *uri = webkit_uri_for_display (webkit_web_view_get_uri
                                                 (c->view));
      return env->make_string (env, uri, strlen (uri));
    }
  return Qnil;
}

static emacs_value
webkit_load_uri (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  size_t size;
  char *uri = NULL;
  if ((c != NULL) && copy_string_contents (env, args[1], &uri, &size))
    webkit_web_view_load_uri (c->view, uri);

  free (uri);
  return Qnil;
}

static emacs_value
webkit_hide (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    gtk_widget_hide (GTK_WIDGET (c->view));
  return Qnil;
}

static emacs_value
webkit_show (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    gtk_widget_show (GTK_WIDGET (c->view));
  return Qnil;
}

static emacs_value
webkit_focus (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      gtk_widget_set_can_focus (GTK_WIDGET (c->view), TRUE);
      gtk_widget_grab_focus (GTK_WIDGET (c->view));
    }
  return Qnil;
}

static emacs_value
webkit_unfocus (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      gtk_widget_set_can_focus (GTK_WIDGET (c->view), FALSE);
      gtk_widget_grab_focus (GTK_WIDGET (c->container));
    }
  return Qnil;
}

static emacs_value
webkit_resize (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client(env, args[0]);
  int x = env->extract_integer(env, args[1]);
  int y = env->extract_integer(env, args[2]);
  int w = env->extract_integer(env, args[3]);
  int h = env->extract_integer(env, args[4]);

  //printf("resize %p to (x:%d y:%d w:%d h:%d)\n", c, x, y, w, h);
  if ((env->non_local_exit_check(env) == emacs_funcall_exit_return)
      && (c != NULL))
    {
      if (GTK_IS_FIXED(c->container))
        gtk_fixed_move (GTK_FIXED (c->container), GTK_WIDGET(c->view), x, y);
      else if (GTK_IS_WINDOW(c->container))
        gtk_window_move (GTK_WINDOW (c->container), x, y);
      else
        assert (0);
      gtk_widget_set_size_request(GTK_WIDGET(c->view), w, h);
    }
  return Qnil;
}

static ssize_t
rio_writen (int fd, void *usrbuf, size_t n) 
{
  size_t nleft = n;
  ssize_t nwritten;
  char *bufp = usrbuf;

  while (nleft > 0) {
    if ((nwritten = write (fd, bufp, nleft)) <= 0) {
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
send_to_lisp (Client *c, const char *id, const char *message)
{
  if (id == NULL || message == NULL
      || rio_writen (c->fd, (void *)id, strlen (id)+1) < 0
      || rio_writen (c->fd, (void *)message, strlen (message)+1) < 0)
    g_warning ("Sending to fd: %d; id: %s; message: %s;", c->fd, id, message);
}

static void
webkit_js_finished (GObject *web_view, GAsyncResult *result, gpointer arg)
{
  GError *error = NULL;
  Callback *cb = (Callback *) arg;

  printf ("js_finished %p with id: %s\n", cb->c, cb->id);

  WebKitJavascriptResult *js_result =
    webkit_web_view_run_javascript_finish (WEBKIT_WEB_VIEW(web_view),
                                           result, &error);

  if (!js_result)
    {
      g_warning ("Error running javascript: %s", error->message);
      g_error_free (error);
      return;
    }

  JSCValue *value = webkit_javascript_result_get_js_value (js_result);
  gchar *json = jsc_value_to_json (value, 1);
  JSCException *exception =
    jsc_context_get_exception (jsc_value_get_context (value));
  if (exception)
    g_warning ("Error running javascript: %s",
               jsc_exception_get_message (exception));
  else
    send_to_lisp (cb->c, cb->id, json == NULL ? "null" : json);

  printf ("Script result: %s\n", json);

  g_free (json);
  free (cb->id);
  free (cb);
  webkit_javascript_result_unref (js_result);
}

static emacs_value
webkit_execute_js (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  size_t size;
  char *script = NULL;
  char *id = NULL;
  if ((c != NULL) && copy_string_contents (env, args[1], &script, &size))
    {
      if ((n == 3) && copy_string_contents (env, args[2], &id, &size))
        {
          Callback *cb = malloc (sizeof (Callback));
          cb->c = c;
          cb->id = id;
          webkit_web_view_run_javascript (c->view, script, NULL,
                                          webkit_js_finished, (gpointer) cb);
        }
      else
        {
          webkit_web_view_run_javascript (c->view, script, NULL, NULL, NULL);
        }
    }
  printf ("executing %p script: %s id: %s\n", c, script, id);
  free (script);
  return Qnil;
}

static emacs_value
webkit_add_user_style (emacs_env *env, ptrdiff_t n,
                          emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  size_t size;
  char *style = NULL;
  if ((c != NULL) && copy_string_contents (env, args[1], &style, &size))
    {
      WebKitUserContentManager *ucm =
        webkit_web_view_get_user_content_manager (c->view);
      WebKitUserStyleSheet *user_style = webkit_user_style_sheet_new
        (style,
         ((n > 3) && env->is_not_nil (env, args[3])) ?
         WEBKIT_USER_CONTENT_INJECT_TOP_FRAME :
         WEBKIT_USER_CONTENT_INJECT_ALL_FRAMES,
         ((n > 2) && env->is_not_nil (env, args[2])) ?
         WEBKIT_USER_STYLE_LEVEL_AUTHOR : WEBKIT_USER_STYLE_LEVEL_USER,
         NULL, NULL);
      webkit_user_content_manager_add_style_sheet (ucm, user_style);
      webkit_user_style_sheet_unref (user_style);
    }
  printf ("adding %p style: %s\n", c, style);
  free (style);
  return Qnil;
}

static emacs_value
webkit_remove_all_user_styles (emacs_env *env, ptrdiff_t n,
                                  emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      WebKitUserContentManager *ucm =
        webkit_web_view_get_user_content_manager (c->view);
      webkit_user_content_manager_remove_all_style_sheets (ucm);
    }
  printf("webkit_remove_all_user_styles from %p", c);
  return Qnil;
}
static emacs_value
webkit_add_user_script (emacs_env *env, ptrdiff_t n,
                           emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  size_t size;
  char *script = NULL;
  if ((c != NULL) && copy_string_contents (env, args[1], &script, &size))
    {
      WebKitUserContentManager *ucm =
        webkit_web_view_get_user_content_manager (c->view);
      WebKitUserScript *user_script =
        webkit_user_script_new (script,
                                ((n > 3) && env->is_not_nil (env, args[3])) ?
                                WEBKIT_USER_CONTENT_INJECT_TOP_FRAME :
                                WEBKIT_USER_CONTENT_INJECT_ALL_FRAMES,
                                ((n > 2) && env->is_not_nil (env, args[2])) ?
                                WEBKIT_USER_SCRIPT_INJECT_AT_DOCUMENT_END :
                                WEBKIT_USER_SCRIPT_INJECT_AT_DOCUMENT_START,
                                NULL, NULL);
      webkit_user_content_manager_add_script (ucm, user_script);
      webkit_user_script_unref (user_script);
    }
  printf ("adding %p script: %s\n", c, script);
  free (script);
  return Qnil;
}

static emacs_value
webkit_remove_all_user_scripts (emacs_env *env, ptrdiff_t n,
                                   emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      WebKitUserContentManager *ucm =
        webkit_web_view_get_user_content_manager (c->view);
      webkit_user_content_manager_remove_all_scripts (ucm);
    }
  printf("webkit_remove_all_user_scripts from %p", c);
  return Qnil;
}

static void
webkit_script_message_cb (WebKitUserContentManager *scriptor,
                             WebKitJavascriptResult *js_result, gpointer data)
{
  Client *c = (Client *)data;
  WebKitUserContentManager *ucm =
    webkit_web_view_get_user_content_manager (c->view);
  GSignalInvocationHint *hint = g_signal_get_invocation_hint ((gpointer)ucm);
  const gchar *name = g_quark_to_string (hint->detail);

  JSCValue *value = webkit_javascript_result_get_js_value (js_result);
  gchar *json = jsc_value_to_json (value, 1);
  JSCException *exception =
    jsc_context_get_exception (jsc_value_get_context (value));
  if (exception)
    g_warning ("Error in javascript message recieve: %s",
               jsc_exception_get_message (exception));
  else
    send_to_lisp (c, name, json == NULL ? "null" : json);

  printf ("Script name: %s, result: %s\n", name, json);
}

static emacs_value
webkit_register_script_message (emacs_env *env, ptrdiff_t n,
                                   emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  size_t size;
  char *name = NULL;
  if ((c != NULL) && copy_string_contents (env, args[1], &name, &size))
    {
      WebKitUserContentManager *ucm =
        webkit_web_view_get_user_content_manager (c->view);

      gchar *signal_name = g_strconcat ("script-message-received::", name, NULL);
      g_signal_connect (ucm, signal_name,
                        G_CALLBACK (webkit_script_message_cb), c);
      g_free (signal_name);

      if (!webkit_user_content_manager_register_script_message_handler (ucm, name))
        g_signal_handlers_disconnect_matched
          (ucm, G_SIGNAL_MATCH_FUNC, 0, g_quark_from_string (name), 0,
           G_CALLBACK (webkit_script_message_cb), 0);
    }
  printf ("adding %p script message: %s\n", c, name);
  free (name);
  return Qnil;
}

static emacs_value
webkit_unregister_script_message (emacs_env *env, ptrdiff_t n,
                                     emacs_value *args, void *ptr)
{
  size_t size;
  char *name = NULL;
  Client *c = get_client (env, args[0]);
  if ((c != NULL) && copy_string_contents (env, args[1], &name, &size))
    {
      WebKitUserContentManager *ucm =
        webkit_web_view_get_user_content_manager (c->view);

      webkit_user_content_manager_unregister_script_message_handler (ucm, name);
      g_signal_handlers_disconnect_matched
        (ucm, G_SIGNAL_MATCH_FUNC, 0, g_quark_from_string (name), 0,
         G_CALLBACK (webkit_script_message_cb), 0);
    }
  printf ("removing %p script message: %s\n", c, name);
  free (name);
  return Qnil;
}

static gboolean
webview_key_press_event (GtkWidget *w, GdkEvent *e, Client *c)
{
  printf ("key_press_event: %p\n", c);
  switch (e->type) {
  case GDK_KEY_PRESS:
    printf("key.keyval = %d\n", e->key.keyval);
    if (e->key.keyval == GDK_KEY_Escape && e->key.state == 0)
      {
        gtk_widget_grab_focus(GTK_WIDGET(c->container));
        return TRUE;
      }
  default:
    break;
  }
  return FALSE;
}

static void
webview_notify_load_progress (WebKitWebView *webview, GParamSpec *pspec, Client *c)
{
  gdouble prog = 100.0 * webkit_web_view_get_estimated_load_progress (webview);
  char buf[G_ASCII_DTOSTR_BUF_SIZE];
  send_to_lisp (c, "webkit--callback-progress",
                g_ascii_dtostr (buf, sizeof (buf), prog));
}

static void
webview_notify_uri (WebKitWebView *webview, GParamSpec *pspec, Client *c)
{
  const gchar *uri = webkit_uri_for_display (webkit_web_view_get_uri (webview));
  if (uri != NULL)
    send_to_lisp (c, "webkit--callback-uri", uri);
}

static void
webview_notify_title (WebKitWebView *webview, GParamSpec *pspec, Client *c)
{
  const gchar *title = webkit_web_view_get_title(webview);

  if (title != NULL)
    send_to_lisp (c, "webkit--callback-title", title);
}

static void
decide_navigation_action (Client *c, WebKitPolicyDecision *dec)
{
  WebKitNavigationAction *action =
    webkit_navigation_policy_decision_get_navigation_action
    (WEBKIT_NAVIGATION_POLICY_DECISION (dec));
  WebKitURIRequest *req = webkit_navigation_action_get_request(action);
  const char *uri = webkit_uri_request_get_uri(req);
  guint button = webkit_navigation_action_get_mouse_button (action);
  guint mod = webkit_navigation_action_get_modifiers (action);

  /* Request new view if triggered by CTRL-LeftMouse or MiddleMouse. */
  if ((webkit_navigation_action_get_navigation_type(action) ==
       WEBKIT_NAVIGATION_TYPE_LINK_CLICKED)
      && (button == 2 || (button == 1 && mod & GDK_CONTROL_MASK)))
    {
      webkit_policy_decision_ignore(dec);
      send_to_lisp (c, "webkit--callback-new-view", uri);
    }
  else
    {
      webkit_policy_decision_use(dec);
    }
}

static void
decide_new_window_action (Client *c, WebKitPolicyDecision *dec)
{
  WebKitNavigationAction *action =
    webkit_navigation_policy_decision_get_navigation_action
    (WEBKIT_NAVIGATION_POLICY_DECISION (dec));
  WebKitURIRequest *req = webkit_navigation_action_get_request(action);
  const char *uri = webkit_uri_request_get_uri(req);

  /* This is triggered on link click for links with target="_blank" */
  if (webkit_navigation_action_is_user_gesture(action))
    send_to_lisp (c, "webkit--callback-new-view", uri);
  webkit_policy_decision_ignore(dec);
}

static void
decide_response (Client *c, WebKitPolicyDecision *dec)
{
  if (webkit_response_policy_decision_is_mime_type_supported
      (WEBKIT_RESPONSE_POLICY_DECISION(dec))) {
    webkit_policy_decision_use(dec);
  } else {
    webkit_policy_decision_download(dec);
  }
}

static gboolean
webview_decide_policy (WebKitWebView *webview, WebKitPolicyDecision *dec,
                       WebKitPolicyDecisionType type, Client *c)
{
  switch (type)
    {
    case WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION:
      decide_navigation_action (c, dec);
      break;

    case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION:
      decide_new_window_action (c, dec);
      break;

    case WEBKIT_POLICY_DECISION_TYPE_RESPONSE:
      decide_response (c, dec);
      break;

    default:
      webkit_policy_decision_use (dec);
      break;
    }
  return TRUE;
}

static void
webcontext_download_started (WebKitWebContext *webctx, WebKitDownload *download,
                             Client *c)
{
  const char *uri =
    webkit_uri_request_get_uri(webkit_download_get_request(download));
  webkit_download_cancel(download);

  if (uri != NULL)
    send_to_lisp (c, "webkit--callback-download-request", uri);
} 

static gboolean
webview_close (WebKitWebView *webview, Client *c)
{
  assert (webview == c->view);
  send_to_lisp (c, "webkit--close", "");
  return TRUE;
}

static void
window_destroy (GtkWidget *window, Client *c)
{
  printf("window destroying %p\n", c);
  if (c->container != NULL)
    gtk_widget_destroy (c->container);
  c->container = NULL;
  c->view = NULL;
  send_to_lisp (c, "webkit--close", "");
}

static emacs_value
webkit_destroy (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  printf("destroying %p\n", c);
  if (c != NULL)
    {
      //gtk_container_remove (GTK_CONTAINER (c->container), GTK_WIDGET (c->view));
      gtk_widget_destroy (GTK_WIDGET (c->view));
      if (GTK_IS_WINDOW(c->container))
        gtk_widget_destroy (c->container);

      c->container = NULL;
      c->view = NULL;
    }
  return Qnil;
}

static void
client_free (void *ptr)
{
  printf ("freeing %p\n", ptr);
  Client *c = (Client *)ptr;
  assert (c->container == NULL);
  assert (c->view == NULL);
  free(c);
}

static void
print_widget_tree (GList *widgets)
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
          print_widget_tree(gtk_container_get_children(GTK_CONTAINER(l->data)));
        }
      else if (GTK_IS_CONTAINER(l->data))
        {
          printf("  container\n");
          print_widget_tree(gtk_container_get_children(GTK_CONTAINER(l->data)));
        }
    }
}

static GtkFixed *
find_fixed_widget (GList *widgets)
{
  for (GList *l = widgets; l != NULL; l = l->next)
    {
      if (GTK_IS_FIXED (l->data))
        return l->data;
      if (GTK_IS_CONTAINER (l->data))
        return find_fixed_widget (gtk_container_get_children
                                  (GTK_CONTAINER (l->data)));
    }
  return NULL;
}

static GtkFixed *
find_focused_fixed_widget ()
{
  GList *widgets = gtk_window_list_toplevels ();
  for (GList *l = widgets; l != NULL; l = l->next)
    if (gtk_window_has_toplevel_focus (l->data))
      return find_fixed_widget (gtk_container_get_children
                                (GTK_CONTAINER (l->data)));
  return NULL;
}

static emacs_value
webkit_new (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c;
  if (!(c = calloc (1, sizeof (Client))))
    {
      env->non_local_exit_signal (env, env->intern (env, "memory-full"),
                                  env->intern (env, "nil"));
      return Qnil;
    }

  c->fd = env->open_channel (env, args[0]);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
    return Qnil;

  c->view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  gtk_widget_set_can_focus(GTK_WIDGET(c->view), FALSE);
  //gtk_widget_set_focus_on_click (GTK_WIDGET (c->view), FALSE);

  print_widget_tree(gtk_window_list_toplevels());
  if (env->is_not_nil (env, args[1]))
    {
      c->container = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      //gtk_window_set_default_size
      //  (GTK_WINDOW(c->container),
      //   (n > 2) ? env->extract_integer (env, args[2]) : 400
      //   (n > 3) ? env->extract_integer (env, args[3]) : 600);

      g_signal_connect (G_OBJECT (c->container), "destroy",
                        G_CALLBACK(window_destroy), c);

      gtk_container_add (GTK_CONTAINER(c->container), GTK_WIDGET (c->view));
      gtk_widget_show_all(c->container);
    }
  else
    {
      GtkFixed *fixed = find_focused_fixed_widget ();
      if (fixed == NULL)
        {
          env->non_local_exit_signal
            (env, env->intern (env, "webkit-module-no-focused-fixed-widget"),
             env->intern (env, "nil"));
          return Qnil;
        }
      c->container = GTK_WIDGET (fixed);
      gtk_fixed_put (GTK_FIXED (c->container), GTK_WIDGET (c->view), 0, 0);
      gtk_widget_show_all (GTK_WIDGET (c->view));
    }

  g_signal_connect (G_OBJECT (c->view), "close",
                    G_CALLBACK(webview_close), c);
  g_signal_connect (G_OBJECT (c->view), "key-press-event",
                    G_CALLBACK (webview_key_press_event), c);
  g_signal_connect (G_OBJECT (c->view), "notify::title",
                    G_CALLBACK (webview_notify_title), c);
  g_signal_connect (G_OBJECT (c->view), "notify::uri",
                    G_CALLBACK (webview_notify_uri), c);
  g_signal_connect (G_OBJECT (c->view), "notify::estimated-load-progress",
                    G_CALLBACK (webview_notify_load_progress), c);
  g_signal_connect (G_OBJECT (c->view), "decide-policy",
                    G_CALLBACK (webview_decide_policy), c);
  g_signal_connect(webkit_web_view_get_context(c->view), "download-started",
                   G_CALLBACK(webcontext_download_started), c);

  /* webkit uses GSubprocess which sets sigaction causing emacs to not catch
     SIGCHLD with it's usual handle setup in catch_child_signal().
     This resets the SIGCHLD sigaction */
  struct sigaction old_action;
  sigaction (SIGCHLD, NULL, &old_action);
  webkit_web_view_load_uri(c->view, "about:blank");
  sigaction (SIGCHLD, &old_action, NULL);

  return env->make_user_ptr (env, client_free, (void *) c);
}

static void
bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  env->funcall(env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}

int
emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);
  int argc = 0;
  char **argv = NULL;
  if (!gtk_init_check (&argc, &argv))
    {
      env->non_local_exit_signal
        (env, env->intern (env, "webkit-module-init-gtk-failed"),
         env->intern (env, "nil"));
      return 1;
    }

  // Symbols
  Qt = env->make_global_ref(env, env->intern(env, "t"));
  Qnil = env->make_global_ref(env, env->intern(env, "nil"));

  // Functions
  emacs_value fun;
  fun = env->make_function(env, 2, 2, webkit_new, "", NULL);
  bind_function(env, "webkit--new", fun);

  fun = env->make_function(env, 1, 1, webkit_destroy, "", NULL);
  bind_function(env, "webkit--destroy", fun);

  fun = env->make_function(env, 5, 5, webkit_resize, "", NULL);
  bind_function(env, "webkit--resize", fun);

  fun = env->make_function(env, 1, 1, webkit_hide, "", NULL);
  bind_function(env, "webkit--hide", fun);

  fun = env->make_function(env, 1, 1, webkit_show, "", NULL);
  bind_function(env, "webkit--show", fun);

  fun = env->make_function(env, 1, 1, webkit_focus, "", NULL);
  bind_function(env, "webkit--focus", fun);

  fun = env->make_function(env, 1, 1, webkit_unfocus, "", NULL);
  bind_function(env, "webkit--unfocus", fun);

  fun = env->make_function(env, 1, 1, webkit_forward, "", NULL);
  bind_function(env, "webkit--forward", fun);

  fun = env->make_function(env, 1, 1, webkit_back, "", NULL);
  bind_function(env, "webkit--back", fun);

  fun = env->make_function(env, 1, 1, webkit_reload, "", NULL);
  bind_function(env, "webkit--reload", fun);

  fun = env->make_function(env, 1, 1, webkit_get_zoom, "", NULL);
  bind_function(env, "webkit--get-zoom", fun);

  fun = env->make_function(env, 2, 2, webkit_set_zoom, "", NULL);
  bind_function(env, "webkit--set-zoom", fun);

  fun = env->make_function(env, 1, 1, webkit_get_title, "", NULL);
  bind_function(env, "webkit--get-title", fun);

  fun = env->make_function(env, 1, 1, webkit_get_uri, "", NULL);
  bind_function(env, "webkit--get-uri", fun);

  fun = env->make_function(env, 2, 2, webkit_load_uri, "", NULL);
  bind_function(env, "webkit--load-uri", fun);

  fun = env->make_function(env, 2, 3, webkit_execute_js, "", NULL);
  bind_function(env, "webkit--execute-js", fun);

  fun = env->make_function(env, 2, 4, webkit_add_user_style, "", NULL);
  bind_function(env, "webkit--add-user-style", fun);

  fun = env->make_function(env, 1, 1, webkit_remove_all_user_styles, "", NULL);
  bind_function(env, "webkit--remove-all-user-styles", fun);

  fun = env->make_function(env, 2, 4, webkit_add_user_script, "", NULL);
  bind_function(env, "webkit--add-user-script", fun);

  fun = env->make_function(env, 1, 1, webkit_remove_all_user_scripts, "", NULL);
  bind_function(env, "webkit--remove-all-user-scripts", fun);

  fun = env->make_function(env, 2, 2, webkit_register_script_message, "", NULL);
  bind_function(env, "webkit--register-script-message", fun);

  fun = env->make_function(env, 2, 2, webkit_unregister_script_message, "", NULL);
  bind_function(env, "webkit--unregister-script-message", fun);

  emacs_value Qfeat = env->intern(env, "webkit-module");
  emacs_value Qprovide = env->intern(env, "provide");
  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
  printf("init webkit-module\n");
  return 0;
}

