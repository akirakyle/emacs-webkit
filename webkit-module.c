#define _POSIX_SOURCE 1

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>
#include <stdbool.h>
#include <signal.h>
#include <assert.h>
#include <errno.h>
#include <gdk/gdkx.h>

#include "emacs-module.h"

#ifdef DEBUG
#define DEBUG_TEST 1
#else
#define DEBUG_TEST 0
#endif

#define debug_print(fmt, ...)                                           \
  do { if (DEBUG_TEST) fprintf(stderr, fmt, ##__VA_ARGS__); } while (0)

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
      && c->container != NULL && c->view != NULL)
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
      if (title != NULL)
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
webkit_search (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  size_t size;
  char *text = NULL;
  if ((c != NULL) && copy_string_contents (env, args[1], &text, &size))
    webkit_find_controller_search (webkit_web_view_get_find_controller(c->view),
                                   text,
                                   ((n > 2) && env->is_not_nil (env, args[2])) ?
                                   WEBKIT_FIND_OPTIONS_NONE :
                                   WEBKIT_FIND_OPTIONS_CASE_INSENSITIVE,
                                   G_MAXUINT);

  free (text);
  return Qnil;
}

static emacs_value
webkit_search_finish (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    webkit_find_controller_search_finish
      (webkit_web_view_get_find_controller(c->view));
  return Qnil;
}

static emacs_value
webkit_search_next (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    webkit_find_controller_search_next
      (webkit_web_view_get_find_controller(c->view));
  return Qnil;
}

static emacs_value
webkit_search_previous (emacs_env *env, ptrdiff_t n,
                        emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    webkit_find_controller_search_previous
      (webkit_web_view_get_find_controller(c->view));
  return Qnil;
}

static emacs_value
webkit_start_web_inspector (emacs_env *env, ptrdiff_t n,
                            emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      WebKitSettings *settings = webkit_web_view_get_settings (c->view);
      g_object_set (G_OBJECT(settings), "enable-developer-extras", TRUE, NULL);

      WebKitWebInspector *inspector = webkit_web_view_get_inspector (c->view);
      webkit_web_inspector_show (inspector);
    }
  return Qnil;
}

static emacs_value
webkit_enable_javascript (emacs_env *env, ptrdiff_t n,
                          emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if (c != NULL)
    {
      WebKitSettings *settings = webkit_web_view_get_settings (c->view);
      g_object_set (G_OBJECT(settings), "enable-javascript-markup",
                    env->is_not_nil (env, args[1]) ? TRUE : FALSE, NULL);
      debug_print ("c %p enable-javascript-markup %d\n", c,
                   env->is_not_nil (env, args[1]) ? TRUE : FALSE);
    }
  return Qnil;
}

static emacs_value
webkit_cookie_set_persistent_storage (emacs_env *env, ptrdiff_t n,
                                      emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  size_t size;
  char *file = NULL;
  if ((c != NULL) && copy_string_contents (env, args[1], &file, &size))
    {
      WebKitWebContext *context = webkit_web_view_get_context (c->view);
      WebKitCookieManager *cm = webkit_web_context_get_cookie_manager (context);
      webkit_cookie_manager_set_persistent_storage
        (cm, file, WEBKIT_COOKIE_PERSISTENT_STORAGE_TEXT);
      debug_print ("c %p webkit_cookie_set_persistent_storage %s\n", c, file);
    }
  free (file);
  return Qnil;
}

static emacs_value
webkit_proxy_set_uri (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  size_t size;
  char *proxy_uri = NULL;
  if ((c != NULL) && copy_string_contents (env, args[1], &proxy_uri, &size))
    {
      WebKitWebContext *context = webkit_web_view_get_context (c->view);
      WebKitNetworkProxySettings *proxy = webkit_network_proxy_settings_new
        (proxy_uri, NULL);
      webkit_web_context_set_network_proxy_settings
        (context, WEBKIT_NETWORK_PROXY_MODE_CUSTOM, proxy);
      webkit_network_proxy_settings_free (proxy);
      debug_print ("c %p webkit_proxy_set_uri %s\n", c, proxy_uri);
    }
  free (proxy_uri);
  return Qnil;
}

static emacs_value
webkit_proxy_set_default (emacs_env *env, ptrdiff_t n,
                          emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  if ((c != NULL))
    {
      WebKitWebContext *context = webkit_web_view_get_context (c->view);
      webkit_web_context_set_network_proxy_settings
        (context, WEBKIT_NETWORK_PROXY_MODE_DEFAULT, NULL);
      debug_print ("c %p webkit_proxy_set_default\n", c);
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

  debug_print ("c %p resize (x:%d y:%d w:%d h:%d)\n", c, x, y, w, h);
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

  debug_print ("c %p js_finished with id: %s\n", cb->c, cb->id);

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

  debug_print ("Script result: %s\n", json);

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
  debug_print ("c %p executing script: %s id: %s\n", c, script, id);
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
  debug_print ("c %p add_user_style: %s\n", c, style);
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
  debug_print ("c %p webkit_remove_all_user_styles\n", c);
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
  debug_print ("c %p add_user_script: %s\n", c, script);
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
  debug_print ("c %p webkit_remove_all_user_scripts\n", c);
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

  debug_print ("Script name: %s, result: %s\n", name, json);
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
  debug_print ("c %p register_script_message: %s\n", c, name);
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
  debug_print ("c %p unregister_script_message: %s\n", c, name);
  free (name);
  return Qnil;
}

static gboolean
webview_key_press_event (GtkWidget *w, GdkEvent *e, Client *c)
{
  debug_print ("key_press_event: %p\n", c);
  switch (e->type) {
  case GDK_KEY_PRESS:
    debug_print ("key.keyval = %d\n", e->key.keyval);
    //if (e->key.keyval == GDK_KEY_Escape && e->key.state == 0)
    if ((e->key.state & GDK_CONTROL_MASK) && (e->key.keyval == 'g'))
      {
        debug_print ("c %p webview_key_press_event c-g detected\n", c);
        send_to_lisp (c, "webkit--callback-unfocus", "");
        return TRUE;
      }
  default:
    break;
  }
  return FALSE;
}

static void
webview_load_changed (WebKitWebView  *webview, WebKitLoadEvent load_event,
                      Client *c)
{
  switch (load_event) {
  case WEBKIT_LOAD_STARTED:
    break;
  case WEBKIT_LOAD_REDIRECTED:
    break;
  case WEBKIT_LOAD_COMMITTED:
    break;
  case WEBKIT_LOAD_FINISHED:
    send_to_lisp (c, "webkit--load-finished", "");
    break;
  }
}

static void
webview_notify_load_progress (WebKitWebView *webview, GParamSpec *pspec, Client *c)
{
  gdouble prog = 100.0 * webkit_web_view_get_estimated_load_progress (webview);
  gchar *buf = g_strdup_printf ("%f", prog);
  send_to_lisp (c, "webkit--callback-progress", buf);
  g_free (buf);
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
findcontroller_counted_matches(WebKitFindController *finder,
                               guint count, Client *c)
{
  gchar *buf = g_strdup_printf ("%d", count);
  send_to_lisp (c, "webkit--counted-matches", buf);
  g_free (buf);
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

/*
#ifdef DEBUG
static void
print_widget_tree (GList *widgets)
{
  for (GList *l = widgets; l != NULL; l = l->next)
    {
      char *path = gtk_widget_path_to_string (gtk_widget_get_path (l->data));
      const char *type_name = G_OBJECT_TYPE_NAME (l->data);
      //debug_print ("widget %p; fixed %d; path %s\n", l->data,
      //             GTK_IS_FIXED (l->data), path);
      if (GTK_IS_MENU(l->data))
        continue;
      debug_print ("widget %p; fixed %d; type %s\n", l->data,
                   GTK_IS_FIXED (l->data), type_name);
      if (GTK_IS_WINDOW (l->data))
        debug_print ("  ->window; focused %d; title %s\n",
                     gtk_window_has_toplevel_focus (l->data),
                     gtk_window_get_title (l->data));
      if (GTK_IS_CONTAINER(l->data))
        print_widget_tree (gtk_container_get_children (GTK_CONTAINER (l->data)));
    }
}
#endif
*/

static GtkFixed *
find_fixed_widget (GList *widgets)
{
  for (GList *l = widgets; l != NULL; l = l->next)
    {
      debug_print ("widget %p; fixed %d; type %s\n", l->data,
                   GTK_IS_FIXED (l->data), G_OBJECT_TYPE_NAME (l->data));
      if (GTK_IS_FIXED (l->data))
        return l->data;
      if (GTK_IS_BOX (l->data))
        {
          GtkFixed *fixed = find_fixed_widget (gtk_container_get_children
                                               (GTK_CONTAINER (l->data)));
          if (fixed != NULL)
            return fixed;
        }
    }
  return NULL;
}

static GtkFixed *
find_focused_fixed_widget ()
{
  GList *widgets = gtk_window_list_toplevels ();
  for (GList *l = widgets; l != NULL; l = l->next)
    {
      debug_print ("window %p focused %d\n", l->data,
                   gtk_window_has_toplevel_focus (l->data));
      if (gtk_window_has_toplevel_focus (l->data))
        return find_fixed_widget (gtk_container_get_children
                                  (GTK_CONTAINER (l->data)));
    }
  return NULL;
}

int container_child_prop_helper (GtkWidget *container, gpointer child,
                                 const char *prop)
{
  GValue v = G_VALUE_INIT;
  g_value_init (&v, G_TYPE_INT);
  gtk_container_child_get_property (GTK_CONTAINER (container),
                                    GTK_WIDGET (child), prop, &v);
  return g_value_get_int (&v);
}

static void
webview_change_container (Client *c, GtkFixed *fixed)
{
  debug_print ("c %p change_container from %p to %p\n", c, c->container, fixed);
  if (c->container != NULL)
    gtk_container_remove (GTK_CONTAINER (c->container),
                          GTK_WIDGET (c->view));
  c->container = GTK_WIDGET (fixed);

  /* play nice with child frames (should webkit always go under child frames?) */
  GList *widgets = gtk_container_get_children (GTK_CONTAINER (c->container));

  gtk_fixed_put (GTK_FIXED (c->container), GTK_WIDGET (c->view), 0, 0);
  //gtk_container_add (GTK_CONTAINER (c->container), GTK_WIDGET (c->view));

  for (GList *l = widgets; l != NULL; l = l->next)
    {
      int x = container_child_prop_helper (c->container, l->data, "x");
      int y = container_child_prop_helper (c->container, l->data, "y");
      debug_print ("c %p removing child with x: %d, y: %d\n", c, x, y);
      g_object_ref (l->data);
      gtk_container_remove (GTK_CONTAINER (c->container), GTK_WIDGET (l->data));
      gtk_fixed_put (GTK_FIXED (c->container), GTK_WIDGET (l->data), x, y);
      g_object_unref (l->data);
    }
}

static emacs_value
webkit_xid_to_pointer (emacs_env *env, ptrdiff_t n,
                       emacs_value *args, void *ptr)
{
  intmax_t xid = env->extract_integer(env, args[0]);
  debug_print ("xid_to_pointer xid %lu\n", xid);

  GList *widgets = gtk_window_list_toplevels ();
  for (GList *l = widgets; l != NULL; l = l->next)
    {
      debug_print ("window %p focused %d\n", l->data,
                   gtk_window_has_toplevel_focus (l->data));
      GtkFixed *fixed = find_fixed_widget (gtk_container_get_children
                                           (GTK_CONTAINER (l->data)));
      if (fixed != NULL)
        {
          //debug_print ("gdk window %p\n", gtk_widget_get_window (GTK_WIDGET(fixed)));
          //debug_print ("gdk window %p\n", gtk_widget_get_window (l->data));
          uintptr_t fixed_xid = (uintptr_t)GDK_WINDOW_XID
            (gtk_widget_get_window (GTK_WIDGET(fixed)));
          debug_print ("fixed %p xid %lu\n", fixed, fixed_xid);

          if (((uintptr_t)xid) == fixed_xid)
            return env->make_integer (env, (intmax_t)l->data);
        }
    }
  return Qnil;
}

static emacs_value
webkit_move_to_frame (emacs_env *env, ptrdiff_t n,
                      emacs_value *args, void *ptr)
{
  intmax_t window_id = env->extract_integer(env, args[1]);
  Client *c = get_client (env, args[0]);
#ifdef DEBUG
  //print_widget_tree (gtk_window_list_toplevels());
#endif
  debug_print ("c %p move_to_frame %p\n", c, (void *)window_id);
  if (c != NULL)
    {
      //webkit_move_to_focused_frame_internal (c, env);
      GList *widgets = gtk_window_list_toplevels ();
      for (GList *l = widgets; l != NULL; l = l->next)
        {
          debug_print ("window %p focused %d\n", l->data,
                       gtk_window_has_toplevel_focus (l->data));
          if (l->data == (void *)window_id)
            {
              GtkFixed *fixed = find_fixed_widget (gtk_container_get_children
                                                   (GTK_CONTAINER (l->data)));
              if (fixed != NULL)
                {
                  webview_change_container (c, fixed);
                  return Qnil;
                }
            }
        }
    }
  env->non_local_exit_signal
    (env, env->intern (env, "webkit-module-no-fixed-widget"),
     env->intern (env, "nil"));
  return Qnil;
}

static gboolean
webview_close (WebKitWebView *webview, Client *c)
{
  debug_print ("c %p webview_close\n", c);
  assert (webview == c->view);
  send_to_lisp (c, "webkit--close", "");
  return TRUE;
}

static void
window_destroy (GtkWidget *window, Client *c)
{
  debug_print ("c %p window_destroy\n", c);
  if (c->container != NULL)
    gtk_widget_destroy (c->container);
  c->container = NULL;
  send_to_lisp (c, "webkit--close", "");
}

/*
static void
webview_destroy (WebKitWebView *webview, Client *c)
{
  debug_print ("webview_destroy %p\n", c);
  //c->container = NULL;
#ifdef DEBUG
  print_widget_tree (gtk_window_list_toplevels());
#endif
  GtkFixed *fixed = find_fixed_widget (gtk_window_list_toplevels ());
  if (fixed == NULL)
    {
      c->container = NULL;
      send_to_lisp (c, "webkit--close", "");
    }
  else 
    {
      webkit_move_to_frame (c, fixed);
    }
}
*/

static emacs_value
webkit_destroy (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  Client *c = get_client (env, args[0]);
  debug_print ("c %p webkit_destroy\n", c);
  if (c != NULL)
    {
      if (GTK_IS_WINDOW(c->container))
        gtk_widget_destroy (c->container);

      c->container = NULL;
    }
  return Qnil;
}

static void
client_free (void *ptr)
{
  debug_print ("c %p client_free\n", ptr);
  Client *c = (Client *)ptr;
  assert (c->container == NULL);
  gtk_widget_destroy (GTK_WIDGET (c->view));
  g_object_unref (c->view);
  free(c);
}

static emacs_value
webkit_new (emacs_env *env, ptrdiff_t n, emacs_value *args, void *ptr)
{
  int argc = 0;
  char **argv = NULL;

  if (!gtk_init_check (&argc, &argv))
    {
      env->non_local_exit_signal
        (env, env->intern (env, "webkit-module-init-gtk-failed"),
         env->intern (env, "nil"));
      return Qnil;
    }

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

  WebKitWebContext *context = webkit_web_context_new ();
  //webkit_web_context_set_sandbox_enabled (context, true);

  c->view = WEBKIT_WEB_VIEW (webkit_web_view_new_with_context (context));
  /* set lifetime of c->view to be same as c which is owend by Emacs user_ptr */
  g_object_ref (c->view); 
  g_object_ref_sink (c->view);
  gtk_widget_set_can_focus(GTK_WIDGET(c->view), FALSE);
  //gtk_widget_set_focus_on_click (GTK_WIDGET (c->view), FALSE);

#ifdef DEBUG
  //print_widget_tree (gtk_window_list_toplevels());
#endif
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
          const char *err_msg = "webkit-module-no-focused-fixed-widget";
          env->non_local_exit_signal
            (env, env->intern (env, err_msg),
             env->make_string (env, err_msg, strlen (err_msg)));
          return Qnil;
        }
      webview_change_container (c, fixed);
      gtk_widget_show_all (GTK_WIDGET (c->view));
    }

  //g_signal_connect (G_OBJECT (c->view), "destroy",
  //                  G_CALLBACK(webview_destroy), c);
  g_signal_connect (G_OBJECT (c->view), "close",
                    G_CALLBACK(webview_close), c);
  g_signal_connect (G_OBJECT (c->view), "key-press-event",
                    G_CALLBACK (webview_key_press_event), c);
  g_signal_connect (G_OBJECT (c->view), "load-changed",
                    G_CALLBACK (webview_load_changed), c);
  g_signal_connect (G_OBJECT (c->view), "notify::title",
                    G_CALLBACK (webview_notify_title), c);
  g_signal_connect (G_OBJECT (c->view), "notify::uri",
                    G_CALLBACK (webview_notify_uri), c);
  g_signal_connect (G_OBJECT (c->view), "notify::estimated-load-progress",
                    G_CALLBACK (webview_notify_load_progress), c);
  g_signal_connect (G_OBJECT (c->view), "decide-policy",
                    G_CALLBACK (webview_decide_policy), c);
  g_signal_connect (webkit_web_view_get_context (c->view), "download-started",
                    G_CALLBACK (webcontext_download_started), c);
  g_signal_connect (webkit_web_view_get_find_controller (c->view),
                    "counted-matches",
                    G_CALLBACK(findcontroller_counted_matches), c);

#ifdef DEBUG
  webkit_settings_set_enable_write_console_messages_to_stdout
    (webkit_web_view_get_settings (c->view), true);
#endif
  // https://bugs.webkit.org/show_bug.cgi?id=200856
  // https://github.com/NixOS/nixpkgs/pull/103728
  webkit_settings_set_hardware_acceleration_policy
    (webkit_web_view_get_settings (c->view), WEBKIT_HARDWARE_ACCELERATION_POLICY_NEVER);
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
mkfn (emacs_env *env,
      ptrdiff_t min_arity,
      ptrdiff_t max_arity,
      emacs_value (*func) (emacs_env *env,
                           ptrdiff_t nargs,
                           emacs_value* args,
                           void *data),
      const char *name,
      const char *docstring,
      void *data)
{
  emacs_value Sfun = env->make_function (env, min_arity, max_arity,
                                         func, docstring, data);
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);

  env->funcall (env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);
  // Symbols
  Qt = env->make_global_ref (env, env->intern(env, "t"));
  Qnil = env->make_global_ref (env, env->intern(env, "nil"));

  // Functions
  mkfn (env, 2, 2, webkit_new, "webkit--new", "", NULL);
  mkfn (env, 1, 1, webkit_destroy, "webkit--destroy", "", NULL);
  mkfn (env, 5, 5, webkit_resize, "webkit--resize", "", NULL);
  mkfn (env, 2, 2, webkit_move_to_frame, "webkit--move-to-frame", "", NULL);
  mkfn (env, 1, 1, webkit_xid_to_pointer, "webkit--xid-to-pointer", "", NULL);
  mkfn (env, 1, 1, webkit_hide, "webkit--hide", "", NULL);
  mkfn (env, 1, 1, webkit_show, "webkit--show", "", NULL);
  mkfn (env, 1, 1, webkit_focus, "webkit--focus", "", NULL);
  mkfn (env, 1, 1, webkit_unfocus, "webkit--unfocus", "", NULL);
  mkfn (env, 1, 1, webkit_forward, "webkit--forward", "", NULL);
  mkfn (env, 1, 1, webkit_back, "webkit--back", "", NULL);
  mkfn (env, 1, 1, webkit_reload, "webkit--reload", "", NULL);
  mkfn (env, 1, 1, webkit_get_zoom, "webkit--get-zoom", "", NULL);
  mkfn (env, 2, 2, webkit_set_zoom, "webkit--set-zoom", "", NULL);
  mkfn (env, 1, 1, webkit_get_title, "webkit--get-title", "", NULL);
  mkfn (env, 1, 1, webkit_get_uri, "webkit--get-uri", "", NULL);
  mkfn (env, 2, 2, webkit_load_uri, "webkit--load-uri", "", NULL);
  mkfn (env, 2, 3, webkit_search, "webkit--search", "", NULL);
  mkfn (env, 1, 1, webkit_search_finish, "webkit--search-finish", "", NULL);
  mkfn (env, 1, 1, webkit_search_next, "webkit--search-next", "", NULL);
  mkfn (env, 1, 1, webkit_search_previous, "webkit--search-previous", "", NULL);
  mkfn (env, 1, 1, webkit_start_web_inspector, "webkit--start-web-inspector", "", NULL);
  mkfn (env, 2, 2, webkit_enable_javascript, "webkit--enable-javascript", "", NULL);
  mkfn (env, 2, 2, webkit_cookie_set_persistent_storage, "webkit--cookie-set-storage", "", NULL);
  mkfn (env, 2, 2, webkit_proxy_set_uri, "webkit--proxy-set-uri", "", NULL);
  mkfn (env, 1, 1, webkit_proxy_set_default, "webkit--proxy-set-default", "", NULL);
  mkfn (env, 2, 3, webkit_execute_js, "webkit--execute-js", "", NULL);
  mkfn (env, 2, 4, webkit_add_user_style, "webkit--add-user-style", "", NULL);
  mkfn (env, 1, 1, webkit_remove_all_user_styles, "webkit--remove-all-user-styles", "", NULL);
  mkfn (env, 2, 4, webkit_add_user_script, "webkit--add-user-script", "", NULL);
  mkfn (env, 1, 1, webkit_remove_all_user_scripts, "webkit--remove-all-user-scripts", "", NULL);
  mkfn (env, 2, 2, webkit_register_script_message, "webkit--register-script-message", "", NULL);
  mkfn (env, 2, 2, webkit_unregister_script_message, "webkit--unregister-script-message", "", NULL);

  emacs_value Qfeat = env->intern (env, "webkit-module");
  emacs_value Qprovide = env->intern (env, "provide");
  env->funcall (env, Qprovide, 1, (emacs_value[]){Qfeat});
  debug_print ("init webkit-module\n");
  return 0;
}

