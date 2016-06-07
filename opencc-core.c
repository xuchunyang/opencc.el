#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <emacs-module.h>
#include <opencc.h>

int plugin_is_GPL_compatible;

static emacs_value
Fopencc_core (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value lisp_str;
  ptrdiff_t size = 0;
  char *text = NULL;
  char *config = NULL;

  lisp_str = args[1];
  env->copy_string_contents (env, lisp_str, config, &size);
  config = malloc (size);
  env->copy_string_contents (env, lisp_str, config, &size);

  lisp_str = args[0];
  env->copy_string_contents (env, lisp_str, text, &size);
  text = malloc (size);
  env->copy_string_contents (env, lisp_str, text, &size);

  opencc_t opencc = opencc_open (config);
  char *res = opencc_convert_utf8 (opencc, text, strlen (text));
  emacs_value rtv = env->make_string (env, res, size - 1);
  opencc_convert_utf8_free (res);
  return rtv;
}

static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, \
                 env->make_function (env, amin, amax, csym, doc, data));

  DEFUN ("opencc-core", Fopencc_core, 2, 2, NULL, NULL);
#undef DEFUN

  provide (env, "opencc-core");
  return 0;
}
