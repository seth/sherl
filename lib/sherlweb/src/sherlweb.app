{application, sherlweb,
 [{description, "sherlweb"},
  {vsn, "0.1"},
  {modules, [
    sherlweb,
    sherlweb_app,
    sherlweb_sup,
    sherlweb_deps,
    shorten_resource,
    lookup_resource
  ]},
  {registered, []},
  {mod, {sherlweb_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
