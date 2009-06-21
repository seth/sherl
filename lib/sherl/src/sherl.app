%% -*- mode: erlang -*-
{application, sherl,
 [{description, "sherl URL shortening core service"},
  {vsn, "0.1"},
  {modules, [base62, sherl, sherl_app, sherl_db, sherl_sup]},
  {registered, [sherl, sherl_sup]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {sherl_app, []}},
  {start_phases, []}
 ]}.
