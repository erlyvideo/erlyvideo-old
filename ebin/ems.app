{application, ems,
 [
  {description, "Erlang Media Server"},
  {vsn, "0.0.5"},
  {id, "ems_server"},
  {modules,      [ems_server]},
  {registered,   [ems_sup, ems_server]},
  {applications, [kernel, stdlib]},
  {mod, {ems_app, []}},
  {env, []}
 ]
}.
