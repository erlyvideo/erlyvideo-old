%% -*- mode: Erlang; -*-

{application, shoutcast,
 [
  {description, "Shoutcast reader"},
  {vsn, "1.2.2"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {modules, [
             shoutcast_reader
            ]},
  {env, []}
 ]}.
