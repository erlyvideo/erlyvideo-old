-define(D(X), ems_log:debug(3, main, "~p:~p ~p",[?MODULE, ?LINE, X])).
-define(DBG(F,A), ems_log:debug(3, main, "(~w:~b): " ++ F, [?MODULE, ?LINE] ++ A)).

