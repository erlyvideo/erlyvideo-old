-define(D(X), (case application:get_env(rtmp, logging_function) of undefined -> io:format("~p:~p ~240p~n", [?MODULE, ?LINE, X]); _ -> (element(2,application:get_env(rtmp,logging_function)))(?MODULE, ?LINE, X) end)).

