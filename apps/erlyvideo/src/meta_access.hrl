get(#?MODULE{} = Media, Key) -> 
  element(index(Key, record_info(fields, ?MODULE)) + 1, Media);
  
get(Media, Key) when is_pid(Media) ->
  gen_server:call(Media, {get_field, Key}).
  

set(#?MODULE{} = Media, Key, Value) ->
  Pos = index(Key, record_info(fields, ?MODULE)) + 1,
  setelement(Pos, Media, Value).

index(Element, List) -> index(Element, List, 1).
index(Element, [Element|_], N) -> N;
index(Element, [_|List], N) -> index(Element, List, N+1);
index(_, [], _) -> undefined.


set(#?MODULE{} = Media, Options) ->
  lists:foldl(fun({K,V}, M) -> set(M, K, V) end, Media, Options).

