get(Media, Key) when is_pid(Media) ->
  gen_server:call(Media, {get_field, Key});

get(Record, Keys) when is_list(Keys) ->
  [get(Record, Key) || Key <- Keys];
  
get(#?MODULE{} = Media, Key) -> 
  element(index(Key, record_info(fields, ?MODULE)) + 1, Media).
  
  

set(#?MODULE{} = Media, Key, Value) ->
  Pos = index(Key, record_info(fields, ?MODULE)) + 1,
  setelement(Pos, Media, Value).

index(Element, List) -> index(Element, List, 1).
index(Element, [Element|_], N) -> N;
index(Element, [_|List], N) -> index(Element, List, N+1);
index(_, [], _) -> undefined.


set(#?MODULE{} = Media, Options) ->
  lists:foldl(fun({K,V}, M) -> set(M, K, V) end, Media, Options).

