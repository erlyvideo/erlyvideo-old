-export([get/2, set/3, set/2, construct/1]).

get(Media, Key) when is_pid(Media) ->
  gen_server:call(Media, {get_field, Key});

get(Record, Keys) when is_list(Keys) ->
  [get(Record, Key) || Key <- Keys];
  
get(#?MODULE{} = Media, Key) -> 
  case record_index(Key, record_info(fields, ?MODULE)) of
    undefined ->
      Props = Media#?MODULE.properties,
      proplists:get_value(Key, Props);
    Pos ->  
      element(Pos, Media)
  end.
  

set(Media, Key, Value) when is_pid(Media) ->
  gen_server:call(Media, {set_field, Key, Value});  

set(#?MODULE{} = Media, Key, Value) ->
  case record_index(Key, record_info(fields, ?MODULE)) of
    undefined ->
      Props = case Media#?MODULE.properties of
        undefined -> [];
        Else -> Else
      end,
      Media#?MODULE{properties = lists:keystore(Key, 1, Props, {Key,Value})};
    Pos ->
      setelement(Pos, Media, Value)
  end.

record_index(Element, List) -> record_index(Element, List, 1).
record_index(Element, [Element|_], N) -> N + 1;
record_index(Element, [_|List], N) -> record_index(Element, List, N+1);
record_index(_, [], _) -> undefined.


set(#?MODULE{} = Media, Options) ->
  lists:foldl(fun({K,V}, M) -> set(M, K, V) end, Media, Options).


construct(Values) ->
  set(#?MODULE{}, Values).