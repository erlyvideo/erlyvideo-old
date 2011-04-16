-define(record_to_struct(RecordName, Record),
  % we are zipping record's field names and corresponding values together
  % then we turn it into tuple resulting in *struct* - erlang's equivalent of json's *object*
  list_to_tuple(
    lists:zip(
      lists:map(fun(F) -> list_to_binary(atom_to_list(F)) end, record_info(fields, RecordName)),
      lists:map(
        %% convention record's *undefined* value is represented as json's *null*
        fun(undefined) -> null;
           (E) -> E
        end,
    %% we are turning the record into list chopping its head (record name) off
        tl(tuple_to_list(Record))
      )
    )
  )
).

-define(struct_to_record(RecordName, Struct),
  % I use fun here in order to avoid possible variable collison by shaddowing them
  fun(ValuesByFieldsDict) ->
    % construct the tuple being the proper record from the struct
    list_to_tuple(
      %% first element in the tuple is record name
      [RecordName] ++
      lists:map(
        %% convention: json's *null* represents record's *undefined* value
        fun(Field) ->
          case dict:find(Field, ValuesByFieldsDict) of
            {ok, Value} -> Value;
            error -> undefined
          end  
        end,
        % getting the record field names in the order the tuple representing the record instance has its values
        record_info(fields, RecordName)
      )
    )
  end(
    % create quickly accessible maping of struct values by its keys turned to atoms, as it is in records.
    lists:foldl(
      fun({K, V}, Dict) ->
        dict:store(jsonerl:to_ex_a(K), V, Dict)
      end,
      dict:new(),
      tuple_to_list(Struct)
    )
  )
).

-define(record_to_json(RecordName, Record),
  % serialize erlang struct into json string
  jsonerl:encode(?record_to_struct(RecordName, Record))
).

-define(json_to_record(RecordName, Json),
  % decode json text to erlang struct
  ?struct_to_record(RecordName, jsonerl:decode(Json))
).
