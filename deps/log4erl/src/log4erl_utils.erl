-module(log4erl_utils).

-export([gen_log_txt/1,return_2columns/1, get_current_time/1, to_log/2]).
-export([get_id/0, get_month_name/1, get_month_long_name/1]).
-export([to_atom/1, to_list/1, to_int/1]).

% a function to make the log level text look pretty
gen_log_txt(L) when is_list(L) ->
    case string:len(L) of
	1 ->
	    "[" ++ L ++ "]    ";
	2 ->
	    "[" ++ L ++ "]   ";
	3 ->
	    "[" ++ L ++ "]  ";
	4 ->
	    "[" ++ L ++ "] ";
	_ ->
	    "[" ++ L ++ "]"
    end.

% a function to format date/time properly (e.g. 09 instead of 9)
return_2columns(X) ->
    case string:len(X) of
	1 ->
	    "0" ++ X;
	_ ->
	    X
    end.

% returns date/time as a properly formatted string (e.g. "01-01-2000 12:12:12")
%get_current_time() ->
get_current_time({{Y, M, D}, {H, Mi, S}}) ->
    %{{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    L = lists:map(fun(X) -> 
			  X2=integer_to_list(X), 
			  return_2columns(X2) 
		  end, 
		  [Y, M, D, H, Mi, S]
		 ),
    [Y2, M2, D2, H2, Mi2, S2] = L,
    Y2 ++ "-" ++ M2 ++ "-" ++ D2 ++ " " ++ H2 ++ ":" ++ Mi2 ++ ":" ++ S2.

%% DEBUG <- INFO <- WARN <- ERROR <- FATAL
%% user defined levels are always logged
to_log(Cur, Level) ->
    case Level of
	debug ->
	    true;
	info ->
	    ((Cur == info) or (Cur == warn) or (Cur == error) or (Cur == fatal));
	warn ->
	    ((Cur == warn) or (Cur == error) or (Cur == fatal));
	error ->
	    ((Cur == error) or (Cur == fatal));
	fatal ->
	    (Cur == fatal);
	none ->
	    false;
	_ ->
	    true
    end.

get_id() ->
    {_,_,N} = now(),
    Id = "log4erl_" ++ integer_to_list(random:uniform(N)),
    list_to_atom(Id).

get_month_name(Month) ->
    case Month of
	1 ->
	    "Jan";
	2 ->
	    "Feb";
	3 ->
	    "Mar";
	4 ->
	    "Apr";
	5 ->
	    "May";
	6 ->
	    "Jun";
	7 ->
	    "Jul";
	8 ->
	    "Aug";
	9 ->
	    "Sep";
	10 ->
	    "Oct";
	11 ->
	    "Nov";
	12 ->
	    "Dec"
    end.

get_month_long_name(Month) ->
    case Month of
	1 ->
	    "January";
	2 ->
	    "February";
	3 ->
	    "March";
	4 ->
	    "April";
	5 ->
	    "May";
	6 ->
	    "June";
	7 ->
	    "July";
	8 ->
	    "August";
	9 ->
	    "September";
	10 ->
	    "October";
	11 ->
	    "November";
	12 ->
	    "December"
    end.

to_list(A) when is_atom(A) ->
    atom_to_list(A);
to_list(A) when is_integer(A) ->
    integer_to_list(A);
to_list(A) when is_binary(A) ->
    binary_to_list(A);
to_list(A) when is_list(A) ->
    A.

to_atom(A) when is_list(A) ->
    list_to_atom(A);
to_atom(A) when is_binary(A) ->
    list_to_atom(binary_to_list(A));
to_atom(A) when is_atom(A) ->
    A.

to_int(A) when is_list(A) ->
    list_to_integer(A);
to_int(A) when is_integer(A) ->
    A.
