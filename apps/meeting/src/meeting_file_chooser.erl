%%% @author     Yura Zhloba <yzh44yzh@gmail.com> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Choose file where to save conference
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(meeting_file_chooser).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-include_lib("eunit/include/eunit.hrl").

-export([get_for_writing/2, get_for_reading/2]).

get_for_writing(Dir, Name) ->
    get_for_writing(Dir, Name, fun file_exists/1).

get_for_writing(Dir, Name, CheckFun) ->
    get_for_writing(Dir, Name, CheckFun, 0).

get_for_writing(Dir, Name, CheckFun, Index) ->
    File = build_name(Dir, Name, Index),
    case CheckFun(File) of
        true -> get_for_writing(Dir, Name, CheckFun, Index + 1);
        false -> File
    end.

get_for_reading(Dir, Name) ->
    get_for_reading(Dir, Name, fun file_exists/1).

get_for_reading(Dir, Name, CheckFun) ->
  DirectPath = Dir ++ "/" ++ Name ++ ".flv",
  case CheckFun(DirectPath) of
    true -> DirectPath;
    false -> get_for_reading(Dir, Name, CheckFun, 0)
  end.

get_for_reading(Dir, Name, CheckFun, Index) ->
    File = build_name(Dir, Name, Index),
    case CheckFun(File) of
        true -> get_for_reading(Dir, Name, CheckFun, Index + 1);
        false -> case Index of
                     0 -> undefined;
                     _ -> build_name(Dir, Name, Index - 1)
                 end
    end.

build_name(Dir, Name, Index) ->
  Dir ++ "/" ++ Name ++ "-" ++ integer_to_list(Index) ++ ".flv".

file_exists(File) ->
    filelib:is_regular(File).

file_exists_mock(File) ->
    case File of
        "/tmp/conferences/conf1-0.flv" -> true;
        "/tmp/conferences/conf1-1.flv" -> true;
        "/tmp/conferences/conf1-2.flv" -> true;
        "/tmp/conferences/conf1-3.flv" -> true;
        "/tmp/conferences/conf2-0.flv" -> true;
        "/tmp/conferences/conf2-1.flv" -> true;
        _ -> false
    end.

get_for_writing_test() ->
    Dir = "/tmp/conferences",
    ?assertEqual(get_for_writing(Dir, "conf1", fun file_exists_mock/1), "/tmp/conferences/conf1-4.flv"),
    ?assertEqual(get_for_writing(Dir, "conf2", fun file_exists_mock/1), "/tmp/conferences/conf2-2.flv"),
    ?assertEqual(get_for_writing(Dir, "conf3", fun file_exists_mock/1), "/tmp/conferences/conf3-0.flv"),
    ok.

get_for_reading_test() ->
    Dir = "/tmp/conferences",
    ?assertEqual(get_for_reading(Dir, "conf1-2", fun file_exists_mock/1), "/tmp/conferences/conf1-2.flv"),
    ?assertEqual(get_for_reading(Dir, "conf1", fun file_exists_mock/1), "/tmp/conferences/conf1-3.flv"),
    ?assertEqual(get_for_reading(Dir, "conf2", fun file_exists_mock/1), "/tmp/conferences/conf2-1.flv"),
    ?assertEqual(get_for_reading(Dir, "conf3", fun file_exists_mock/1), undefined),
    ok.
