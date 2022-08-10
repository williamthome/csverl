-module(csverl_utils).

-export([ binary_trim/1 ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Trims a binary.
%% @end
%%------------------------------------------------------------------------------
-spec binary_trim(binary()) -> binary().

binary_trim(Bin) when is_binary(Bin) ->
    do_binary_trim(Bin, init, <<>>, <<>>).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_binary_trim(<<" ", Bin/binary>>, init, <<>>, <<>>) ->
    do_binary_trim(Bin, init, <<>>, <<>>);
do_binary_trim(<<H, Bin/binary>>, init, <<>>, <<>>) ->
    do_binary_trim(Bin, acc, <<>>, <<H>>);
do_binary_trim(<<" ", Bin/binary>>, acc, Cache, Acc) ->
    do_binary_trim(Bin, cache, <<Cache/binary, " ">>, Acc);
do_binary_trim(<<H, Bin/binary>>, acc, Cache, Acc) ->
    do_binary_trim(Bin, acc, Cache, <<Acc/binary, H>>);
do_binary_trim(<<" ", Bin/binary>>, cache, Cache, Acc) ->
    do_binary_trim(Bin, cache, <<Cache/binary, " ">>, Acc);
do_binary_trim(<<H, Bin/binary>>, cache, Cache, Acc) ->
    do_binary_trim(Bin, acc, <<>>, <<Acc/binary, Cache/binary, H>>);
do_binary_trim(<<>>, _Cursor, _Cache, Acc) ->
    Acc.
