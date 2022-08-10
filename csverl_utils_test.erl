-module(csverl_utils_test).

-include_lib("eunit/include/eunit.hrl").

binary_trim_test() ->
    [
        ?assertEqual(<<>>, csverl_utils:binary_trim(<<>>)),
        ?assertEqual(<<"foo">>, csverl_utils:binary_trim(<<"foo">>)),
        ?assertEqual(<<"foo">>, csverl_utils:binary_trim(<<"  foo">>)),
        ?assertEqual(<<"foo">>, csverl_utils:binary_trim(<<"foo  ">>)),
        ?assertEqual(<<"foo">>, csverl_utils:binary_trim(<<"  foo  ">>)),
        ?assertEqual(<<"foo bar">>, csverl_utils:binary_trim(<<"  foo bar  ">>))
    ].
