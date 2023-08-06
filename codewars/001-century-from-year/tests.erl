-module(kata_tests).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {"Basic tests", [
        {"Testing for 1705", ?_assertEqual(18, kata:century(1705))},
        {"Testing for 1900", ?_assertEqual(19, kata:century(1900))},
        {"Testing for 1601", ?_assertEqual(17, kata:century(1601))},
        {"Testing for 2000", ?_assertEqual(20, kata:century(2000))},
        {"Testing for 89", ?_assertEqual(1, kata:century(89))}
    ]}.
