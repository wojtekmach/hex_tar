-module(hex_tar_tests).
-include_lib("eunit/include/eunit.hrl").

create_test() ->
    ok = hex_tar:create().
