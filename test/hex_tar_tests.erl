-module(hex_tar_tests).
-include_lib("eunit/include/eunit.hrl").

in_memory_test() ->
    %% create
    Meta = [{name, <<"foo">>}, {version, <<"1.0.0">>}],
    Files = [
     {"tmp/foo.erl",<<"-module(foo).">>},
     {"tmp/bar.erl",<<"-module(bar).">>}
    ],
    {ok, {Tar, Checksum}} = hex_tar:create(Meta, Files),

    %% extract
    {ok, {Checksum2, Meta2, Files2}} = hex_tar:unpack(Tar),
    Checksum = Checksum2,
    Meta = Meta2,
    Files = Files2,

    %% create again
    {ok, {_Tar3, Checksum3}} = hex_tar:create(Meta2, Files2),
    Checksum2 = Checksum3.
