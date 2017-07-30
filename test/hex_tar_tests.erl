-module(hex_tar_tests).
-include_lib("eunit/include/eunit.hrl").

create_test() ->
    file:make_dir("tmp/"),
    ok = file:write_file("tmp/foo.erl", "-module(foo)."),
    ok = file:write_file("tmp/bar.erl", "-module(bar)."),

    Meta = [{name, "foo"}, {version, "1.0.0"}],
    Files = ["tmp/foo.erl", "tmp/bar.erl"],
    {ok, {Tar, Checksum}} = hex_tar:create(Meta, Files),

    {ok, {Checksum2, Meta2, Files2}} = hex_tar:unpack(Tar),
    Checksum = Checksum2,
    [
     {name, <<"foo">>},
     {version, <<"1.0.0">>}
    ] = Meta2,
    [
     {"tmp/foo.erl",<<"-module(foo).">>},
     {"tmp/bar.erl",<<"-module(bar).">>}
    ] = Files2.
