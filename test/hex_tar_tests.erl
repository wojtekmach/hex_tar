-module(hex_tar_tests).
-include_lib("eunit/include/eunit.hrl").

in_memory_test() ->
    %% create
    Meta = [{name, <<"foo">>}, {version, <<"1.0.0">>}],
    Files = [
             {"foo.erl", <<"-module(foo).">>},
             {"bar.erl", <<"-module(bar).">>}
            ],
    {ok, {Tar, Checksum}} = hex_tar:create(Meta, Files),

    %% extract
    {ok, {Checksum2, Meta2, Files2}} = hex_tar:unpack({binary, Tar}),
    Checksum = Checksum2,
    Meta = Meta2,
    Files = Files2,

    %% create again
    {ok, {_Tar3, Checksum3}} = hex_tar:create(Meta2, Files2),
    Checksum2 = Checksum3.

disk_test() ->
    file:make_dir("tmp"),
    file:make_dir("tmp/pkg"),
    ok = file:write_file("tmp/pkg/foo.erl", <<"-module(foo).">>),
    ok = file:write_file("tmp/pkg/bar.erl", <<"-module(bar).">>),

    %% create
    Meta = [{name, <<"foo">>}, {version, <<"1.0.0">>}],
    Files = [
             {"foo.erl", "tmp/pkg/foo.erl"},
             {"bar.erl", "tmp/pkg/bar.erl"}
            ],
    {ok, {Tar, Checksum}} = hex_tar:create(Meta, Files),

    %% extract
    {ok, {Checksum2, Meta2}} = hex_tar:unpack({binary, Tar}, [{destination, "tmp/pkg_extracted"}]),
    Checksum = Checksum2,
    Meta = Meta2,
    {ok, <<"-module(foo).">>} = file:read_file("tmp/pkg_extracted/foo.erl"),
    {ok, <<"-module(bar).">>} = file:read_file("tmp/pkg_extracted/bar.erl"),

    %% create again
    Files2 = [
              {"foo.erl", "tmp/pkg_extracted/foo.erl"},
              {"bar.erl", "tmp/pkg_extracted/bar.erl"}
             ],
    {ok, {_Tar3, Checksum3}} = hex_tar:create(Meta2, Files2),
    Checksum2 = Checksum3.
