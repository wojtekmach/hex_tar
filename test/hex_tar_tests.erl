-module(hex_tar_tests).
-include_lib("eunit/include/eunit.hrl").

fixture_meta() ->
    #{
        app => <<"foo">>,
        name => <<"foo">>,
        version => <<"1.0.0">>,
        description => <<"description">>,
        build_tools => [rebar3],
        files => [<<"rebar.config">>, <<"foo.erl">>, <<"bar.erl">>],
        licenses => [<<"Apache 2.0">>],
        maintainers => [<<"Wojtek Mach">>],
        requirements => #{
            <<"bar">> => #{
                app => <<"bar">>,
                optional => false,
                requirement => <<"~> 0.1">>
            }
        },
        links => #{<<"GitHub">> => <<"https://github.com/hexpm/foo">>},
        extra => #{<<"foo">> => <<"bar">>}
    }.

in_memory_test() ->
    %% create
    Meta = fixture_meta(),
    Files = [
             {"rebar.config", <<"">>},
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
    Checksum2 = Checksum3,
    ok.

disk_test() ->
    file:make_dir("tmp"),
    file:make_dir("tmp/pkg"),
    ok = file:write_file("tmp/pkg/rebar.config", <<"">>),
    ok = file:write_file("tmp/pkg/foo.erl", <<"-module(foo).">>),
    ok = file:write_file("tmp/pkg/bar.erl", <<"-module(bar).">>),

    %% create
    Meta = fixture_meta(),
    Files = [
             {"rebar.config", "tmp/pkg/rebar.config"},
             {"foo.erl", "tmp/pkg/foo.erl"},
             {"bar.erl", "tmp/pkg/bar.erl"}
            ],
    {ok, {_Tar, Checksum}} = hex_tar:create(Meta, Files, [keep_tarball]),

    %% extract
    {ok, {Checksum2, Meta2}} = hex_tar:unpack("foo-1.0.0.tar", [{destination, "tmp/pkg_extracted"}]),
    Checksum = Checksum2,
    Meta = Meta2,
    {ok, <<"">>} = file:read_file("tmp/pkg_extracted/rebar.config"),
    {ok, <<"-module(foo).">>} = file:read_file("tmp/pkg_extracted/foo.erl"),
    {ok, <<"-module(bar).">>} = file:read_file("tmp/pkg_extracted/bar.erl"),
    true = filelib:is_file("tmp/pkg_extracted/hex_metadata.config"),

    %% create again
    Files2 = [
              {"rebar.config", "tmp/pkg_extracted/rebar.config"},
              {"foo.erl", "tmp/pkg_extracted/foo.erl"},
              {"bar.erl", "tmp/pkg_extracted/bar.erl"}
             ],
    {ok, {_Tar3, Checksum3}} = hex_tar:create(Meta2, Files2),
    Checksum2 = Checksum3,

    %% cleanup
    file:delete("foo-1.0.0.tar").

guess_build_tools_test() ->
    Meta = fixture_meta(),

    Meta2 = maps:put(files, [<<"rebar.config">>], maps:remove(build_tools, Meta)),
    Meta2Result = roundtrip_meta(Meta2, [{"rebar.config", <<"">>}]),
    [rebar3] = maps:get(build_tools, Meta2Result),

    Meta3 = maps:put(files, [], maps:remove(build_tools, Meta)),
    Meta3Result = roundtrip_meta(Meta3, []),
    [] = maps:get(build_tools, Meta3Result),

    ok.

roundtrip_meta(Meta, Files) ->
    {ok, {Tar, _Checksum}} = hex_tar:create(Meta, Files),
    {ok, {_Checksum, Meta2, _Files}} = hex_tar:unpack({binary, Tar}),
    Meta2.
