-module(hex_tar).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(VERSION, <<"3">>).
-define(MAX_SIZE, (8 * 1024 * 1024)).
-define(REQUIRED_FILES, lists:sort(["VERSION", "CHECKSUM", "metadata.config", "contents.tar.gz"])).
-define(METADATA_REQUIRED_FIELDS, lists:sort([name, version, app, description, files, licenses, requirements, build_tools])).
-define(METADATA_OPTIONAL_FIELDS, lists:sort([elixir, maintainers, links, extra])).
-define(REQUIREMENT_REQUIRED_FIELDS, lists:sort([app, optional, requirement])).
-define(REQUIREMENT_OPTIONAL_FIELDS, lists:sort([repository])).

%% create/2, encode_meta/1 based on [1]
%% binarify/1 based on [2]
%% unpack/1 based on [3] and [4]
%%
%% [1] https://github.com/hexpm/rebar3_hex/blob/v2.5.0/src/rebar3_hex_tar.erl
%% [2] https://github.com/hexpm/rebar3_hex/blob/v2.5.0/src/rebar3_hex_utils.erl#L41:L57
%% [3] https://github.com/erlang/rebar3/blob/3.4.2/src/rebar_pkg_resource.erl#L90:L96
%% [4] https://github.com/hexpm/hex/blob/v0.16.1/lib/hex/tar.ex

%% TODO:
%%
%% * add function that verifies checksum
%% * ensure existing hex packages can be rebuilt and will have the same checksum
%% * add docs
%% * add typespes
%% * (maybe) perform stricter validations on shape of requirements/links/extra etc

%%====================================================================
%% API functions
%%====================================================================

-export([create/2, create/3, unpack/1, unpack/2]).

% Examples:
%
%     {ok, {Tar, Checksum}} = hex_tar:create(Meta, ["src/foo.erl"]).
%
%     {ok, {Tar, Checksum}} = hex_tar:create(Meta, [{"src/foo.erl", "tmp/foo/src/foo.erl"}]).
%
%     {ok, {Tar, Checksum}} = hex_tar:create(Meta, [{"src/foo.erl", "-module(foo)."}]).
%
create(Meta, Files) ->
    create(Meta, Files, []).

% Examples:
%
%     {ok, {Tar, Checksum}} = hex_tar:create(Meta, ["src/foo.erl"], [keep_tarball]).
%
create(Meta, Files, Options) ->
    ok = verify_meta(Meta),
    #{name := Name, version := Version} = Meta,
    ContentsPath = io_lib:format("~s-~s-contents.tar.gz", [Name, Version]),
    Path = io_lib:format("~s-~s.tar", [Name, Version]),
    ok = hex_erl_tar:create(ContentsPath, Files, [compressed]),

    {ok, Contents} = file:read_file(ContentsPath),
    MetaString = encode_meta(Meta),
    Checksum = checksum(MetaString, Contents),

    MetaFiles = [
                 {"VERSION", ?VERSION},
                 {"CHECKSUM", list_to_binary(Checksum)},
                 {"metadata.config", MetaString},
                 {"contents.tar.gz", Contents}
                ],

    ok = hex_erl_tar:create(Path, MetaFiles),
    ok = verify_size(Path),
    {ok, Tar} = file:read_file(Path),
    file:delete(ContentsPath),

    proplists:get_bool(keep_tarball, Options) orelse file:delete(Path),

    {ok, {Tar, Checksum}}.

% Examples:
%
%     {ok, {Checksum, Meta, Files}} = hex_tar:unpack({binary, Tar}).
%
%     {ok, {Checksum, Meta, Files}} = hex_tar:unpack("foo-1.0.0.tar").
%
unpack(Tar) ->
    {Checksum, Meta, Contents} = do_unpack(Tar),
    {ok, Files} = hex_erl_tar:extract({binary, Contents}, [memory, compressed]),
    {ok, {Checksum, Meta, Files}}.

% Examples:
%
%     {ok, {Checksum, Meta}} = hex_tar:unpack({binary, Tar}, [{destination, "/tmp/foo/"}]).
%
unpack(Tar, [{destination, Destination}]) ->
    {Checksum, Meta, Contents} = do_unpack(Tar),
    ok = hex_erl_tar:extract({binary, Contents}, [compressed, {cwd, Destination}]),
    {ok, {Checksum, Meta}}.

%%====================================================================
%% Internal functions
%%====================================================================

encode_meta(Meta) ->
    Data = lists:map(fun(MetaPair) ->
        String = io_lib_pretty:print(binarify(MetaPair), [{encoding, utf8}]),
        unicode:characters_to_binary([String, ".\n"])
      end, maps:to_list(Meta)),
    iolist_to_binary(Data).

binarify(Term) when is_boolean(Term) ->
    Term;
binarify(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
binarify([]) ->
    [];
binarify(Term) when is_list(Term) ->
    case io_lib:printable_list(Term) of
        true ->
            list_to_binary(Term);
        false ->
            [binarify(X) || X <- Term]
    end;
binarify(Term) when is_map(Term) ->
    List = maps:to_list(Term),
    lists:map(fun({K, V}) -> binarify({K, V}) end, List);
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.

decode_meta(Binary) when is_binary(Binary) ->
    String = binary_to_list(Binary),
    {ok, Tokens, _Line} = safe_erl_term:string(String),
    List = safe_erl_term:terms(Tokens),
    List2 = lists:map(fun({Key, Val}) -> decode_meta({Key, Val}) end, List),
    maps:from_list(List2);
decode_meta({<<"requirements">>, Value}) when is_list(Value) ->
    {requirements, decode_requirements(Value)};
decode_meta({<<"links">>, Value}) when is_list(Value) ->
    {links, maps:from_list(Value)};
decode_meta({<<"extra">>, Value}) when is_list(Value) ->
    {extra, maps:from_list(Value)};
decode_meta({Key, Value}) ->
    %% FIXME: avoid binary_to_atom, use whitelist instead
    {erlang:binary_to_atom(Key, unicode), Value}.

decode_requirements(Requirements) ->
    List = lists:map(fun({Name, Requirement}) ->
                             {Name, maps:from_list(lists:map(fun(X) -> decode_meta(X) end, Requirement))}
                     end, Requirements),
    maps:from_list(List).

checksum(MetaString, Contents) ->
    Blob = <<(?VERSION)/binary, MetaString/binary, Contents/binary>>,
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Blob),
    string:to_upper(lists:flatten(io_lib:format("~64.16.0b", [X]))).

do_unpack(Tar) ->
    ok = verify_size(Tar),
    {ok, Files} = hex_erl_tar:extract(Tar, [memory]),
    {"VERSION", Version} = lists:keyfind("VERSION", 1, Files),
    {"CHECKSUM", Checksum} = lists:keyfind("CHECKSUM", 1, Files),
    {"metadata.config", MetaString} = lists:keyfind("metadata.config", 1, Files),
    {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, Files),
    Checksum2 = binary_to_list(Checksum),
    ok = verify_version(Version),
    ok = verify_files(Files),
    Meta = decode_meta(MetaString),
    ok = verify_meta(Meta),
    {Checksum2, Meta, Contents}.

verify_size({binary, Binary}) ->
    verify_size(byte_size(Binary));
verify_size(Filename) when is_list(Filename) ->
    verify_size(filelib:file_size(Filename));
verify_size(Size) when Size =< ?MAX_SIZE ->
    ok;
verify_size(_) ->
    {error, too_big}.

verify_version(?VERSION) -> ok;
verify_version(Version) -> {error, {unsupported_version, Version}}.

verify_files(Files) ->
    Filenames = lists:sort(proplists:get_keys(Files)),
    verify_files(Filenames, ?REQUIRED_FILES).

verify_files(Filenames, Filenames) -> ok;
verify_files(Filenames, _) -> {error, {invalid_files, Filenames}}.

verify_meta(Meta) ->
    ok = verify_fields(Meta, ?METADATA_REQUIRED_FIELDS, ?METADATA_OPTIONAL_FIELDS),
    #{requirements := Requirements} = Meta,
    ok = verify_requirements(Requirements).

verify_requirements(_) ->
    ok.

verify_fields(Map, RequiredFields, OptionalFields) ->
    Fields = maps:keys(Map),
    RequiredFieldsDiff = RequiredFields -- Fields,
    UnknownFields = Fields -- (RequiredFields ++ OptionalFields),

    case {RequiredFieldsDiff, UnknownFields} of
        {[], []} -> ok;
        {RequiredFieldsDiff, []} -> {error, {missing_required_fields, RequiredFieldsDiff}};
        {_, UnknownFields} -> {error, {unknown_fields, UnknownFields}}
    end.
