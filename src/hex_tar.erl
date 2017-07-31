-module(hex_tar).
-include("hex_tar.hrl").
-include_lib("eunit/include/eunit.hrl").

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
%% * add option to `create` that keeps tarball on disk
%% * add `unpack` variant that saves files on disk
%% * verify that all required metadata fields are present
%% * warn on unknown metadata fields
%% * add function that verifies checksum
%% * ensure existing hex packages can be rebuilt and will have the same checksum
%% * add docs
%% * add typespes

%%====================================================================
%% API functions
%%====================================================================

-export([create/2, unpack/1]).

create(Meta, Files) ->
    {name, Name} = lists:keyfind(name, 1, Meta),
    {version, Version} = lists:keyfind(version, 1, Meta),
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
    {ok, Tar} = file:read_file(Path),
    file:delete(ContentsPath),
    file:delete(Path),
    {ok, {Tar, Checksum}}.

unpack(Tar) ->
    {Version, Checksum, MetaString, Contents} = do_unpack(Tar),
    ok = verify_version(Version),
    Meta = decode_meta(MetaString),
    {ok, Files} = hex_erl_tar:extract({binary, Contents}, [memory, compressed]),
    {ok, {Checksum, Meta, Files}}.

%%====================================================================
%% Internal functions
%%====================================================================

encode_meta(Meta) ->
    Data = lists:map(fun(MetaPair) ->
        String = io_lib_pretty:print(binarify(MetaPair), [{encoding, utf8}]),
        unicode:characters_to_binary([String, ".\n"])
      end, Meta),
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
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.

decode_meta(Binary) ->
    String = binary_to_list(Binary),
    {ok, Tokens, _Line} = safe_erl_term:string(String),
    Meta = safe_erl_term:terms(Tokens),

    %% FIXME: avoid binary_to_atom, use whitelist instead
    lists:map(fun({Key, Value}) -> {erlang:binary_to_atom(Key, unicode), Value} end, Meta).

checksum(MetaString, Contents) ->
    Blob = <<(?VERSION)/binary, MetaString/binary, Contents/binary>>,
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Blob),
    string:to_upper(lists:flatten(io_lib:format("~64.16.0b", [X]))).

do_unpack(Tar) ->
    {ok, Files} = hex_erl_tar:extract({binary, Tar}, [memory]),
    {"VERSION", Version} = lists:keyfind("VERSION", 1, Files),
    {"CHECKSUM", Checksum} = lists:keyfind("CHECKSUM", 1, Files),
    {"metadata.config", MetaString} = lists:keyfind("metadata.config", 1, Files),
    {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, Files),
    Checksum2 = binary_to_list(Checksum),
    {Version, Checksum2, MetaString, Contents}.

verify_version(?VERSION) -> ok;
verify_version(Version) -> {error, {unsupported_version, Version}}.
