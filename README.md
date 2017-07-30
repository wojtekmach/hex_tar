HexTar
======

**Warning: Work in progress**

An implementation of Hex Package Tarball specification [1].

Uses code from `hex`, `rebar3` and `rebar3_hex` and hopefully all these projects
will eventually use `hex_tar` instead.

[1] <https://github.com/hexpm/specifications/blob/master/package_tarball.md>

Example
-------

Let's grab a package that's hosted on Hex.pm:

```
$ git clone https://github.com/wojtekmach/hex_tar
$ cd hex_tar
$ rebar3 shell
```

```erlang
Url = "https://repo.hex.pm/tarballs/mime-1.1.0.tar",
{ok, {{_, 200, _}, _Headers, Tar}} = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
{ok, {Checksum, Meta, Files}} = hex_tar:unpack(Tar).
```

Inspect package metadata:

```erlang
proplists:get_value(description, Meta).
%% => <<"A MIME type module for Elixir">>
```

Inspect files:

```erlang
proplists:get_keys(Files).
%% => ["README.md","mix.exs","lib/mime.ex","lib/mime.types", "LICENSE"]

proplists:get_value("LICENSE", Files).
%% => <<"Copyright (c) 2016 Plataformatec.\n\n...>>
```

Build package tarball using package metadata and files:

```erlang
{ok, {Tar2, Checksum2}} = hex_tar:create(Meta, Files).
```
