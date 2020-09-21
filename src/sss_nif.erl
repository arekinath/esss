%%
%% erlang NIF binding for dsprenkel's sss library
%%
%% Copyright 2020 Alex Wilson <alex@uq.edu.au>, The University of Queensland
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

%% @private
-module(sss_nif).

-on_load(init/0).

-export([get_mlen/0]).
-export([create_shares/3, combine_shares/2]).
-export([create_keyshares/3, combine_keyshares/2]).

try_paths([Last], BaseName) ->
    filename:join([Last, BaseName]);
try_paths([Path | Next], BaseName) ->
    case filelib:is_dir(Path) of
        true ->
            WCard = filename:join([Path, "{lib,}" ++ BaseName ++ ".*"]),
            case filelib:wildcard(WCard) of
                [] -> try_paths(Next, BaseName);
                _ -> filename:join([Path, BaseName])
            end;
        false -> try_paths(Next, BaseName)
    end.

init() ->
    Paths0 = [
        filename:join(["..", lib, esss, priv]),
        filename:join(["..", priv]),
        filename:join([priv])
    ],
    Paths1 = case code:priv_dir(esss) of
        {error, bad_name} -> Paths0;
        Dir -> [Dir | Paths0]
    end,
    SoName = try_paths(Paths1, "sss_nif"),
    erlang:load_nif(SoName, 0).

-type key() :: iolist().
%% Length must be 32 bytes exactly.

-type n() :: integer().
%% The total number of shares.

-type k() :: integer().
%% The threshold (number of shares required to recover).

-type keyshares() :: [binary()].
%% Containing concatenated key shares

-type message() :: iolist().
%% Must be get_mlen() bytes long.

-spec get_mlen() -> integer().
get_mlen() -> error(no_nif).

-spec create_shares(message(), n(), k()) -> keyshares().
create_shares(_Msg, _N, _K) -> error(no_nif).

-spec combine_shares(keyshares(), n()) -> {ok, message()} | error.
combine_shares(_KeyShares, _N) -> error(no_nif).

-spec create_keyshares(key(), n(), k()) -> keyshares().
create_keyshares(_Key, _N, _K) -> error(no_nif).

-spec combine_keyshares(keyshares(), n()) -> key().
combine_keyshares(_KeyShares, _N) -> error(no_nif).
