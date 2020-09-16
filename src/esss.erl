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

%% @doc Shamir secret sharing using GF^256.
%%
%% This module is an Erlang NIF binding to the "sss" secret-sharing library
%% written by Daan Sprenkels (https://github.com/dsprenkels/sss). The following
%% is taken from the library's README:
%%
%% sss is a library that exposes an API to split secret data buffers into a
%% number of different shares. With the possession of some or all of these
%% shares, the original secret can be restored. It is the schoolbook
%% example of a cryptographic threshold scheme.
%%
%% As often with crypto libraries, there is a lot of Shamir secret sharing
%% code around that does not meet cryptographic standards (a.k.a. is insecure).
%% Some details—like integrity checks and side-channel resistance—are often
%% forgotten. But these slip-ups can often fully compromise the security of
%% the scheme. With this in mind, I have made this library to:
%% <ul>
%%    <li>Be side channel resistant (timing, branch, cache)</li>
%%    <li>Secure the shared secret with a MAC</li>
%%    <li>Use the platform (OS) randomness source</li>
%% </ul>
%%
%% It should be safe to use this library in "the real world". I currently
%% regard the API as being stable.
-module(esss).

-export([create_shares/3, combine_shares/1, get_message_len/0]).

-export_type([message/0, share/0]).

-type message() :: iolist().
%% A secret message which can be split into shares. Messages must be of a
%% fixed length (which you can retrieve by calling
%% <code>get_message_len/0</code>).
-type share() :: binary().
%% A share of a secret message, used to recover it later.

%% @doc Returns the required length of a message().
%%
%% This is configurable, but only at compile-time by editing <code>sss.h</code>.
%%
%% Messages not of this length will produce a "badarg" exception when given
%% to <code>create_shares/3</code>.
-spec get_message_len() -> integer().
get_message_len() ->
    sss_nif:get_mlen().

%% @doc Create <code>N</code> shares of a secret message.
%%
%% Shares are created such that <code>K</code> or more shares will be able
%% to restore the secret.
-spec create_shares(message(), N :: integer(), K :: integer()) -> [share()].
create_shares(Message, N, K) ->
    sss_nif:create_shares(Message, N, K).

%% @doc Combine a list of shares returned by <code>create_shares/3</code> to
%%      restore the original secret.
%%
%% This function can fail if the shares are incorrectly formatted or sized, an
%% incorrect number of shares are given, or in the case of any form of
%% corruption of the shares (checked using a MAC over the original secret
%% message).
-spec combine_shares([share()]) -> {ok, message()} | error.
combine_shares(Shares) ->
    sss_nif:combine_shares(Shares, length(Shares)).
