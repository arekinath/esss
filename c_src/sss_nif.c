/*
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
*/

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <stdint.h>
#include <time.h>
#include <sys/time.h>

#include "sss/sss.h"
#include "sss/hazmat.h"

#include "erl_nif.h"

static ERL_NIF_TERM
sss_nif_get_mlen(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	return (enif_make_uint(env, sss_MLEN));
}

static ERL_NIF_TERM
sss_nif_create_shares(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int n, k, i;
	ErlNifBinary bin;
	sss_Share *out;
	ERL_NIF_TERM outbin, list, tmp;

	if (!enif_get_uint(env, argv[1], &n))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, argv[2], &k))
		return (enif_make_badarg(env));
	if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
		return (enif_make_badarg(env));
	if (bin.size != sss_MLEN)
		return (enif_make_badarg(env));

	out = (sss_Share *)enif_make_new_binary(env,
	    n * sizeof (sss_Share), &outbin);

	sss_create_shares(out, bin.data, n, k);

	list = enif_make_list(env, 0);
	for (i = 0; i < n; ++i) {
		tmp = enif_make_sub_binary(env, outbin,
		    i * sizeof (sss_Share), sizeof (sss_Share));
		list = enif_make_list_cell(env, tmp, list);
	}

	return (list);
}

static ERL_NIF_TERM
sss_nif_combine_shares(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int k;
	ErlNifBinary bin;
	uint8_t *out;
	sss_Share *shares;
	ERL_NIF_TERM ret;

	if (!enif_get_uint(env, argv[1], &k))
		return (enif_make_badarg(env));
	if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
		return (enif_make_badarg(env));
	if (bin.size != (sizeof (sss_Share) * k))
		return (enif_make_badarg(env));

	shares = (sss_Share *)bin.data;
	out = enif_make_new_binary(env, sss_MLEN, &ret);

	sss_combine_shares(out, shares, k);

	return (ret);
}

static ERL_NIF_TERM
sss_nif_create_keyshares(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int n, k, i;
	ErlNifBinary bin;
	sss_Keyshare *out;
	ERL_NIF_TERM outbin, list, tmp;

	if (!enif_get_uint(env, argv[1], &n))
		return (enif_make_badarg(env));
	if (!enif_get_uint(env, argv[2], &k))
		return (enif_make_badarg(env));
	if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
		return (enif_make_badarg(env));
	if (bin.size != 32)
		return (enif_make_badarg(env));

	out = (sss_Keyshare *)enif_make_new_binary(env,
	    n * sizeof (sss_Keyshare), &outbin);

	sss_create_keyshares(out, bin.data, n, k);

	list = enif_make_list(env, 0);
	for (i = 0; i < n; ++i) {
		tmp = enif_make_sub_binary(env, outbin,
		    i * sizeof (sss_Keyshare), sizeof (sss_Keyshare));
		list = enif_make_list_cell(env, tmp, list);
	}

	return (list);
}

static ERL_NIF_TERM
sss_nif_combine_keyshares(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int k;
	ErlNifBinary bin;
	uint8_t *out;
	sss_Keyshare *shares;
	ERL_NIF_TERM ret;

	if (!enif_get_uint(env, argv[1], &k))
		return (enif_make_badarg(env));
	if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
		return (enif_make_badarg(env));
	if (bin.size != (sizeof (sss_Keyshare) * k))
		return (enif_make_badarg(env));

	shares = (sss_Keyshare *)bin.data;
	out = enif_make_new_binary(env, 32, &ret);

	sss_combine_keyshares(out, shares, k);

	return (ret);
}

static int
sss_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM info)
{
	return (0);
}

static void
sss_nif_unload(ErlNifEnv *env, void *priv_data)
{
}

static ErlNifFunc nif_funcs[] = {
	{ "get_mlen", 0, sss_nif_get_mlen },
	{ "create_shares", 3, sss_nif_create_shares },
	{ "combine_shares", 2, sss_nif_combine_shares },
	{ "create_keyshares", 3, sss_nif_create_keyshares },
	{ "combine_keyshares", 2, sss_nif_combine_keyshares }
};

ERL_NIF_INIT(sss_nif, nif_funcs, sss_nif_load, NULL, NULL, sss_nif_unload);
