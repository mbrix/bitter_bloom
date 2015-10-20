-module(bitter_bloom_tests).
-author('mbranton@emberfinancial.com').


-include_lib("../include/bitter_bloom.hrl").
-include_lib("eunit/include/eunit.hrl").


start() -> ok.
stop(_) -> ok.



murmur() ->
	% format: expected, seed, data
    % see: https://github.com/bitcoin/bitcoin/blob/master/src/test/hash_tests.cpp
    D = [
      [16#00000000, 16#00000000, ""],
      [16#6a396f08, 16#FBA4C795, ""],
      [16#81f16f39, 16#ffffffff, ""],
      [16#514e28b7, 16#00000000, "00"],
      [16#ea3f0b17, 16#FBA4C795, "00"],
      [16#fd6cf10d, 16#00000000, "ff"],
      [16#16c6b7ab, 16#00000000, "0011"],
      [16#8eb51c3d, 16#00000000, "001122"],
      [16#b4471bf8, 16#00000000, "00112233"],
      [16#e2301fa8, 16#00000000, "0011223344"],
      [16#fc2e4a15, 16#00000000, "001122334455"],
      [16#b074502c, 16#00000000, "00112233445566"],
      [16#8034d2a0, 16#00000000, "0011223344556677"],
      [16#b4698def, 16#00000000, "001122334455667788"]
    ],
    lists:foreach(fun([Expected, Seed, Data]) ->
    					  ?assertEqual(Expected,
    					   erlang_murmurhash:murmurhash3_32(parse_hex(Data), Seed))
				  end, D).


murmur_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [
	{"Murmur tests", fun murmur/0}
   ]
  }.

filter_size() ->
      D = [
        [2, 0.001, 3, 8],
        [3, 0.01, 3, 5],
        [10, 0.2, 4, 2],
        [100, 0.2, 41, 2],
        [10000, 0.3, 3132, 1]
      ],
      lists:foreach(fun([NumElements, FalsePositiveRate, VLength, HashFuncs]) ->
							{ok, B} = bitter_bloom:new(NumElements, FalsePositiveRate),
							?assertEqual(byte_size(B#bbloom.vData), VLength),
							?assertEqual(B#bbloom.nHashFuncs, HashFuncs)
					end, D).


simple() ->
	%% test data from bitcoind
	%% see: https://github.com/bitcoin/bitcoin/blob/master/src/test/bloom_tests.cpp
	[A,B,C,D] = get_set(),
	{ok, F} = bitter_bloom:new(3, 0.01),
	{ok, F2} = bitter_bloom:insert(F, A),
	?assert(bitter_bloom:contains(F2, A)),
	?assertNot(bitter_bloom:contains(F2, B)),
	{ok, F3} = bitter_bloom:insert(F2, C),
	?assert(bitter_bloom:contains(F3, C)),
	{ok, F4} = bitter_bloom:insert(F3, D),
	?assert(bitter_bloom:contains(F4, D)),
	?assertEqual(#bbloom{vData = <<97,78,155>>,
						nHashFuncs = 5,
						nTweak = 0,
						nFlags = 0},
				F4).

ntweak() ->
	[A,B,C,D] = get_set(),
	{ok, F} = bitter_bloom:new(3, 0.01, 2147483649, 1),
	{ok, F2} = bitter_bloom:insert(F, A),
	?assert(bitter_bloom:contains(F2, A)),
	?assertNot(bitter_bloom:contains(F2, B)),
	{ok, F3} = bitter_bloom:insert(F2, C),
	?assert(bitter_bloom:contains(F3, C)),
	{ok, F4} = bitter_bloom:insert(F3, D),
	?assert(bitter_bloom:contains(F4, D)),
	?assertEqual(#bbloom{vData = <<206, 66, 153>>,
						nHashFuncs = 5,
						nTweak = 2147483649,
						nFlags = 1},
				F4).

public_keys() ->
	{ok, F}  = bitter_bloom:new(2, 0.001, 0, 1),
	{ok, F2} = bitter_bloom:insert(F, parse_hex("045b81f0017e2091e2edcd5eecf10d5bdd120a5514cb3ee65b8447ec18bfc4575c6d5bf415e54e03b1067934a0f0ba76b01c6b9ab227142ee1d543764b69d901e0")),
	{ok, F3} = bitter_bloom:insert(F2, parse_hex("477abbacd4113f2e6b100526222eedd953c26a64")),
	?assertEqual(#bbloom{vData = <<143, 193, 107>>,
						nHashFuncs = 8,
						nTweak = 0,
						nFlags = 1},
				F3).

max_vals() ->
	{ok, F} = bitter_bloom:new(900000000000000000000000000000000000, 0.01),
	?assertEqual(?MAX_BLOOM_FILTER_SIZE_BITS, byte_size(F#bbloom.vData)),
	{ok, F2} = bitter_bloom:new(10, 0.0000000000000001),
	?assertEqual(?MAX_HASH_FUNCS, F2#bbloom.nHashFuncs).

clear() ->
	[A,_,_,_] = get_set(),
	{ok, F} = bitter_bloom:new(1, 0.01),
	{ok, F2} = bitter_bloom:insert(F, A),
	?assert(bitter_bloom:contains(F2, A)),
	{ok, F3} = bitter_bloom:clear(F2),
	?assertNot(bitter_bloom:contains(F3, A)).

bloom_test_() -> 
  {foreach,
  fun start/0,
  fun stop/1,
   [
	{"Filter sizing", fun filter_size/0},
	{"Simple", fun simple/0},
	{"Ntweak serialization", fun ntweak/0},
	{"Public keys", fun public_keys/0},
	{"Clear filter", fun clear/0},
	{"Max vals", fun max_vals/0}
   ]
  }.


%% Utilities

parse_hex(S) when is_binary(S) ->
	parse_hex(erlang:binary_to_list(S));

parse_hex(S) ->
  parse_hex(S, []).
parse_hex([], Acc) ->
  list_to_binary(lists:reverse(Acc));
parse_hex([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  parse_hex(T, [V | Acc]).

get_set() ->
  [parse_hex("99108ad8ed9bb6274d3980bab5a85c048f0950c8"),
   parse_hex("19108ad8ed9bb6274d3980bab5a85c048f0950c8"),
   parse_hex("b5a2c786d9ef4658287ced5914b37a1b4aa32eee"),
   parse_hex("b9300670b4c5366e95b2699e8b18bc75e5f729c5")].
