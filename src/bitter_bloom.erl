%% Bitcoin compatible bloom filter using murmurhash3
-module(bitter_bloom).
-author('mbranton@emberfinancial.com').

-behaviour(application).
-export([start/0, start/2, stop/0, stop/1]).

-behaviour(supervisor).
-export([init/1]).

-export([new/2,
		 new/4,
		 is_bloom/1,
		 clear/1,
		 insert/2,
		 contains/2,
		 create/4]).

-include_lib("bitter_bloom.hrl").
-include_lib("eunit/include/eunit.hrl").


%% Application behaviour
start() -> application:ensure_all_started(bitter_bloom).
start(_, _) -> supervisor:start_link({local,?MODULE},?MODULE,[]).
stop() -> application:stop(bitter_bloom).
stop(_) -> ok.

init([]) -> {ok, {{one_for_one,3,10},[]}}.
%% End application behaviour

new(NumElements, FalsePositiveRate) ->
	new(NumElements, FalsePositiveRate, 0, ?BLOOM_UPDATE_NONE).
new(NumElements, FalsePositiveRate, NTweak, NFlags) ->
	NE = erlang:abs(NumElements),
	FP = erlang:abs(FalsePositiveRate),
	try
	Size = -1.0 / ?LN2SQUARED * NE * math:log(FP),
	FilterSize = lessthan_of(floor(Size / 8), ?MAX_BLOOM_FILTER_SIZE * 8),
	VData = <<0:(FilterSize*8)>>,
	NHashFuncs = greater_than(lessthan_of(floor(size(VData)*8 / NE * ?LN2),
										  ?MAX_HASH_FUNCS), ?MIN_HASH_FUNCS),
	create(vdata(VData), NHashFuncs, NTweak, NFlags)
	catch _:_ ->
			  create(<<0>>, 1, NTweak, NFlags)
	end.

vdata(<<>>) -> <<0>>;
vdata(Other) -> Other.

is_bloom(#bbloom{}) -> true;
is_bloom(_) -> false.

create(VData, _NHashFuncs, _NTweak, _NFlags) when size(VData) > ?MAX_BLOOM_FILTER_SIZE_BITS -> error;
create(_Vdata, NHashFuncs, _NTweak, _NFlags) when NHashFuncs < 1 -> error;
create(_VData, NHashFuncs, _NTweak, _NFlags) when NHashFuncs > ?MAX_HASH_FUNCS -> error;
create(VData, NHashFuncs, NTweak, NFlags) ->
	{ok, #bbloom{vData = VData,
		    nHashFuncs = NHashFuncs,
		    nTweak = NTweak,
		    nFlags = NFlags}}.

%% returns index

%% A size of 0 will return NaN in javascript, lets return 0.
hash(0, _, _, _) -> 0;
hash(Size, NHashNum, NTweak, HashData) ->
	erlang_murmurhash:murmurhash3_32(HashData,
					((NHashNum * 16#FBA4C795) + NTweak) band 16#FFFFFFFF) rem Size.

insert(B, Element) -> insert(B, Element, 0, B#bbloom.nHashFuncs).

insert(B, _Element, _N, _N) -> {ok, B};
insert(B, Element, N, Z) ->
	I = hash(byte_size(B#bbloom.vData)*8, N, B#bbloom.nTweak, Element),
	Pos = (1 bsl (7 band I)),
	Index = (I bsr 3),
	<<Pre:(Index)/binary, Byte:8, Post/binary>> = B#bbloom.vData,
	insert(B#bbloom{vData = <<Pre/binary, (Byte bor Pos):8, Post/binary>>}, Element, N+1, Z).

contains(#bbloom{vData = <<>>}, _Element) -> false;
contains(B, Element) -> contains(B, Element, 0, B#bbloom.nHashFuncs).

contains(_B, _Element, _N, _N) -> true;
contains(B, Element, N, Z) ->
	I = hash(byte_size(B#bbloom.vData)*8, N, B#bbloom.nTweak, Element),
	Index = (I bsr 3),
	Pos = (1 bsl (7 band I)),
	<<_:(Index)/binary, Byte:8, _/binary>> = B#bbloom.vData,
	if ((bnot (Byte band Pos)) =:= -1) -> false;
	   true -> contains(B, Element, N+1, Z)
	end.


clear(B) when is_record(B, bbloom) -> {ok, B#bbloom{vData = <<>>}}.


%% Utility functions
%%

lessthan_of(0, _) -> 0;
lessthan_of(X,Y) when Y < X -> Y;
lessthan_of(X,_) -> X.

greater_than(X,Y) when Y > X -> Y;
greater_than(X,_) -> X.

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> trunc(X).
