-author('mbranton@emberfinancial.com').

-record(bbloom, {vData,     %% Bloom filter data
				nHashFuncs, %% The number of hash functions
				nTweak,     %% A random value to seed the hash function
				nFlags}).    %% A flag to determine how matched items are added

-define(BLOOM_UPDATE_NONE, 0).
-define(BLOOM_UPDATE_ALL,  1).
-define(BLOOM_UPDATE_P2PUBKEY_ONLY, 2).
-define(MAX_BLOOM_FILTER_SIZE, 36000). %% bytes
-define(MAX_BLOOM_FILTER_SIZE_BITS, 288000). %% bits
-define(MAX_HASH_FUNCS, 50).
-define(MIN_HASH_FUNCS, 1).

-define(LN2SQUARED, math:pow(math:log(2), 2)).
-define(LN2, math:log(2)).
