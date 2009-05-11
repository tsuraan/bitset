-module(bstest).
-compile(export_all).

-define(SET, gb_sets).

make_bs(Bs, -1) -> Bs;
make_bs(Bs, Count) -> 
  %io:format("~p~n", [Count]),
  make_bs(bitset:insert(Count, Bs), Count-1).

verify_bs(_Bs, -1) -> ok;
verify_bs(Bs, Number) ->
  true = bitset:contains(Number, Bs),
  verify_bs(Bs, Number-1).

speedtest() ->
  Size = 1000000,
  { MTime, Bs } = timer:tc(bstest, make_bs, [ bitset:new(), Size ]),
  { VTime, ok } = timer:tc(bstest, verify_bs, [ Bs, Size ]),
  io:format("Created bitset with ~p elements in ~p seconds~n", 
    [ Size, MTime/1000000 ]),
  io:format("Verified contents of bitset in ~p seconds~n", [ VTime/1000000 ]),
  ok.

random_list(0, Lst) ->
  Lst;
random_list(Count, Lst) ->
  random_list(Count - 1, [ random:uniform(10000000) | Lst ]).

random_set(0, Set) ->
  Set;
random_set(Count, Set) ->
  random_set(Count - 1, ?SET:add_element(random:uniform(10000000), Set)).

assert_out([], _Bs) -> ok;
assert_out([Head | Tail], Bs) ->
  false = bitset:contains(Head, Bs),
  assert_out(Tail, Bs).

assert_in([], _Bs) -> ok;
assert_in([Head | Tail], Bs) ->
  true = bitset:contains(Head, Bs),
  assert_in(Tail, Bs).

scramble_list(0, Lst) -> Lst;
scramble_list(N, Lst) ->
  scramble_list(N-1, lists:sort(fun(_, _) -> random:uniform(2) =:= 1 end, Lst)).

neg_test(Entries, Tests, Bs) ->
  Out  = ?SET:subtract(random_set(Tests, ?SET:new()), ?SET:from_list(Entries)),
  ok = assert_out(?SET:to_list(Out), Bs).

fuzz_simple() ->
  io:format("Making random set for inclusion~n"),
  In   = random_set(100000, ?SET:new()),
  io:format("Shuffling list~n"),
  InL  = scramble_list(10, ?SET:to_list(In)),
  io:format("Making bitset~n"),
  Bs   = bitset:new(InL),
  io:format("Checking that all entries are in~n"),
  ok = assert_in(InL, Bs),
  io:format("Checking that non-entries are not in~n"),
  ok = neg_test(InL, 10000, Bs).

fuzz_intersect() ->
  io:format("Making sets~n"),
  S1 = random_set(100000, ?SET:new()),
  S2 = random_set(100000, ?SET:new()),
  S3 = random_set(100000, ?SET:new()),
  %S4 = random_set(100000, ?SET:new()),
  
  io:format("Making bitsets~n"),
  B1 = bitset:new(?SET:to_list(S1)),
  B2 = bitset:new(?SET:to_list(S2)),
  B3 = bitset:new(?SET:to_list(S3)),
  %B4 = bitset:new(?SET:to_list(S4)),

  I1 = ?SET:intersection(S1, S2),
  io:format("First intersection size is ~p~n", [?SET:size(I1)]),
  ok = assert_in(?SET:to_list(I1), bitset:intersect([B1,B2])),
  ok = neg_test(?SET:to_list(I1), 100000, bitset:intersect([B1, B2])),

  I2 = ?SET:intersection([S1, S2, S3]),
  io:format("Second intersection size is ~p~n", [?SET:size(I2)]),
  ok = assert_in(?SET:to_list(I2), bitset:intersect([B1,B2,B3])),
  ok = neg_test(?SET:to_list(I2), 100000, bitset:intersect([B1, B2, B3])).

fuzz_union() ->
  io:format("Making sets~n"),
  S1 = random_set(100000, ?SET:new()),
  S2 = random_set(100000, ?SET:new()),
  S3 = random_set(100000, ?SET:new()),

  io:format("Making bitsets~n"),
  B1 = bitset:new(?SET:to_list(S1)),
  B2 = bitset:new(?SET:to_list(S2)),
  B3 = bitset:new(?SET:to_list(S3)),

  I1 = ?SET:union(S1, S2),
  io:format("First union size is ~p~n", [?SET:size(I1)]),
  ok = assert_in(?SET:to_list(I1), bitset:union([B1,B2])),
  ok = neg_test(?SET:to_list(I1), 100000, bitset:union([B1, B2])),

  I2 = ?SET:union([S1, S2, S3]),
  io:format("Second union size is ~p~n", [?SET:size(I2)]),
  ok = assert_in(?SET:to_list(I2), bitset:union([B1,B2,B3])),
  ok = neg_test(?SET:to_list(I2), 100000, bitset:union([B1, B2, B3])).

fuzz_difference() ->
  io:format("Making sets~n"),
  S1 = random_set(10000, ?SET:new()),
  S2 = random_set(100000, ?SET:new()),

  io:format("Making bitsets~n"),
  B1 = bitset:new(?SET:to_list(S1)),
  B2 = bitset:new(?SET:to_list(S2)),

  I1 = ?SET:subtract(S1, S2),
  io:format("First difference size is ~p~n", [?SET:size(I1)]),
  ok = assert_in(?SET:to_list(I1), bitset:difference(B1,B2)),
  ok = assert_out(?SET:to_list(S2), bitset:difference(B1,B2)),
  ok = neg_test(?SET:to_list(I1), 100000, bitset:difference(B1, B2)).

fuzz_fold() ->
  List = random_list(100000, []),
  B    = bitset:new(List),
  L2   = bitset:foldr(fun(X, A) -> [ X | A ] end, [], B),
  L2   = ordsets:to_list(ordsets:from_list(List)),
  L3   = bitset:foldl(fun(X, A) -> [ X | A ] end, [], B),
  L3   = lists:reverse(ordsets:to_list(ordsets:from_list(List))),
  ok.

test(Msg, Fn) ->
  io:format("~s: ~p~n", [ Msg, ok = Fn() ]).

main() ->
  {A1,A2,A3} = now(),
  random:seed(A1, A2, A3),
  test("Speedtest", fun speedtest/0),
  test("Simple fuzzed", fun fuzz_simple/0),
  test("Fuzzed Intersection", fun fuzz_intersect/0),
  test("Fuzzed Union", fun fuzz_union/0),
  test("Fuzzed Difference", fun fuzz_difference/0),
  test("Fuzzed Folding", fun fuzz_fold/0),
  ok.
