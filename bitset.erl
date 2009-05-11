-module(bitset).
-export([new/0, new/1, from_list/1, insert/2, 
    intersect/1, union/1, difference/2,
    contains/2, foldl/3, foldr/3,
    pmap/2]).

%% Create a new bitset, with no bits set.
%% return     A new bitset
new() ->
  { ?MODULE, { 1, 0 }}.

%% Create a new bitset from the entries in the given list of numbers
%% Elems      A list of integers that should be set in this new bitset
new(Elems) when is_list(Elems) ->
  from_list(Elems, new()).

from_list(Elems) when is_list(Elems) ->
  from_list(Elems, new()).

%% Put a given Key (non-negative integer) into the bitset
%% Key        Non-negative integer to be stored in the set
%% Set        The set in which to store the key
%% return     A new bitset, with the desired key set
insert(Key, { ?MODULE, Tree = { Depth, _}}) when is_integer(Key), Key >= 0 ->
  { ToMake, Path } = split_key(Depth, Key),
  LargeEnough = grow(ToMake, Tree),
  { ?MODULE, set(Path, LargeEnough) }.

%% Determine the intersection of the given list of bitsets.  The resulting 
%% bitset will have bits set where all given sets had them set, and nowhere 
%% else.
%% List       A list of bitsets
%% return     A new bitset that is the intersection of the given bitsets
intersect([]) -> bitset:new();
intersect([Final]) -> Final;
intersect([{ ?MODULE, First }, { ?MODULE, Second } | Rest]) ->
  intersect([{ ?MODULE, imerge(First, Second)} | Rest]).

%% Determine the union of the given list of bitsets.  The resulting bitset will
%% have bits set where any of the given sets had them set, and nowhere else.
%% List       A list of bitsets to union together
%% return     A new bitset which is the union of the given bitsets
union([]) -> bitset:new();
union([Final]) -> Final;
union([{ ?MODULE, First }, { ?MODULE, Second } | Rest]) ->
  union([{ ?MODULE, umerge(First, Second)} | Rest]).

difference({ ?MODULE, Left = { LD, _ }}, { ?MODULE, Right = { RD, _ }}) ->
  { SLeft, SRight } = if
    LD < RD ->
      { grow(RD-LD, Left), Right };
    LD > RD ->
      { Left, grow(LD-RD, Right) };
    LD =:= RD ->
      { Left, Right }
  end,
  { ?MODULE, diff_eq(SLeft, SRight) }.

%% Test whether a bit is set in the given bitset.
%% Key        The key to test
%% Set        The bitset to test against
%% return     true of that bit is set, false otherwise
contains(Key, { ?MODULE, { Depth, _ } = Tree }) 
    when is_integer(Key), Key >= 0 ->
  case split_key(Depth, Key) of
    { 0, Path } ->
      %io:format("Path is ~w~n", [Path]),
      contains2(Path, Tree);
    { _ToMake, _Exists } ->
      %io:format("Key is too large for this set~n"),
      false
  end.

foldl(Fun, Accum0, { ?MODULE, Tree }) -> fold(left, Fun, Accum0, 0, Tree).
foldr(Fun, Accum0, { ?MODULE, Tree }) -> fold(right, Fun, Accum0, 0, Tree).

pmap(Fun, { ?MODULE, Tree }) ->
  pmap(Fun, Tree, 0, 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper for new/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
from_list([], Bs) -> Bs;
from_list([Head | Tail ], Bs) when is_integer(Head) ->
  from_list(Tail, insert(Head, Bs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper for insert/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% empty entry at the bottom becomes a number with the correct bit set
set([Final], empty) when is_integer(Final), Final >= 0 ->
  { 1, 1 bsl Final };

%% good node at the bottom gets a new bit set
set([Final], { 1, Number }) 
    when is_integer(Final), Final >= 0 ->
  { 1, Number bor (1 bsl Final) };

%% non-bottom node gets a new child
set([Pos0 | Rest], { Depth, Children }) 
    when is_integer(Pos0), Pos0 >= 0 ->
  Pos = Pos0+1,
  New = set(Rest, element(Pos, Children)),
  { Depth, setelement(Pos, Children, New) };

%% non-bottom empty becomes a child container, and gets a child
set([Pos0 | Rest], empty) 
    when is_integer(Pos0), Pos0 >= 0 ->
  Pos      = Pos0 + 1,
  Children = erlang:make_tuple(27, empty),
  Depth    = length(Rest)+1,
  { Depth, setelement(Pos, Children, set(Rest, empty)) }.

%% Function used to make the tree deeper, so it can hold elements that are too
%% large for its current size
grow(0, Tree) -> Tree;
grow(Levels, { ChildDepth, _ } = Tree) 
    when is_integer(Levels), Levels > 0 ->
  %io:format("Growing, Levels is ~p~n", [Levels]),
  Children = erlang:make_tuple(27, empty),
  grow(Levels-1, { ChildDepth+1, setelement(1, Children, Tree) }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers for intersect/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
imerge(First = { FDepth, FTree }, Second = { SDepth, STree }) ->
  if
    FDepth > SDepth ->
      imerge(element(1, FTree), Second);
    FDepth < SDepth ->
      imerge(First, element(1, STree));
    FDepth =:= SDepth ->
      imerge_equal(First, Second)
  end.

imerge_equal(_, empty) -> empty;
imerge_equal(empty, _) -> empty;
imerge_equal({ 1, FNumber }, { 1, SNumber }) ->
  { 1, FNumber band SNumber };

imerge_equal({ Depth, FTree }, { Depth, STree }) ->
  NewTree = lists:zipwith(fun imerge_equal/2, 
    tuple_to_list(FTree), tuple_to_list(STree)),
  { Depth, list_to_tuple(NewTree) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers for union/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
umerge(First = { FDepth, _ }, Second = { SDepth, _ }) ->
  if
    FDepth > SDepth ->
      umerge_equal(First, grow(FDepth-SDepth, Second));
    FDepth < SDepth ->
      umerge_equal(grow(SDepth-FDepth, First), Second);
    FDepth =:= SDepth ->
      umerge_equal(First, Second)
  end.

umerge_equal(empty, empty) -> empty;
umerge_equal(First, empty) -> First;
umerge_equal(empty, Second) -> Second;
umerge_equal({ 1, FNumber }, { 1, SNumber }) ->
  { 1, FNumber bor SNumber };

umerge_equal({ Depth, FTree }, { Depth, STree }) ->
  NewTree = lists:zipwith(fun umerge_equal/2,
    tuple_to_list(FTree), tuple_to_list(STree)),
  { Depth, list_to_tuple(NewTree) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers for difference/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
diff_eq(empty, _) -> empty;
diff_eq(Left, empty) -> Left;
diff_eq({ 1, LNumber }, { 1, RNumber }) ->
  RInvert = ((1 bsl 27 - 1) bxor RNumber),
  { 1, LNumber band RInvert };

diff_eq({ Depth, Left }, { Depth, Right }) ->
  NewTree = lists:zipwith(fun diff_eq/2,
    tuple_to_list(Left), tuple_to_list(Right)),
  { Depth, list_to_tuple(NewTree) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers for fold[lr]/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold(_Dir, _Fun, Accum, _Pre, empty) -> Accum;
fold(Dir, Fun, Accum, Pre, { 1, Number }) ->
  %io:format("Iterating on leaf, pre is ~p, Number is ~p~n", [ Pre, Number ]),
  lists:foldl(
    fun(Bit, AccumI) ->
        case Number band (1 bsl Bit) of
          0 -> AccumI;
          _ -> Fun(Pre * 27 + Bit, AccumI)
        end
    end,
    Accum,
    case Dir of
      left  -> lists:seq(0, 26);
      right -> lists:reverse(lists:seq(0, 26))
    end);
fold(Dir, Fun, Accum, Pre, { _Depth, Children }) ->
  %io:format("Pre is ~p, Tree is ~p~n", [ Pre, { Depth, Children }]),
  Offset = Pre*27,
  lists:foldl(
    fun(Element, AccumI) ->
        Child = element(Element+1, Children),
        fold(Dir, Fun, AccumI, Offset+Element, Child)
    end,
    Accum,
    case Dir of
      left  -> lists:seq(0,26);
      right -> lists:reverse(lists:seq(0, 26))
    end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper for pmap/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pmap(Fun, Tree, Pre, 0) ->
  pmap_fold(Fun, Tree, Pre);
pmap(_Fun, empty, _, _) ->
  [];
pmap(Fun, Tree = { Depth, _Children }, Pre, _Levels) when Depth =< 3 ->
  pmap_fold(Fun, Tree, Pre);
pmap(Fun, { _Depth, Children }, Pre, Levels) ->
  Offset = Pre*27,
  Root = self(),
  Pids = [ 
    spawn(fun() -> 
          Root ! { self(), pmap(Fun, Child, Offset+Element, Levels-1) } 
      end)
    || { Element, Child } 
    <- lists:zip(lists:seq(0,26), tuple_to_list(Children)) ],
  lists:foldl(
    fun(Pid, Accum) ->
        receive { Pid, Result } -> 
            Result ++ Accum
        end
    end,
    [],
    Pids).

pmap_fold(Fun, Tree, Pre) ->
  fold(
    left,
    fun(Bit, Accum) ->
        case Fun(Bit) of
          { ok, Value } ->
            [ Value | Accum ];
          nil ->
            Accum
        end
    end,
    [],
    Pre,
    Tree).


%% Helper function for split_key, generates X and Y where X*27+Y = Key; used for
%% converting the key to base-27, which is how our values are stored.
%% Key      Non-negative integer to divmod to our satisfaction
%% return   { Next, Diff } where Diff < 27 and Next * 27 + Diff = Key
next_diff(Key) when is_integer(Key), Key >= 0 ->
  Next = Key div 27,
  Diff = Key - ( Next * 27 ),
  { Next, Diff }.

%% Determine the base-27 form of the given non-negative integer.  This will be
%% returned as a list of numbers in the range [0, 26]; each number is a 
%% coefficient in the base-27 form of the given key, with the most significant
%% at the head of the list.
%% Also given to this function is the current depth of the tree; this function
%% will return how much larger the tree would have to be to hold this key, or 0
%% if the tree is already large enough.
%% Depth    The size of the tree we are generating a key address for
%% Key      The key we want to find in the tree
%% return   { NewLevels, Path }
split_key(Depth, Key) when is_integer(Key), Key >= 0 ->
  split_key(Depth, Key, 0, []).

%% Helper functions for split_key/2, with some accumulators added.
split_key(0, 0, New, Path) ->
  { New, Path };

split_key(0, Key, New, Path) when is_integer(Key), Key >= 0 ->
  { Next, Diff } = next_diff(Key),
  split_key(0, Next, New + 1,[ Diff | Path ]);

split_key(Depth, 0, New, Path) ->
  split_key(Depth-1, 0, New, [ 0 | Path ]);

split_key(Depth, Key, New, Path) when is_integer(Key), Key >= 0 ->
  { Next, Diff } = next_diff(Key),
  split_key(Depth-1, Next, New, [ Diff | Path ]).

contains2(_, empty) ->
  %io:format("Empty is false~n"),
  false;

contains2([ Final ], { 1, Number }) 
    when is_integer(Final), Final >= 0 ->
  %io:format("Final is ~p, Number is ~p~n", [Final, Number]),
  (Number band (1 bsl Final)) =/= 0;

contains2([ Pos0 | Rest ], { _Depth, Children }) 
    when is_integer(Pos0), Pos0 >= 0 ->
  Pos = Pos0+1,
  %io:format("Children are ~p, Pos is ~p~n", [Children, Pos]),
  contains2(Rest, element(Pos, Children)).

