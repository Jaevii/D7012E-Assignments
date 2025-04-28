% Ludvig Järvi, ludjrv-1

% Help functions
sum_of_set([], 0).
sum_of_set([Head|Tail], Sum) :-
    sum_of_set(Tail, TotalSum),
    Sum is Head + TotalSum.

take(0, _, []).
take(_, [], []).
take(N, [Head|Tail], [Head|Result]) :-
    N > 0,
    N1 is N - 1,
    take(N1, Tail, Result).


% 1. Generete all subsets of a list, a subset is defined as [Sum, Start-index, End-index, [Elements]]
create_subset([], _, []).
create_subset(Set, Index, Result) :-
    sum_of_set(Set, Sum),
    I is Index,
    length(Set, Len),
    J is I + Len - 1,
    Subset = [(Sum, I, J, Set)],
    ToTake is Len - 1, 
    take(ToTake, Set, Next),
    create_subset(Next, I, Rest),
    append(Subset, Rest, Result).

generate_subsets([], _, []).
generate_subsets([H|T], Index, Subsets) :-
    create_subset([H|T], Index, Result),
    NextIndex is Index + 1,
    generate_subsets(T, NextIndex, Sets),
    append(Result, Sets, Subsets).


% 2. Sort subsets by sum using Merge sort
% Divide the list by alternating elements into L1 and l2
divide([], [], []).
divide([X], [X], []).
divide([X,Y|Rest], [X|L1], [Y|L2]) :-
    divide(Rest, L1, L2).

% Compare the sum of the first set from L1 and L2, the smallest set is added first to the result
merge([], L, L).
merge(L, [], L).
merge([(Sum1, I1, J1, Set1) | T1], [(Sum2, I2, J2, Set2) | T2], [(Sum1, I1, J1, Set1) | T]) :-
    Sum1 =< Sum2,
    merge(T1, [(Sum2, I2, J2, Set2) | T2], T).
merge([(Sum1, I1, J1, Set1) | T1], [(Sum2, I2, J2, Set2) | T2], [(Sum2, I2, J2, Set2) | T]) :-
    Sum1 > Sum2,
    merge([(Sum1, I1, J1, Set1) | T1], T2, T).

sort_sets([], []).
sort_sets([X], [X]).
sort_sets(List, SortedSubsets) :-
    divide(List, L1, L2),
    sort_sets(L1, SortedL1),
    sort_sets(L2, SortedL2),
    merge(SortedL1, SortedL2, SortedSubsets).


% 3. Print the k smallest subsets
print_smallest_sets([]).
print_smallest_sets([(Sum, I, J, Set)|T]) :-
    write(Sum),
    write('\t'),
    write(I),
    write('\t'),
    write(J),
    write('\t'),
    write(Set),
    write('\n'),
    print_smallest_sets(T).


% 4. Test the program with a sample set and k value
smallest_k_sets(List, K) :-
    generate_subsets(List, 1, Subsets),
    sort_sets(Subsets, SortedSubsets),
    take(K, SortedSubsets, SmallestSets),
    write('\n'),
    write('Entire list: '),
    write(List),
    writeln("\n\nsize\ti\tj\tSubset"),
    print_smallest_sets(SmallestSets),
    write('\n').

test :- 
    %smallest_k_sets([24,-11,-34,42,-24,7,-19,21], 6).
    smallest_k_sets([3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3], 8).