% Ludvig Järvi, ludjrv-1

/*
Three interconnected rooms r1,r2 and r3

    ___________________
    |        |        |
    |   r1       r3   |
    |__    __|________|
    |        |          
    |   r2   |
    |________|

r1 <-> r2: Steel key
r1 <-> r3: Brass key

r1: Steel key & Robot
r2: Brass key
r3: Package

Robot can move between rooms, take up items, and drop items. 
Can carry at most two items at a time.

Can the robot fetch the package from room r3 and bring it to room r2? If so, how should the robot behave?

    Steps:
     1. Pick up the Steel key from r1.
     2. Move into r2.
     3. Pick up the Brass key from r2.
     4. Move into r1.
     5. Drop the Steel key in r1.
     6. Move into r3. 
     7. Pick up the package from r3.
     8. Move into r1.
     9. Drop the Brass key in r1.
    10. Pick up the Steel key from r1.
    11. Move into r2.
    12. DONE

State is defined as state(Robot position, Steel Key position, Brass Key position, Package position, Number of held Items)
*/


% Move between rooms
% R1 <-> R2
action(state(r1, held, BrassKey, Package, Items), move(r1, r2), state(r2, held, BrassKey, Package, Items)).

action(state(r2, held, BrassKey, Package, Items), move(r2, r1), state(r1, held, BrassKey, Package, Items)).

% R1 <-> R3
action(state(r1, SteelKey, held, Package, Items), move(r1, r3), state(r3, SteelKey, held, Package, Items)).

action(state(r3, SteelKey, held, Package, Items), move(r3, r1), state(r1, SteelKey, held, Package, Items)).

% Pick up items
% Steel Key
action(state(Robot, Robot, BrassKey, Package, Items), pickUp(steelkey, Robot), state(Robot, held, BrassKey, Package, NewItems)) :-
    Items < 2,
    NewItems is Items + 1.

% Brass Key
action(state(Robot, SteelKey, Robot, Package, Items), pickUp(brasskey, Robot), state(Robot, SteelKey, held, Package, NewItems)) :-
    Items < 2,
    NewItems is Items + 1.

% Package
action(state(Robot, SteelKey, BrassKey, Robot, Items), pickUp(package, Robot), state(Robot, SteelKey, BrassKey, held, NewItems)) :-
    Items < 2,
    NewItems is Items + 1.

% Drop items
% Steel Key
action(state(Robot, held, BrassKey, Package, Items), drop(steelkey, Robot), state(Robot, Robot, BrassKey, Package, NewItems)) :-
    NewItems is Items - 1.

% Brass Key
action(state(Robot, SteelKey, held, Package, Items), drop(brasskey, Robot), state(Robot, SteelKey, Robot, Package, NewItems)) :-
    NewItems is Items - 1.

% Package
action(state(Robot, SteelKey, BrassKey, held, Items), drop(package, Robot), state(Robot, SteelKey, BrassKey, Robot, NewItems)) :-
    NewItems is Items - 1.

% Solve
solveR(state(_, _, _, r2, _), _, [done|[]]). % Goal: Robot is in r2 holding the package
solveR(State, N, [Action|Trace]) :-
    N > 0,
    action(State, Action, NewState),
    N1 is N - 1,
    solveR(NewState, N1, Trace).

start(T) :- 
    solveR(state(r1, r1, r2, r3, 0), 12, T),
    printL(T, 1).

% Print list helper function
printL([], _).
printL([H|T], Index) :-
    format('~t~d~2|. ~w~n', [Index, H]),
    Next is Index + 1,
    printL(T, Next).
