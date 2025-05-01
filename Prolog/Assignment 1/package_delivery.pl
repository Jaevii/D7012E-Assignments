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
*/



test :- 