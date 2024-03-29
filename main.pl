:- [db].

colors([yellowLine, 
    greenLine, 
    greenbranchLine, 
    magentaLine, 
    pinkLine, 
    orangeLine, 
    bluebranchLine, 
    greyLine, 
    blueLine, 
    redLine, 
    violetLine]).

outputcolor(yellowLine, yellow).
outputcolor(greenLine, green).
outputcolor(greenbranchLine, greenbranch).
outputcolor(magentaLine, magenta).
outputcolor(pinkLine, pink).
outputcolor(orangeLine, orange).
outputcolor(bluebranchLine, bluebranch).
outputcolor(greyLine, grey).
outputcolor(blueLine, blue).
outputcolor(redLine, red).
outputcolor(violetLine, violet).

line(X, LL):- 
    colors(ColorList),
    outputcolor(Y,X),
    member(Y, ColorList), 
    G=..[Y,LL], 
    G.

% Usually hop(a, X)
hop((Color, Station1), (Color, Station2)):-
    line(Color, StationList),
    member(Station1, StationList),
    member(Station2, StationList),
    Station1 \== Station2,
    hopSameColor(Station1, Station2, StationList).

hop((Color1, Station), (Color2, Station)):-
    station(Color1, Station),
    station(Color2, Station),
    Color1\==Color2.

station(Color, Station):-
    line(Color, StationList),
    member(Station, StationList).

hopSameColor(X, Y, [X, Y | _]).
hopSameColor(Y, X, [X, Y | _]).

hopSameColor(X, Y, List):-
    List = [_, B | Rest],
    hopSameColor(X, Y, [B | Rest]).

display_multiple([]).
display_multiple(LList):-
    [H|T]=LList,
    display_path(H),
    display_multiple(T).

display_path(List):-
    nl,length(List,X), format("Length = ~w, ", X), nl, write(List),nl.

maxdepth(Depth,X,Y,Path):-
    station(ColorX, X), 
    station(ColorY, Y),
    maxdepth_r(Depth,(ColorX, X),(ColorY, Y),[],P),
    reverse(P,Path).

maxdepth_r(0,X,X,[],[X]).

maxdepth_r(1,X,Y,Acc,[Y,X|Acc]):- \+memberchk(Y,Acc),hop(X,Y).

maxdepth_r(Depth,X,Y,Acc,Path):-
    Depth > 1,
    hop(X,Z),
    \+memberchk(Z,Acc),
    NewDepth is Depth-1,
    maxdepth_r(NewDepth,Z,Y,[X|Acc],Path).
    
myiddfs(X,Y,LPath):-
    station(_, X),
	station(_, Y), !,
	myiddfs_r(0, X, Y, [], LPath, 5).
    % myiddfs_r(1, X, Y, [], LPath, 5).
    % display_multiple(LPath).

appendLists(X, [], X, _).
appendLists(X, _, X, 0).
appendLists(LPath, LL, NewL, Num_Paths):- 
    Num_Paths>0, 
    LL = [H|T], 
    append(LPath, [H], L2), 
    NewNum_Paths is Num_Paths-1,
    appendLists(L2, T, NewL, NewNum_Paths).

myiddfs_r(_, _, _, LL, LL, 0).

myiddfs_r(Depth, X, Y, LPath, Routes, Num_Paths):-
    Num_Paths>0,
    % write(depth=Depth), nl,
    NewDepth is Depth+1,
    findall(H, maxdepth(Depth, X, Y, H), LL), 
    appendLists(LPath, LL, NewLPath, Num_Paths),
    length(LPath, Len), length(NewLPath, Len2), NewNum_Paths is Num_Paths-(Len2-Len),
    myiddfs_r(NewDepth, X, Y, NewLPath, Routes, NewNum_Paths).

paths(X, Y, LL):- myiddfs(X,Y,LL),!.

% Debugging Aids
getPathLen([],[]).
getPathLen([H|T], L):-length(H,Len), L=[Len|LTail], getPathLen(T,LTail).