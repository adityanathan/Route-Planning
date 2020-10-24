:- [db].

% paths(X,Y,LL)
%  LL - List of all acyclic paths by which a traveller can go from X to Y
%  X must be head of every list in LL and Y must be tail of every list in LL
%  LL is sorted in non-descending order i.e ascending or equal order of hop(X,Y)

% violet, red, blue, grey, bluebranch, orange, pink, magenta, greenbranch, green, yellow

% Optimization Ideas

% Collect all possible hops for a particular unification and assert them
% Use accumulators everywhere.
% Make your Goal orderings and rule orderings such that there is no unnecessary backtracking.
% Use cuts to prune the search space.

colors([yellowLine, greenLine, greenbranchLine, magentaLine, pinkLine, orangeLine, bluebranchLine, greyLine, blueLine, redLine, violetLine]).

line(X, LL):- 
    colors(ColorList), 
    member(X, ColorList), 
    G=..[X,LL], 
    G.

% :-  dynamic  hop_lookup/3.

% hop(X, Y):- hop_lookup(X,Y), !.

hop((Color, Station1), (Color, Station2)):-
    line(Color, StationList),
    station(Color, Station1),
    station(Color, Station2),
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
hopSameColor(X, Y, [_, B | Rest]):-
    hopSameColor(X, Y, [B | Rest]).

display_multiple(LList):-
    [H|T]=LList,
    display_path(H),
    display_multiple(T).

% display_multiple([]).

display_path(List):-
    nl,length(List,X), format("Length = ~w, ", X), nl, write(List),nl.

maxdepth(Depth,X,Y,Path):-
    station(ColorX, X), 
    station(ColorY, Y),
    maxdepth_r(Depth,(ColorX, X),(ColorY, Y),[],P),
    reverse(P,Path).

maxdepth_r(1,X,Y,Acc,[Y,X|Acc]):- \+memberchk(Y,Acc),hop(X,Y).

maxdepth_r(Depth,X,Y,Acc,Path):-
    Depth > 1,
    hop(X,Z),
    \+memberchk(Z,Acc),
    NewDepth is Depth-1,
    maxdepth_r(NewDepth,Z,Y,[X|Acc],Path).
    
myiddfs(X,Y,LPath):-
    length(LPath,5),
    myiddfs_r(1, X, Y, LPath),
    display_multiple(LPath).

% myiddfs_r(Depth, _, _, []):- format("~n Search complete at Depth ~w ~n",Depth).
myiddfs_r(_, _, _, []).

myiddfs_r(Depth, X, Y, LPath):-
    LPath = [H|LPath2],
    write(depth=Depth), nl,
    NewDepth is Depth+1,
    ((maxdepth(Depth, X, Y, H), 
    % write(H),
    !, myiddfs_r(NewDepth, X, Y, LPath2)) ; myiddfs_r(NewDepth, X, Y, LPath)).