% 323CA Dumitrascu Filip Teodor
:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').

% Reprezentare jocului:
% o pereche formata din 2 liste cu termeni compusi
% prima este formata din starea jocului cu piese
% a doua este formata din block 


% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4
empty_state(([], [])).



%%%%%%
% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.
set_tile((Map, B), Pos, ([(Pos, tile) | Map], B)).



% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.
set_blank(Map, _, Map).



% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).
set_target((Map, B), Pos, ([(Pos, target) | Map], B)).



% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.
set_fragile((Map, B), Pos, ([(Pos, fragile) | Map], B)).



% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.
set_block_initial((Map, B), Pos, (NewMap, [(block, [Pos]) | B])) :-
    set_tile((Map, B), Pos, (NewMap, _)).    % se pune cu Pos: tile in S si block in B



% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)]
get_b_pos((_, B), Positions) :- 
    member((block, Pos), B), (Pos = [(X, Y)], Positions = (X, Y), !; Positions = Pos).



% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.
get_bounds((Map, _), Xmin, Xmax, Ymin, Ymax) :-
    findall(X, member(((X, _), _), Map), Xs),
    min_list(Xs, Xmin),
    max_list(Xs, Xmax),
    findall(Y, member(((_, Y), _), Map), Ys),
    min_list(Ys, Ymin),
    max_list(Ys, Ymax).


% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.
get_cell((Map, _), Pos, oswitch) :- member((Pos, oswitch, _, _), Map).
get_cell((Map, _), Pos, xswitch) :- member((Pos, xswitch, _, _), Map).
get_cell((Map, _), Pos, Type) :- member((Pos, Type), Map).


% activate_oswitch/3
% activate_oswitch(Map, Positions, NewMap)
activate_oswitch(Map, Pos, NewMap) :-
    (member((Pos, oswitch, Func, Positions), Map) ->
        activate_switch_tiles(Map, Positions, Func, NewMap)
    ; NewMap = Map).


% activate_xswitch/3
% activate_xswitch(+Map, +Positions, -NewMap)
activate_xswitch(Map, Pos, NewMap) :-
    (member((Pos, xswitch, Func, Positions), Map) ->
        activate_switch_tiles(Map, Positions, Func, NewMap)
    ; NewMap = Map).


% activate_switch_tiles/3
% activate_switch_tiles(+Map, +Positions, +Func, -NewMap)
activate_switch_tiles(Map, [], _, Map).
activate_switch_tiles(Map, [Pos | Rest], Func, NewMap) :-
    (Func = switch ->
        (member((Pos, tile), Map) -> select((Pos, tile), Map, TempMap);
                                     TempMap = [(Pos, tile) | Map]);
    Func = uponly ->
        (member((Pos, tile), Map) -> TempMap = Map;
                                     TempMap = [(Pos, tile) | Map]);
    Func = dnonly ->
        (member((Pos, tile), Map) -> select((Pos, tile), Map, TempMap);
                                     TempMap = Map)),
    activate_switch_tiles(TempMap, Rest, Func, NewMap).


%% TODO
% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).
move(S, Move, SNext) :-                        % din vertical in orizontal
    S = (Map, B),
    member((block, [Pos]), B),
    neighbor(Pos, Move, Pos1),
    neighbor2(Pos, Move, Pos2),
    get_cell(S, Pos1, _), get_cell(S, Pos2, _),
    activate_oswitch(Map, Pos1, NewMap1),
    activate_oswitch(NewMap1, Pos2, NewMap2),
    SNext = (NewMap2, [(block, [Pos1, Pos2])]).

move(S, Move, SNext) :-                       % din orizontal in vertical
    S = (Map, B),
    member((block, [Pos1, Pos2]), B),
    member(Pivot, [Pos1, Pos2]), member(Other, [Pos1, Pos2]),
    neighbor(Other, Move, Pivot),  % mic + 1 == mare
    neighbor(Pivot, Move, Pos),
    get_cell(S, Pos, Type), Type \= fragile,
    activate_oswitch(Map, Pos, NewMap1),
    activate_xswitch(NewMap1, Pos, NewMap2),
    SNext = (NewMap2, [(block, [Pos])]).

move(S, Move, SNext) :-                       % din orizontal in orizontal 
    S = (Map, B),
    member((block, [Pos1, Pos2]), B),
    neighbor(Pos1, Move, NewPos1),
    neighbor(Pos2, Move, NewPos2),
    get_cell(S, NewPos1, _), get_cell(S, NewPos2, _),
    (\+ (member((block, Positions), B), (member(NewPos1, Positions); % blockul nu se poate
    member(NewPos2, Positions)))), % muta pe o pozitie pe care a mai fost inainte 
    activate_oswitch(Map, NewPos1, NewMap1),
    activate_oswitch(NewMap1, NewPos2, NewMap2),
    SNext = (NewMap2, [(block, [NewPos1, NewPos2])]).



%% TODO
% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).
is_final(S) :-
    get_b_pos(S, (X, Y)),
    get_cell(S, (X, Y), target).


%%%%%%%%%% Etapa 2

%% TODO
% set_switch/6
% set_switch(+S, +Pos, +Switch, +Func, +Positions, -SNew)
% Leagă starea SNew la o stare cu aceleași informații ca și S, și în
% plus un switch la poziția Pos, cu parametrii dați.
%
% Switch: oswitch sau xswitch.
% Func: switch, uponly sau dnonly.
% Positions: pozițiile podului.
set_switch((S, B), Pos, Switch, Func, Positions, ([(Pos, Switch, Func, Positions) | S], B)).


%% TODO
% solve/2
% solve(+S, -Moves)
% Solve găsește o soluție pentru problema din starea S. Soluția este
% reprezentată ca secvența de mutări Moves.
%
% Pentru a fi soluție, mutările din Moves trebuie să ducă blocul în
% picioare pe poziția scop (target).
solve(S, Moves) :-
    solve(S, [], Moves).

% solve/3
% solve(+S, +Visited, -Moves)
solve(S, _, _) :- is_final(S).
solve(S, Visited, [Move | Moves]) :-
    move(S, Move, SNext),
    SNext = (_, B),
    \+ member(B, Visited),
    solve(SNext, [B | Visited], Moves).
