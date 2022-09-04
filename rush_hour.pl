:- module(rush_hour, [pocet/3, dopredu/3, dozadu/3, doprava/3, nahrad_prvni/4, dolu/3, nahrad_prvek_na/4, posledni_dva/2, rush_hour/4, cil/1]).

% 6x6 pole
% 12 aut - 2 policka
%  - jedno z nich je cervene - musime dostat ven
% 4 nakladaky - 3 policka
% cervene auto
%   - na tretim radku zhora
%   - vyjezd je na tretim radku vpravo
% vsechny auta a nakladaky nemusi byt na hracim poli

% Reprezentace (podle https://www.thinkfun.com/internal/rush-hour-challenge-generator/ ) :
% -OOOAA--BPPP--BXXC-----CQQQRRR------ 
% Mapa:
% -OOOAA
% --BPPP
% --BXXC
% -----C
% QQQRRR
% ------

% volne misto = o

% naprogramovat solver - napred 6*6 pote m*n
% pote udelat generator, podle poctu kroku solveru

% pocet(+S, +L, ?V) :- pocet symbolu S v listu L je V
pocet(_, [], 0) :- !.
pocet(S, [S|L], V) :- !, pocet(S, L, V1), V is V1 + 1.
pocet(S, [X|L], V) :- X \= S, pocet(S, L, V).

% doprava(+P, ?LL, ?NL) :- posune vsechny symboly P o 1 pole doprava (LL je seznam seznamu), vysledek je v seznamu NL
% LL a NL - jeden musi byt vstup, druhy vystup
doprava(P, [R|L], [NR|L]) :- !, doprava_(P, R, NR).

% doprava_(+P, ?L, ?NL) :- posune vsechny symboly P o 1 pole doprava (musi byt volne misto na posunuti), vysledek je v seznamu NL
doprava_(P, [P,P,P,Z|L], [Z,P,P,P|L]) :- !, Z = o.
doprava_(P, [P,P,Z|L], [Z,P,P|L]) :- !, Z = o.
doprava_(P, [X|L], [X|NL]) :- X \= P, doprava_(P, L, NL).

% doleva(+P, ?LL, ?NL) :- posune vsechny symboly P o 1 pole doleva (LL je seznam seznamu), vysledek je v seznamu NL
doleva(P, L, NL) :- doprava(P, NL, L).

% nahrad_prvni(+P, +N, ?L, ?NL) :- prvni vyskyt prvku P v seznamu L nahradi N, vysledek je v NL
% LL a NL - jeden musi byt vstup, druhy vystup
nahrad_prvni(_, _, [], []) :- !.
nahrad_prvni(P, N, [P|L], [N|L]) :- !.
nahrad_prvni(P, N, [X|L], [X|NL]) :- X \= P, nahrad_prvni(P, N, L, NL).

% dolu(+P, ?LL, ?NL) :- posune vsechny symboly P o 1 pole dolu (musi byt volne misto na posunuti), vysledek je v seznamu NL
% LL a NL - jeden musi byt vstup, druhy vystup
dolu(P, [R|L], [NR|NL]) :- (member(P, R) -> nahrad_prvni(P, o, R, NR), nth0(I, R, P), !, dolu_(P, L, NL, I); dolu(P, L, NL)).

% nahrad_prvek_na(+I, +P, +O, +LL, -NL) :- nahradi prvek na pozici I prvkem P v seznamu LL pouze pokud je na poli symbol O, vysledny seznam je NL.
nahrad_prvek_na(0, P, [S|L], [P|L]) :- !, S = o.
nahrad_prvek_na(I, P, [X|L], [X|NL]) :- NI is I - 1, !, NI >= 0, nahrad_prvek_na(NI, P, L, NL).

dolu_(P, [R|L], [NR|NL], I) :- !, (member(P, R) -> R = NR, dolu_(P, L, NL, I); nahrad_prvek_na(I, P, R, NR), L = NL).

nahoru(P, [R1,R2|L], [NR1,NR2|NL]) :- member(P, R2), nth0(I, R2, P), !, nahrad_prvek_na(I, P, R1, NR1), R2 = NR2, nahoru_(P, L, NL, I).

nahoru_(P, [R1,R2|L], [NR1,NR2|NL], I) :- !, (member(P, R2) -> R1 = NR1, nahoru_(P, [R2|L], [NR2|NL], I); nahrad_prvek_na(I, P, NR1, R1), R2 = NR2, L = NL).
nahoru_(P, [R], [NR], I) :- !, nahrad_prvek_na(I, P, NR, R).

% dopredu(+P, +LL, -NLL) :- posune auto se znackou P o 1 krok dopredu
dopredu(P, [R|L], [NR|NL]) :- !, pocet(P, R, V), 
                                (V=:=0 -> 
                                    R = NR, dopredu(P, L, NL)
                                    ; 
                                    (V > 1 -> 
                                        doprava(P, [R|L], [NR|NL]) 
                                        ; 
                                        dolu(P, [R|L], [NR|NL])
                                    )
                                ).
% dozadu(+P, +LL, -NLL) :- posune auto se znackou P o 1 krok dozadu
dozadu(P, [R1,R2|L], [NR1,NR2|NL]) :- !, pocet(P, R1, V1),
                    (V1 > 1 -> 
                        doleva(P, [R1,R2|L], [NR1,NR2|NL])
                        ; 
                        V1 =:= 0, pocet(P, R2, V2),
                        (V2 =:= 1 -> 
                            nahoru(P, [R1,R2|L], [NR1,NR2|NL]) 
                            ; 
                            R1 = NR1, dozadu(P, [R2|L], [NR2|NL])
                        )
                    ).
dozadu(P, [R], [NR]) :- !, pocet(P, R, V), V > 1, doleva(P, [R], [NR]).

% posledni_dva(?P, +L) :- prvek P je na poslednich dvou pozicich seznamu L
posledni_dva(P, L) :- append(_, [P, P], L), !.

cil([_, _, R3|_]) :- posledni_dva(x, R3).

% cesta(H, S, T, P) :- najde cestu
%   H je jeden krok
%   S je aktualni stav hraciho pole 
%   T kontroluje cilovy stav
%   P je posloupnost tahu
cesta(H, S, T, P, AUTA) :- cesta(H, S, T, [S], P, AUTA).
cesta(_, S, _, _, [], _) :- cil(S).
cesta(H, S, T, V, [A|P], AUTA) :- call(H, S, W, T, A, AUTA), \+ member(W, V), ( cil(W) -> true; cesta(H, W, T, [S|V], P, AUTA)).

cesta_iter(N, H, S, T, P, AUTA) :- between(1, N, K), length(P, K), cesta(H, S, T, P, AUTA), !.

rush_hour_krok(S, W, T, A, AUTA) :- rush_hour_krok_(S, W, T, A, AUTA), \+ call(T, S).
rush_hour_krok_(S, W, _, A, AUTA) :- member(X, AUTA), dopredu(X, S, W), A=[X,f].
rush_hour_krok_(S, W, _, A, AUTA) :- member(X, AUTA), dozadu(X, S, W), A=[X,b].

rush_hour(S, T, [], _) :- call(T, S), !.
rush_hour(S, T, P, AUTA) :- cesta_iter(100, rush_hour_krok, S, T, P, AUTA).

% start with final state, populate cars on free positions, work backwards, test minimal number of steps to solve
% difficulty is determined by the minimal number of steps to complete the puzzle

matice_(M, [X]) :- length(X, M).
matice_(M, [X|V]) :- length(X, M), matice_(M, V).
matice(M, N, V) :- length(V, N), matice_(M, V).

% generace(+M, +N, +R, +A, -V) :- vygeneruje rozlozeni aut ze seznamu A na mape M*N, kde nejkratsi reseni je R kroku, vysledna mapa je V
generace(M, N, R, A, V) :- matice(M, N, V), !, generace_(R, A, [], V).
generace_(R, [X|A], Z, MAPA) :- poloz_v([X,X], MAPA), generace_(R, A, [X|Z], MAPA).
generace_(R, [X|A], Z, MAPA) :- poloz_v([X,X,X], MAPA), generace_(R, A, [X|Z], MAPA).
generace_(R, [X|A], Z, MAPA) :- poloz_h([X,X], MAPA), generace_(R, A, [X|Z], MAPA).
generace_(R, [X|A], Z, MAPA) :- poloz_h([X,X,X], MAPA), generace_(R, A, [X|Z], MAPA).
generace_(R, [], Z, MAPA) :- MAPA = [_,_,R3|_], poloz_h_([x,x],R3), vypln_prazdne(MAPA, o), cesta_iter(R, rush_hour_krok, MAPA, cil, P, [x|Z]), length(P, R), write(P).

% vypln_prazdne(L, P) :- vsechny volne termy unifikuje s P
vypln_prazdne([], _) :- !.
vypln_prazdne([R|MAPA], P) :- vypln_prazdne_(R, P), vypln_prazdne(MAPA, P).
vypln_prazdne_([], _) :- !.
vypln_prazdne_([X|R], P) :- nonvar(X), vypln_prazdne_(R, P).
vypln_prazdne_([X|R], P) :- var(X), X=P, vypln_prazdne_(R, P).

seznam_plny([]).
seznam_plny([X|L]) :- nonvar(X), seznam_plny(L).

% poloz_h(+X, +MAPA) :- polozi auto X na mapu MAPA horizontalne (unifikuje auto s mapou), X je seznam symbolu auta (delka seznamu urcuje delku auta)
poloz_h(X, [R|_]) :- poloz_h_(X, R).
poloz_h(X, [_|MAPA]) :- poloz_h(X, MAPA).
% poloz_h_(+X, +MAPA) :- pomocna funkce poloz_h, ktera provede polozeni auta na radek
poloz_h_(X, [_|R]) :- poloz_h_(X, R).
poloz_h_(X, R) :- append(X, _, R).

% poloz_v(+X, +MAPA) :- polozi auto X na mapu MAPA vertikalne (unifikuje auto s mapou), X je seznam symbolu auta (delka seznamu urcuje delku auta)
poloz_v(A, [_|MAPA]) :- poloz_v(A, MAPA).
poloz_v([X|A], [R|MAPA]) :- nth0(I, R, X), poloz_v_(I, A, MAPA).
% poloz_v_(+X, +MAPA) :- pomocna funkce poloz_v, polozi zbytek auta na dalsi radky
poloz_v_(_, [], _).
poloz_v_(I, [X|A], [R|MAPA]) :- nth0(I, R, X), poloz_v_(I, A, MAPA).