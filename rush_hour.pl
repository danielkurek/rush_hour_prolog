:- module(rush_hour, [pocet/3, dopredu/3, dozadu/3, doprava/3, nahrad_prvni/4, dolu/3, nahrad_prvek_na/4, posledni_dva/2, rush_hour/4, rush_hour/3, cil/1]).

% 6x6 pole
% 12 aut - 2 policka
%  - jedno z nich je cervene - musime dostat ven
% 4 nakladaky - 3 policka
% cervene auto
%   - na tretim radku zhora
%   - vyjezd je na tretim radku vpravo
% vsechny auta a nakladaky nemusi byt na hracim poli

% volne misto = o

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

% dolu(+P, +LL, -NL) :- posune vsechny symboly P o 1 pole dolu (musi byt volne misto na posunuti), vysledek je v seznamu NL
% LL a NL - jeden musi byt vstup, druhy vystup
dolu(P, [R|L], [NR|NL]) :- (member(P, R) -> nahrad_prvni(P, o, R, NR), nth0(I, R, P), !, dolu_(P, L, NL, I); dolu(P, L, NL)).

% nahrad_prvek_na(+I, +P, +O, +LL, -NL) :- nahradi prvek na pozici I prvkem P v seznamu LL pouze pokud je na poli symbol O, vysledny seznam je NL.
nahrad_prvek_na(0, P, [S|L], [P|L]) :- !, S = o.
nahrad_prvek_na(I, P, [X|L], [X|NL]) :- NI is I - 1, !, NI >= 0, nahrad_prvek_na(NI, P, L, NL).

% dolu_(+P, +L, -NL, +I) :- pomocny predikat pro dolu/3, presune vsechny krome prvniho prvku
dolu_(P, [R|L], [NR|NL], I) :- !, (member(P, R) -> R = NR, dolu_(P, L, NL, I); nahrad_prvek_na(I, P, R, NR), L = NL).

% nahoru(+P, +L, -NL) :- posune vsechny symboly P o 1 pole nahoru, vysledny seznam je NL
nahoru(P, [R1,R2|L], [NR1,NR2|NL]) :- member(P, R2), nth0(I, R2, P), !, nahrad_prvek_na(I, P, R1, NR1), R2 = NR2, nahoru_(P, L, NL, I).

% nahoru(+P, +L, -NL, +I) :- pomocny predikat pro nahoru/3, presune vsechny krome prvniho prvku
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

% cil(+M) :- kontrola, jestli M je cilovy stav
cil([_, _, R3|_]) :- posledni_dva(x, R3).

% cesta(+KROK, +START, +CIL, +AUTA, -CESTA) :- najde cestu z START do cile CIL/1 pomoci kroku KROK, ktere lze aplikovat na auta ze seznamu AUTA, vysledna cesta je v CESTA
cesta(KROK, START, CIL, AUTA, CESTA) :- cesta(KROK, START, CIL, [START], AUTA, CESTA).
cesta(_, START, CIL, _, _, []) :- call(CIL, START).
cesta(KROK, START, CIL, MEM, AUTA, [AKCE|CESTA]) :- 
    call(KROK, START, NSTAV, CIL, AKCE, AUTA), 
    \+ member(NSTAV, MEM), 
    ( call(CIL, NSTAV) -> 
        true; 
        cesta(KROK, NSTAV, CIL, [NSTAV|MEM], AUTA, CESTA)
    ).

% cesta_iter(+N, +KROK, +START, +CIL, +AUTA, -CESTA) :- vola hledani cesty pro delky 1 az N, po prvni najite ceste uz nic nehleda
cesta_iter(N, KROK, START, CIL, AUTA, CESTA) :- between(1, N, K), length(CESTA, K), cesta(KROK, START, CIL, AUTA, CESTA), !.

rush_hour_krok(S, W, T, A, AUTA) :- rush_hour_krok_(S, W, T, A, AUTA), \+ call(T, S).
rush_hour_krok_(S, W, _, A, AUTA) :- member(X, AUTA), dopredu(X, S, W), A=[X,f].
rush_hour_krok_(S, W, _, A, AUTA) :- member(X, AUTA), dozadu(X, S, W), A=[X,b].

% rush_hour(+START, +CIL, +AUTA, -CESTA) :- najde nejkratsi cestu z START ke splneni cile CIL/1, muze pohybovat auty ze seznamu AUTA, vysledna cesta je v CESTA
rush_hour(START, CIL, _, []) :- call(CIL, START), !.
rush_hour(START, CIL, AUTA, CESTA) :- cesta_iter(100, rush_hour_krok, START, CIL, AUTA, CESTA).
% rush_hour(+START, +AUTA, -CESTA) :- najde nejkratsi cestu z START ke splneni zakldniho cile, muze pohybovat auty ze seznamu AUTA, vysledna cesta je v CESTA
rush_hour(START, AUTA, CESTA) :- rush_hour(START, cil, AUTA, CESTA).

% matice_(+N, -V) :- vytvori radek matice delky N
matice_(N, [X]) :- length(X, N).
matice_(N, [X|V]) :- length(X, N), matice_(N, V).
% matice(+M, +N, -V) :- vytvori prazdnou matici M x N, vysledek je V
matice(M, N, V) :- length(V, M), matice_(N, V).

% generace(+M, +N, +R, +A, -V) :- vygeneruje rozlozeni aut ze seznamu A na mape M*N, kde nejkratsi reseni je R kroku, vysledna mapa je V
generace(M, N, R, A, V) :- matice(M, N, V), !, generace_(R, A, [], V).
generace_(R, [X|A], Z, MAPA) :- poloz_v([X,X], MAPA), generace_(R, A, [X|Z], MAPA).
generace_(R, [X|A], Z, MAPA) :- poloz_v([X,X,X], MAPA), generace_(R, A, [X|Z], MAPA).
generace_(R, [X|A], Z, MAPA) :- poloz_h([X,X], MAPA), generace_(R, A, [X|Z], MAPA).
generace_(R, [X|A], Z, MAPA) :- poloz_h([X,X,X], MAPA), generace_(R, A, [X|Z], MAPA).
generace_(R, [], Z, MAPA) :- MAPA = [_,_,R3|_], poloz_h_([x,x],R3), 
    vypln_prazdne(MAPA, o), MAXR is R + (R // 2), cesta_iter(MAXR, rush_hour_krok, MAPA, cil, [x|Z], P), length(P, X), X >= R, write(P), vypis_matici(MAPA).

generace2(M, N, R, A2, A3, V) :- matice(M, N, V), !, generace2_(R, A2, A3, [], V).
generace2_(R, [X|A2], A3, Z, MAPA) :- poloz_v([X,X], MAPA), generace2_(R, A2, A3, [X|Z], MAPA).
generace2_(R, A2, [X|A3], Z, MAPA) :- poloz_v([X,X,X], MAPA), generace2_(R, A2, A3, [X|Z], MAPA).
generace2_(R, [X|A2], A3, Z, MAPA) :- poloz_h([X,X], MAPA), generace2_(R, A2, A3, [X|Z], MAPA).
generace2_(R, A2, [X|A3], Z, MAPA) :- poloz_h([X,X,X], MAPA), generace2_(R, A2, A3, [X|Z], MAPA).
generace2_(R, [], [], Z, MAPA) :- MAPA = [_,_,R3|_], poloz_h_([x,x],R3), 
    vypln_prazdne(MAPA, o), MAXR is R + (R // 2), cesta_iter(MAXR, rush_hour_krok, MAPA, cil, [x|Z], P), length(P, X), X >= R, write(P), vypis_matici(MAPA).

% vypln_prazdne(L, P) :- vsechny volne termy unifikuje s P
vypln_prazdne([], _) :- !.
vypln_prazdne([R|MAPA], P) :- vypln_prazdne_(R, P), vypln_prazdne(MAPA, P).
vypln_prazdne_([], _) :- !.
vypln_prazdne_([X|R], P) :- nonvar(X), vypln_prazdne_(R, P).
vypln_prazdne_([X|R], P) :- var(X), X=P, vypln_prazdne_(R, P).

% vypis_matici(+M) :- vypise matici M, po radach
vypis_matici([]).
vypis_matici([R|M]) :- write('\n'), write(R), vypis_matici(M).


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