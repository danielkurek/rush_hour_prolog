:- use_module(['rush_hour.pl']).

:- begin_tests(rush_hour_test).


test("pocet - overit") :-
    pocet(a, [a,a,b,c,d,e,f,a], 3).
test("pocet - spocitat") :-
    pocet(a, [a,a,b,c,a,a,a,d,e,f,a], V), V =:= 6.
test("pocet - prazdne") :-
    pocet(a, [], V), V =:= 0.
test("pocet - 0") :-
    pocet(a, [b,c,d,e,g,b,c,d], V), V =:= 0.



test("posledni - spravne") :-
    posledni_dva(a, [a,a,b,c,d,e,a,a]).
test("posledni - spatne", [fail]) :-
    posledni_dva(x, [a,b,c,d]).
test("posledni - spatne, pouze 1", [fail]) :-
    posledni_dva(x, [a,b,c,x]).
test("posledni - prazdne", [fail]) :-
    posledni_dva(a, []).



test("nahrad prvni - spravne") :-
    nahrad_prvni(x, g, [a,b,c,d,e,f,x,h], [a,b,c,d,e,f,g,h]).
test("nahrad prvni - spravne vraceni") :-
    nahrad_prvni(x, g, [a,b,c,d,e,f,x,h], L), L = [a,b,c,d,e,f,g,h].
test("nahrad prvni - spatne - nahradi vice", [fail]) :-
    nahrad_prvni(x, g, [a,b,c,d,e,f,x,x], [a,b,c,d,e,f,g,g]).
test("nahrad prvni - spatne", [fail]) :-
    nahrad_prvni(x, g, [a,b,c,d,e,f,x,h], [a,b,c,d,e,f,x,h]).
test("nahrad prvni - nic nenahradi") :-
    nahrad_prvni(x, g, [a,b,c,d,e,f,g,h], [a,b,c,d,e,f,g,h]).
test("nahrad prvni - prazdne") :-
    nahrad_prvni(x, g, [], []).



test("nahrad prvek na - spravne") :-
    nahrad_prvek_na(5, a, [o,o,o,o,o,o], L), L = [o,o,o,o,o,a].
test("nahrad prvek na - spatne - neprazdne", [fail]) :-
    nahrad_prvek_na(5, a, [o,o,o,o,o,b], L), L = [o,o,o,o,o,a].
test("nahrad prvek na - spatne - male", [fail]) :-
    nahrad_prvek_na(5, a, [o,o,o,o,o], _).



% test dopredu - dolu
test("dopredu - dolu spravne 1") :-
dopredu(x, [[o,o,o,o,o,o],
            [o,o,x,o,o,o],
            [o,o,x,o,o,o],
            [o,o,x,o,o,o],
            [o,o,o,o,o,o],
            [o,o,o,o,o,o]], V), 
                V = [[o,o,o,o,o,o],
                     [o,o,o,o,o,o],
                     [o,o,x,o,o,o],
                     [o,o,x,o,o,o],
                     [o,o,x,o,o,o],
                     [o,o,o,o,o,o]].
test("dopredu - dolu spravne 2") :-
    dopredu(x, [[o,o,x,o,o,o],
                [o,o,x,o,o,o],
                [o,o,x,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].

test("dopredu - dolu spravne 3") :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,x,o,o,o],
                [o,o,x,o,o,o],
                [o,o,x,o,o,o],
                [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,x,o,o,o]].
test("dopredu - dolu spravne 4") :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [x,o,o,o,o,o],
                [x,o,o,o,o,o],
                [x,o,o,o,o,o],
                [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [x,o,o,o,o,o],
                         [x,o,o,o,o,o],
                         [x,o,o,o,o,o]].
test("dopredu - dolu spravne 5") :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,x],
                [o,o,o,o,o,x],
                [o,o,o,o,o,x],
                [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,x],
                         [o,o,o,o,o,x],
                         [o,o,o,o,o,x]].
test("dopredu - dolu spatne 1", [fail]) :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,x],
                [o,o,o,o,o,x],
                [o,o,o,o,o,x],
                [o,o,o,o,o,a]], _).
test("dopredu - dolu spatne 2", [fail]) :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,x],
                [o,o,o,o,o,x],
                [o,o,o,o,o,x]], _).
test("dopredu - dolu spatne 3", [fail]) :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [x,o,o,o,o,o],
                [x,o,o,o,o,o],
                [x,o,o,o,o,o]], _).
test("dopredu - dolu spatne 4", [fail]) :-
    dopredu(x, [[o,o,o,o,o,x],
                [o,o,o,o,o,x],
                [o,o,o,o,o,x],
                [o,o,o,o,a,a],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o]], _).



% test dopredu - doprava
test("dopredu - doprava spravne 1") :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,x,x,x,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,x,x,x,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dopredu - doprava spravne 2") :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,x,x,x,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,x,x,x]].
test("dopredu - doprava spravne 3") :-
    dopredu(x, [[o,o,x,x,x,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,x,x,x],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dopredu - doprava spravne 4") :-
    dopredu(x, [[x,x,x,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o]], V), 
                    V = [[o,x,x,x,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dopredu - doprava spatne 1", [fail]) :-
    dopredu(x, [[o,o,o,x,x,x],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o]], _).
test("dopredu - doprava spatne 2", [fail]) :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,o,x,x,x]], _).
test("dopredu - doprava spatne 3", [fail]) :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,x,x,x,a,o],
                [o,o,o,o,a,o],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o]], _).
test("dopredu - doprava spatne 4", [fail]) :-
    dopredu(x, [[o,o,o,o,o,o],
                [o,o,o,o,o,o],
                [o,o,x,x,x,a],
                [o,o,o,o,o,a],
                [o,o,o,o,o,o],
                [o,o,o,o,o,o]], _).



% test dozadu - nahoru
test("dozadu - nahoru spravne 1") :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,x,o,o,o],
               [o,o,x,o,o,o],
               [o,o,x,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], V), 
                    V = [[o,o,x,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dozadu - nahoru spravne 2") :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,x,o,o,o],
               [o,o,x,o,o,o],
               [o,o,x,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,x,o,o,o],
                         [o,o,o,o,o,o]].

test("dozadu - nahoru spravne 3") :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [x,o,o,o,o,o],
               [x,o,o,o,o,o],
               [x,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [x,o,o,o,o,o],
                         [x,o,o,o,o,o],
                         [x,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dozadu - nahoru spravne 4") :-
    dozadu(x, [[o,o,o,o,o,o],
               [x,o,o,o,o,o],
               [x,o,o,o,o,o],
               [x,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], V), 
                    V = [[x,o,o,o,o,o],
                         [x,o,o,o,o,o],
                         [x,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dozadu - nahoru spravne 5") :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,x],
               [o,o,o,o,o,x],
               [o,o,o,o,o,x],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,x],
                         [o,o,o,o,o,x],
                         [o,o,o,o,o,x],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dozadu - nahoru spatne 1", [fail]) :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,a,a],
               [o,o,o,o,o,x],
               [o,o,o,o,o,x],
               [o,o,o,o,o,x],
               [o,o,o,o,o,o]], _).
test("dozadu - nahoru spatne 2", [fail]) :-
    dozadu(x, [[o,o,o,o,o,x],
               [o,o,o,o,o,x],
               [o,o,o,o,o,x],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], _).
test("dozadu - nahoru spatne 3", [fail]) :-
    dozadu(x, [[x,o,o,o,o,o],
               [x,o,o,o,o,o],
               [x,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], _).
test("dozadu - nahoru spatne 4", [fail]) :-
    dozadu(x, [[o,o,o,x,o,o],
               [o,o,o,x,o,o],
               [o,o,o,x,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], _).
test("dozadu - nahoru spatne 5", [fail]) :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,a,a,o,o],
               [o,o,x,o,o,o],
               [o,o,x,o,o,o],
               [o,o,x,o,o,o]], _).



% test dozadu - doleva
test("dozadu - doleva spravne 1") :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,x,x,x,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [x,x,x,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dozadu - doleva spravne 2") :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,x,x,x,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [x,x,x,o,o,o]].
test("dozadu - doleva spravne 3") :-
    dozadu(x, [[o,x,x,x,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], V), 
                    V = [[x,x,x,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dozadu - doleva spravne 4") :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,x,x,x],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], V), 
                    V = [[o,o,o,o,o,o],
                         [o,o,x,x,x,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o],
                         [o,o,o,o,o,o]].
test("dozadu - doleva spatne 1", [fail]) :-
    dozadu(x, [[x,x,x,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], _).
test("dozadu - doleva spatne 2", [fail]) :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [x,x,x,o,o,o]], _).
test("dozadu - doleva spatne 3", [fail]) :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [a,x,x,x,o,o],
               [a,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], _).
test("dozadu - doleva spatne 4", [fail]) :-
    dozadu(x, [[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [a,x,x,x,o,o],
               [a,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], _).


% test reseni rush_hour
test("rush_hour - jednoduche") :-
    rush_hour([[o,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,x,x,o,o,o],
               [a,o,o,o,o,o],
               [o,o,o,o,o,o],
               [o,o,o,o,o,o]], cil, P, [x]),
               P = [[x,f],[x,f],[x,f]].
:- end_tests(rush_hour_test).