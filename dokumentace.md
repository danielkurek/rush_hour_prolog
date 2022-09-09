# Rush hour

## Generace
Můžeme si nechat vygenerovat rozložení aut s danou složitostí. U Rush Hour můžeme za složitost požadovat počet kroků potřebných k vyřešení daného rozložení, kde jeden krok je posunutí auta o 1 políčko dopředu/dozadu.

Na generaci nám slouží predikát `generace(+M, +N, +KROKY, +AUTA, -MAPA)`.
 - `M` a `N` nám udává velikost hracího pole (`M` řádků, `N` sloupců)
    - Minimální počet řádků je 3 a sloupců 2, kvůli umístění hlavního auta (které se má dostat do cíle)
 - `KROKY` udává kolik kroků je má nejkratší řešení
 - `AUTA` udává označení aut, které se může umisťovat na hrací plochu (nesmí to být `x`, to je vyhrazeno pro hlavní auto, ani `o`, to označuje prázdné místo)
 - `MAPA` je výsledné rozložení aut na mapě
Nebo můžeme využít predikát `generace(+M, +N, +KROKY, +AUTA2, +AUTA3, -MAPA)`, který je stejný jako předchozí, ale máme zvlášť seznam pro auto délky 2 (`AUTA2`) a délky 3 (`AUTA3`).

Generování není moc optimalizované, takže zvládá pouze menší vstupy. Rush Hour má totiž hodně stavů do kterých se dostat (pokud stav reprezentujeme rozložením aut na mapě). Lze to udělat i lépe, ale to není moc vhodné pro prolog.

## Řešení
Na vyřešení předem zadaného rozložení nám slouží predikát `rush_hour(+MAPA, +AUTA, -CESTA)`.
 - `MAPA` je matice (list listů), která udává rozložení aut
   - jednotlivá políčka mají označení
     - `x` pro hlavní auto, které se má dostat do cíle
     - `o` pro prázdné políčko
     - libovolný jiný term pro označení dalších aut (doporučuji použít písmena od `a`)
   - rozložení musí být správné, jinak je chování nedefinované
     - např. auta nesmí být rozdělena
     - musí být rovná a mít délku 2 nebo 3
     - nesmí být více aut se stejným označením
 - `AUTA` je seznam termů, které označují auta na mapě (všech aut, včetně hlavního auta `x`)
 - `CESTA` je vysledná nejkratší posloupnost kroků, která vede k vyřešení daného rozložení
   - `CESTA` je seznam kroků, kde jeden krok je dvojice symbolu auta, s kterým se pohybuje, a směrem `b`=dozadu nebo `f`=dopředu (dopředu je dolů nebo doprava, dozadu je nahoru nebo doleva)

## Automatické testy
Součástí řešení je také sada testů, které testují základní funkčnost důležitých predikátů pro korektní běh řešení.

Testy se můžou spustit příkazem `swipl -g run_tests -t halt rush_hour.plt`, kde  `rush_hour.plt` je soubor s testy.

## Ukázkové vstupy

```prolog

?- rush_hour([[o,o,o],[o,o,o],[x,x,o]], [x], P).
P = [[x, f]].

?- rush_hour([[o,b,b],[o,o,a],[x,x,a]], [a,b,x], P).
P = [[b, b], [a, b], [x, f]].

?- rush_hour([
    [d,c,c,c],
    [d,b,b,b],
    [x,x,o,a],
    [e,e,o,a]], [a,b,c,d,e,x], P) ; true.
P = [[e, f], [x, f], [d, f], [d, f], [b, b], [a, b], [c, b], [a|...], [...|...]] [write]
P = [[e, f], [x, f], [d, f], [d, f], [b, b], [a, b], [c, b], [a, b], [x, f]].

?- generace(4,4,5,[a,b,c,d],V).
[[d,b],[c,b],[b,b],[b,b],[x,f]]
[o,o,c,c]
[o,o,d,d]
[a,x,x,b]
[a,o,o,b]
V = [[o, o, c, c], [o, o, d, d], [a, x, x, b], [a, o, o, b]] ;
[[d,b],[c,b],[b,b],[b,b],[x,f]]
[o,o,c,c]
[o,d,d,d]
[a,x,x,b]
[a,o,o,b]
V = [[o, o, c, c], [o, d, d, d], [a, x, x, b], [a, o, o, b]] .

?- generace(3,5,6,[a,b,c,d,e],V).
[[d,b],[d,b],[c,b],[b,b],[x,f],[x,f]]
[o,o,o,d,d]
[a,e,e,b,c]
[a,x,x,b,c]
V = [[o, o, o, d, d], [a, e, e, b, c], [a, x, x, b, c]] .

```