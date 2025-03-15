# Määrittelydokumentti

Helsingin yliopiston TKT-kandiohjelman aineopintojen harjoitustyö.

## Aihe ja toteutus

Harjoitustyö toteutan shakkia pelaavan tekoälyn. Ohjelma saa ihmiseltä siirron, johon se laskee optimaalisen vastauksen. Pelin tekoälyssä hyödynnetään Minimax-algoritmia, jota optimoidaan alfa-beta karsinnalla. Harjoitustyössä käytetään kurssin tarjoamaa AI-platformia.

## Tavoitteena olevat aika- ja tilavaativuudet:

# Aikavaativuudet:

Minimax-algoritmin aikavaativuus on O(b<sup>d</sup>), jossa b on haaraumien määrä ja d on syvyys. b arvoa pyritään laskemeaan alfa-beta karsinnalla, joka pahimmassa mahdollisessa tapauksessa ei vaikuta aikavaatimukseen, eli se pysyy O(b<sup>d</sup>) ja parhaimmassa tapauksessa se voi laskea algoritmin aikavaativuuden O(√b<sup>d</sup>).

# Tilavaativuudet:

Minimax-algoritmin tilavaativuus on O(bd), jossa b on haaraumien määrä ja d on syvyys.

## Kielet

Harjoitustyön ohjelmointikieli on Haskell. (Tästä haluaisin kuitenkin vielä keskustella ohjaajan kanssa)
Vertaisarviointia varten tuttuja kieliä on JavaScript ja Python.
Dokumentaatio kirjoitetaan suomeksi, mutta koodi englanniksi.

## Lähteet

[Wikipedia Minimax](https://en.wikipedia.org/wiki/Minimax)
[Wikipedia Alpha-beta pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning)
[GeeksForGeeks Minimax](https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-1-introduction/)
