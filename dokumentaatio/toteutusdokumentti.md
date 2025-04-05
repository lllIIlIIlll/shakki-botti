# Toteutusdokumentti

## Ohjelman yleisrakenne

Ohjelma on shakkimoottori, joka hyödyntää minimax-algoritmia sekä alfa-beta-karsintaa. Algoritmi simuloi kaikkia mahdollisia siirtosarjoja nykyisestä laudan tilanteesta eteenpäin, evaluoi ne ja palauttaa parhaimman mahdollisen evaluaation ja siirron saavuttaessaan määritetyn syvyyden (tai laillisten siirtojen loppuessa, jolloin peli on päättynyt joko shakkimattiin tai pattitilanteeseen). Afla-beta-karsinta tehostaa tätä mahdollisten siirtosarjojen simulointia jättämällä simuloimatta ne haarat/siirtosarjat joiden evaluaatio ei voi olla parempi kun jo löydetty evaluaatio.

Ohjelma on tarkoitettu käytettäksi kurssin tarjoaman AI-platformin kanssa, mutta sitä voi myös suorittaa komentoriviltä.

## Tila- ja aikavaativuudet

Minimax-algoritmin aikavaativuus on O(b<sup>d</sup>), jossa b on mahdollisten siirtojen/haaraumien määrä ja d on hakusyvyys. Hakusyvyyden kasvaessa myös evaluoitavien laudan tilojen määrä kasvaa eksponentiaalisesti. Tätä saadaan nopeutettua alfa-beta-karsinnalla, sekä laittamalla käsiteltävät siirrot järjestykseen siten, että lupaavimmat siirrot, esimerkiksi lyönnit käsitellään ensiksi. Alfa-beta-karsinta käytännössä vähentää tutkittavien haarojen määrää, joka teoriassa parhaimmassa tapauksessa voi laskea algoritmin aikavaativuuden O(√b<sup>d</sup>). Pahimmassa mahdollisessa tapauksessa ei vaikuta algoritmin aikavaativuuteen, eli se pysyy O(b<sup>d</sup>).

## Puutteet ja parannusehdotukset

Ohjelmaa on vaikea verrata muihin shakkimoottoreihin, sillä siitä puuttuu tornitus ja ohestalyönti (tämä saattaa muuttua vielä jos aikaa riittää). Tämän lisäksi shakkimoottori ei osaa pelata loppupeliä, sillä syvyys millä se tällä hetkellä toimii (maks. 3 siirtoparia) ei riitä laskemaan shakkimattiin saakka, vaan ohjelma alkaa pelaamaan päättömiä siirtoja.

## Laajojen kielimallien käyttö

Laajojen kielimallien käyttö on kohdistunut lähinnä ideointiin ja se on toiminut myös Haskellin dokumentaationa. Ideoinnilla tarkoitan esimerkiksi lähestymistapojen tiedustelua, esimerkiksi millaisia komponentteja shakkimoottorit käyttävät yleisellä tasolla. Käytetty ChatGPT o3 miniä.

## Viitteet

[Wikipedia Minimax](https://en.wikipedia.org/wiki/Minimax)  
[Wikipedia Alpha-beta pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning)  
[GeeksForGeeks Minimax](https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-1-introduction/)
