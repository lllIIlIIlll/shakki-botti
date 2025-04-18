# Käyttöohje

#### Ohjelman asennus ja suoritus

1. Kloonaa repositorio.

2. Varmista että sinulla on Haskellin projektinhallintatyökalu stack asennettuna. Mikäli ei, niin [täältä](https://docs.haskellstack.org/en/stable/) löytyy ohjeet asennukseen.

3. Asenna riippuvuudet komennolla

```
stack build
```

4. Jos haluat suorittaa ohjelmaa komentorivillä, se onnistuu komennolla:

```
stack exec chess-engine-exe
```

Suosittelen kuitenkin käyttämään kurssin tarjoamaa [teköälyalustaa](https://github.com/game-ai-platform-team/tira-ai-local).

#### Testit

Testien suorittaminen tapahtuu komennolla:

```
stack test
```

- Ensimmäisellä suorituskerralla komento asentaa testiriippuvuudet.

Testikattavuusraportin generointi komennolla:

```
stack test --coverage
```

#### Linttaus

Ensiksi hlint tulisi asentaa globaalisti komennolla:

```
stack install hlint
```

Tämän jälkeen linttaus tapahtuu komennolla:

```
hlint .
```
