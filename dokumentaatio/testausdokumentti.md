# Testausdokumentti

Ohjelman testauksessa on käytetty Hspec-kirjastoa. Testit ajetaan komennolla stack test ja testikattavuusraportin sa generoitua komennolla stack test --coverage.

### Testikattavuus

![testikattavuusraportti](./coverage.png)

### Mitä testattu

Pelin tilan päivitys.

- fenToGameState luo annetusta fen-merkkijonosta pelitilan, jota ohjelma käyttää pelin kulun seuraamiseen.
- updateGameState päivittää pelitilan pelatun siirron mukaiseksi.
- Pelitila ei ota kantaa tornitukseen, ohestalyöntiin tai siirtolukuihin. Nämä on kuitenkin lisätty, jotta toimintojen lisääminen olisi myöhemmässä vaiheessa mahdollista.

Ohjelma löytää yhden, kahden ja kolmen siirron shakkimatit.

- Testitapaukset alustettu antamalla laudan tila, jossa ohjelmalla on varma shakkimatti tietyllä syvyydellä.
- Kutsutaan findBestMove-funktiota ja verrataan ohjelman palauttamaa siirtoa siirtoon, joka johtaa shakkimattiin. Tarkistetaan myös että funktion palauttama voiton evaluaatio vastaa odotettua syvyyttä.

Ohjelma osaa evaluoida sille annetun pelin tilan.

- evaluateBoard-funktio palauttaa evaluaation, jossa huomioidaan laudalla oleva materiaali ja nappuloiden sijainti.
