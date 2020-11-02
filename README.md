# Alouette

## Overview

*Alouette* is a UCI chess engine able to play Fischer Random Chess. It was initially a programming exercise around *bitboards*.

It is not a very formidable opponent. You should be able to beat him easily.

## Opening book

Since the version 0.1.0, the engine uses an opening book. The book is in the [format described by Kathe Spracklen](https://content.iospress.com/articles/icga-journal/icg6-1-04).

    (e4(e5(d4))(c5))(d4(d5))

This format has been previously used by Marc-Philippe Huget in his engine [La Dame Blanche](http://www.quarkchess.de/ladameblanche/).

## Random mover

The executable named *random32* (or *random64*) is a pure random mover.

## Credits

Thanks to:

* Philippe Guesset for the [optimized version](https://www.developpez.net/forums/d2001819-2/autres-langages/assembleur/x86-32-bits-64-bits/reecriture-pascal-d-fonction-assembleur/#post11124482) of the function allowing to count the bits
* Mathieu Dalbin for the [TTree class](https://www.developpez.net/forums/d2034310/autres-langages/pascal/langage/representation-l-arbre-d-livre-d-ouvertures-aux-echecs/#post11310888) allowing to create trees of moves

## Author

*Alouette* is an open source program written for the Free Pascal Compiler by Roland Chastain.

## Logo

![alt text](https://raw.githubusercontent.com/rchastain/alouette/master/logo/logo.bmp)
