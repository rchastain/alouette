# Alouette

## Présentation

*Alouette* est un moteur UCI capable de jouer aux échecs aléatoires de Fischer, ou échecs 960. C'était au départ un exercice de programmation sur les *bitboards*, c'est-à-dire sur la représentation d'un damier au moyen d'un nombre entier à 64 chiffres binaires.

*Alouette* n'est pas un adversaire très redoutable. Vous devriez pouvoir le battre facilement.

## Livre d'ouvertures

À partir de la version 0.1.0, le programme est équipé d'un livre d'ouvertures au format décrit par [Kathe Spracklen](https://content.iospress.com/articles/icga-journal/icg6-1-04).

    (e4(e5(d4))(c5))(d4(d5))

Ce format a auparavant été utilisé par Marc-Philippe Huget dans le moteur d'échecs [La Dame Blanche](http://www.quarkchess.de/ladameblanche/).

## Générateur de coups aléatoires

L'exécutable nommé *random32* (ou *random64*) est un moteur qui joue des coups purement aléatoires. C'est presque le seul programme qu'*Alouette* arrive à battre !

## Remerciements

Merci à Philippe Guesset pour la [version optimisée](https://www.developpez.net/forums/d2001819-2/autres-langages/assembleur/x86-32-bits-64-bits/reecriture-pascal-d-fonction-assembleur/#post11124482) de la procédure permettant de compter les cases allumées, et à Mathieu Dalbin pour la [classe TTree](https://www.developpez.net/forums/d2034310/autres-langages/pascal/langage/representation-l-arbre-d-livre-d-ouvertures-aux-echecs/#post11310888) permettant de créer des arbres de coups.

## Auteur

*Alouette* est un programme à code source ouvert, écrit en Pascal par Roland Chastain.

## Nom du programme

> ALOUETTE [a-lou-è-t'] s. f. 
>   Oiseau de l'ordre des passereaux ; il fait son nid dans les plaines. S'éveiller, se lever au chant de l'alouette, se lever de très grand matin. 
> 
> ÉTYMOLOGIE
>   Wallon, alauie ; bourguig. auluôtte ; Berry, alouvette ; provenç. alauza, alauzeta ; ital. allodola, lodola ; anc. espagn. aloeta ; espagn. mod. alondra. Alouette est le diminutif d'aloue ; aloue vient du latin alauda ; mais alauda était un mot gaulois. Pline, Hist. nat. II, 37, et Suétone, Vie de César, nous apprennent que ce général avait donné à une de ses légions, composée d'hommes des Gaules, le nom gaulois d'alauda. L'oiseau huppé qu'en gaulois on nomme alauda, dit Marcellus Empiricus, ch. 29.

## Logo

![alt text](https://raw.githubusercontent.com/rchastain/alouette/master/logo.bmp)
