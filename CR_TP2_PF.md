# TP2 - Programmation Fonctionnelle
**Auteurs** : Baptiste Bouvier et Ancelin Serre \
**Date** : 07/09/2018 \
**Polytech INFO4**

**Site du TP :** [lien vers le TP2](http://www-verimag.imag.fr/~wack/APF/Poly-TP-18-19.pdf) \
**Cours utiles pour ce TP:**
- [Cours 1](http://www-verimag.imag.fr/~wack/APF/Cours01.pdf)
- [Cours 2](http://www-verimag.imag.fr/~wack/APF/Cours02.pdf)

*Note: un fichier OCaml (.ml) est présent dans le dépôt afin de jouer nos bouts de codes plus rapidement.*

-----
## Exercice 17

1) Définir le type contenu.
```ocaml
type contenu = Meuble | Objet | Cadre | Plante;;
```

2) Définir le type solidite.
```ocaml
type solidite = Fragile | Normal | Robuste;;
```

3) Définir le type paquet.
```ocaml
type paquet = contenu * solidite * int;;
```

## Exercice 18

Faire une fonction `fragiles` indiquant le nombre de paquets fragiles présents dans **l'inventaire** passé en argument.

```ocaml
(* On génère l'inventaire *)
let inventaire = [
    (Meuble,Robuste,50);
    (Meuble,Normal,17);
    (Objet,Fragile,0);
    (Cadre,Normal,9);
    (Plante,Fragile,0)
];;

(* Fonction fragiles *)
let rec fragiles inventaire =
    match inventaire with
    | [] -> 0
    | (contenu, solidite, poids)::s ->
        if solidite = Fragile then 1 + fragiles s else 0 + fragiles s
```

## Exercice 19

Faire une fonction `legers` qui prend en paramètre un poids et l'inventaire et rend la liste des paquets pesant au plus ce poids.

```ocaml
let rec legers p inventaire =
    match inventaire with
    | [] -> []
    | (contenu, solidite, poids)::s ->
        if poids <= p then
            (contenu, solidite, poids)::legers p s
        else
            legers p s;;
```

## Exercice 20

Ecrire une fonction `poids_plantes` qui indique le poids et l'inventaire et rend la liste de paquets au plus ce poids.

```ocaml
let rec poids_plantes inventaire =
    match inventaire with
    | [] -> 0
    | (contenu, solidite , poids)::s ->
        if contenu = Plante then
            poids + poids_plantes s
        else
            0 + poids_plantes s;;
```

## Exercice 21

Faire une fonction `exposition` qui retire tous les cadres d'un inventaire.

```ocaml
let rec exposition inventaire =
    match inventaire with
    | [] -> []
    | (contenu, solidite, poids)::s ->
        if contenu = Cadre then
            exposition s
        else
            (contenu, solidite, poids)::exposition s;;
```

## Exercice 22

Ecrire une fonction `inventorie` qui insère un paquet dans un inventaire par ordre croissant de poids. Nous supposons que l'inventaire est ordonné.

```ocaml
let rec inventorie inventaire (nc,ns,np) =
    match inventaire with
    | [] -> [(nc, ns, np)]
    | (contenu, solidite, poids)::next ->
        if np > poids then
            (contenu, solidite, poids)::inventorie next (nc,ns,np)
        else
            (nc, ns, np)::(contenu, solidite, poids)::next;;
```

## Exercice 23

Ecrire une fonction `dromadaire` qui prend l'inventaire en paramètre et indique le paquet le plus lourd.

```ocaml
let rec dromadaire inventaire =
    match inventaire with
    | [] -> failwith "Aucun paquet dans l'inventaire !"
    | (contenu, solidite, poids)::[] -> (contenu, solidite, poids)
    | (contenu, solidite, poids)::next ->
        let (nc, ns, np) = dromadaire next in
            if poids > np then
                (contenu, solidite, poids)
            else
                (nc, ns, np)
```

## Exercice 24

Ecrire une fonction `chameau` qui prend l'inventaire en paramètre et indique les deux plus lourds paquets.

```ocaml
let rec chameau inventaire =



```

## Exercice 25

Definir les types adaptés à la situation du magasin Tardy.

```ocaml
type genre = MP3 | Camera | Photo | Telephone | PC;;
type marque = Apple | Samsung | Sony | Lipship ;;
```

## Exercice 26

Écrire une fonction est en stock qui indique si un élément est présent en stock (c’est- à-dire si le nombre d’articles disponibles est strictement positif). Elle prend en argument un produit, une marque et un prix (caractéristiques de l’élément) et la liste des articles répertoriés.

```ocaml
let rec est_en_stock l marque genre prix =
  match l with
  | [] -> false
  | (m,g,p,s)::next -> if(m = marque && genre = g && prix = p && s > 0) then true else est_en_stock next marque genre prix;;
```

## Exercice 27

Écrire une fonction ajoute article qui ajoute un article dans la liste, en vérifiant qu’il n’y est pas déjà, et s’il y est déjà, modifie le nombre d’éléments en stock dans la liste en additionnant le nombre en stock de l’argument article. Elle prend en argument un article et la liste des articles répertoriés.

```ocaml
let rec ajouter l (marque,genre,prix,stock) =
  match l with
  | [] -> [(marque,genre,prix,stock)]
  | (m,g,p,s)::next -> if(m = marque && genre = g && prix = p && s > 0) then s = s + stock else ajouter next (marque,genre,prix,stock);;
```


## Exercice 28

Écrire une fonction enleve article qui enlève un article de la liste. Elle prend en argument la liste des articles répertoriés et l’article à enlever.

```ocaml
let rec enlever l marque genre =
  match l with
  | [] -> failwith "L'article n'est pas en rayon !"
  | (m,g,p,s)::next ->
    if m = marque && g = genre then
      next
    else
      (m,g,p,s)::enlever next marque genre;;
```

## Exercice 29

Écrire une fonction ces produits qui prend en argument un produit et une liste d’articles et renvoie la liste des articles qui conviennent dans la liste d’articles (ex : ces produits(MP3,L) renvoie tous les MP3 présents dans L).

```ocaml
let rec ces_produit l gm =
    match l with
    | [] -> []
    | (marque, genre, prix, stock)::next ->
        if gm = genre then
            (marque, genre, prix, stock)::ces_produit next gm
        else
            ces_produit next gm;;
```

## Exercice 30

Marc se rend compte que les clients choisissent généralement le deuxième produit le moins cher, i.e. si l’on classe tous les produits de la liste par ordre de prix croissant, le deuxième produit de cette liste. Sans classer tous les articles correspondant à un même produit par ordre croissant, écrivez une fonction deuxieme moins cher qui prend en argument une liste d’articles répertoriés, un produit, et renvoie le choix préféré des clients.

```ocaml
let rec deuxieme l genre =
  match l with
  | [] -> failwith "L'article n'est pas en rayon !"
  | (m,g,p,s)::[] ->
    if g = genre then
      [(m,g,p,s)]
   else
      failwith "L'article n'est pas en rayon !"
  | (m,g,p,s)::(m1,g1,p1,s1)::next ->
    if g = genre && g1 = genre then
      [(m1,g1,p1,s1)]
    else if g = genre && g1 != genre then
      deuxieme ((m1,g1,p1,s1)::next) genre
    else deuxieme next genre;;
```

## Exercice 31

Écrire une fonction budget qui prend en argument deux entiers m, budget minimal, et M budget maximal, ainsi qu’une liste d’articles, et renvoie la liste des articles compris dans ce budget (i.e. dont le prix p est tel que m≤p≤M).

```ocaml
let rec budget l pmin pmax =
    match l with
    | [] -> []
    | (marque, genre, prix, stock)::next ->
        if prix >= pmin && prix <= pmax then
            (marque, genre, prix, stock)::budget next pmin pmax
        else
            budget next pmin pmax;;
```

## Exercice 32

Écrire une fonction achete qui prend en argument une liste d’articles et les nom de produit, marque et prix pour un élément, et fait diminuer de 1 la quantité en stock d’un article.


```ocaml
let rec achete l marque genre prix =
  match l with
  | [] -> failwith "L'article n'est pas en rayon !"
  | (m,g,p,s)::next ->
    if(marque = m && genre = g && p = prix) then
      [(m,g,p,s-1)]
    else
      achete next marque genre prix;;
```

## Exercice 33

Écrire une fonction commande qui prend en argument la liste des articles et renvoie la liste des articles à commander au fournisseur (ceux dont le nombre en stock est nul).

```ocaml
let rec commande l =
  match l with
  | [] -> []
  | (m,g,p,s)::next ->
    if s = 0 then
      (m,g,p,s)::commande next
    else
      commande next;;
```

## Exercice 34

Écrire une fonction trouve min de type article list → article * article list qui calcule l’article le moins cher d’une liste non vide ainsi que la liste privée de cet article.

```ocaml
let rec trouve_min l =
    match l with
    | [] -> failwith "Aucun article en rayon!"
    | (marque, genre, prix, stock)::[] -> (marque, genre, prix, stock)
    | (marque, genre, prix, stock)::next ->
        let (m,g,p,s) = trouve_min next in
            if prix < p then
                (marque, genre, prix, stock)
            else
                (m,g,p,s);;
```

## Exercice 35

Écrire une fonction tri selection de type article list → article list qui trie la liste donnée en entrée par prix croissant en suivant l’algorithme du tri par sélection.

```ocaml
let rec tri_selection l =
    match l with
    | [] -> []
    | (m,g,p,s)::[] -> [(m,g,p,s)]
    | (m,g,p,s)::next -> let [(marque,genre,prix,stock)] = trouve_min l in
        (marque,genre,prix,stock)::(tri_selection (enlever l marque genre));;

```
