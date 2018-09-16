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