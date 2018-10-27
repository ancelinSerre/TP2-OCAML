(* Exercice 17 *)
(*-------------*)

(* Définition du type contenu *)
type contenu = Meuble | Objet | Cadre | Plante;;
(* Définition du type solidité *)
type solidite = Fragile | Normal | Robuste;;
(* Définition du type paquet *)
type paquet = contenu * solidite * int;;

(* Exercice 18 *)
(*-------------*)

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
      if solidite = Fragile then 1 + fragiles s else 0 + fragiles s;;

(* Exercice 19 *)
(*-------------*)

let rec legers p inventaire =
  match inventaire with
  | [] -> []
  | (contenu, solidite, poids)::s ->
    if poids <= p then
      (contenu, solidite, poids)::legers p s
    else
      legers p s;;

(* Exercice 20 *)
(*-------------*)

let rec poids_plantes inventaire =
  match inventaire with
  | [] -> 0
  | (contenu, solidite , poids)::s ->
    if contenu = Plante then
      poids + poids_plantes s
    else
      0 + poids_plantes s;;
          
(* Exercice 21 *)
(*-------------*)

let rec exposition inventaire =
  match inventaire with
  | [] -> []
  | (contenu, solidite, poids)::s ->
    if contenu = Cadre then
      exposition s
    else
      (contenu, solidite, poids)::exposition s;;

(* Exercice 22 *)
(*-------------*)

let rec inventorie inventaire (nc,ns,np) =
  match inventaire with
  | [] -> [(nc, ns, np)]
  | (contenu, solidite, poids)::next ->
    if np > poids then
      (contenu, solidite, poids)::inventorie next (nc,ns,np)
    else
      (nc, ns, np)::(contenu, solidite, poids)::next;;

(* Exercice 23 *)
(*-------------*)

let rec dromadaire inventaire =
  match inventaire with
  | [] -> failwith "Aucun paquet dans l'inventaire !"
  | (contenu, solidite, poids)::[] -> (contenu, solidite, poids)
  | (contenu, solidite, poids)::next ->
    let (nc, ns, np) = dromadaire next in
      if poids > np then
        (contenu, solidite, poids)
      else
        (nc, ns, np);;

(* Exercice 24 *)
(*-------------*)

let rec chameau inventaire =
  (* TODO *)
;;

(* Exercice 25 *)
(*-------------*)

(* Définition type genre *)
type genre = MP3 | Camera | Photo | Telephone | PC;;
(* Définition type marque *)
type marque = Apple | Samsung | Sony | Lipship ;

(* Exercice 26 *)
(*-------------*)

let rec est_en_stock l marque genre prix =
  match l with
  | [] -> false
  | (m,g,p,s)::next -> 
    if(m = marque && genre = g && prix = p && s > 0) then 
      true 
    else 
      est_en_stock next marque genre prix;;

(* Exercice 27 *)
(*-------------*)

let rec ajouter l (marque,genre,prix,stock) =
  match l with
  | [] -> [(marque,genre,prix,stock)]
  | (m,g,p,s)::next -> 
    if(m = marque && genre = g && prix = p && s > 0) then 
      s = s + stock 
    else 
      ajouter next (marque,genre,prix,stock);;

(* Exercice 28 *)
(*-------------*)

let rec enlever l marque genre =
  match l with
  | [] -> failwith "L'article n'est pas en rayon !"
  | (m,g,p,s)::next ->
    if m = marque && g = genre then
      next
    else
      (m,g,p,s)::enlever next marque genre;;

(* Exercice 29 *)
(*-------------*)

let rec ces_produit l gm =
  match l with
  | [] -> []
  | (marque, genre, prix, stock)::next ->
    if gm = genre then
      (marque, genre, prix, stock)::ces_produit next gm
    else
      ces_produit next gm;;

(* Exercice 30 *)
(*-------------*)

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
    else 
      deuxieme next genre;;

(* Exercice 31 *)
(*-------------*)

let rec budget l pmin pmax =
  match l with
  | [] -> []
  | (marque, genre, prix, stock)::next ->
    if prix >= pmin && prix <= pmax then
      (marque, genre, prix, stock)::budget next pmin pmax
    else
      budget next pmin pmax;;

(* Exercice 32 *)
(*-------------*)

let rec achete l marque genre prix =
  match l with
  | [] -> failwith "L'article n'est pas en rayon !"
  | (m,g,p,s)::next ->
    if(marque = m && genre = g && p = prix) then
      [(m,g,p,s-1)]
    else
      achete next marque genre prix;;

(* Exercice 33 *)
(*-------------*)

let rec commande l =
  match l with
  | [] -> []
  | (m,g,p,s)::next ->
    if s = 0 then
      (m,g,p,s)::commande next
    else
      commande next;;

(* Exercice 34 *)
(*-------------*)

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

(* Exercice 35 *)
(*-------------*)

let rec tri_selection l =
  match l with
  | [] -> []
  | (m,g,p,s)::[] -> [(m,g,p,s)]
  | (m,g,p,s)::next -> 
    let [(marque,genre,prix,stock)] = trouve_min l in
    (marque,genre,prix,stock)::(tri_selection (enlever l marque genre));;