(* 
BIGNON Alan 685L E197700R
Tasyurek Ekin 685K Erasmus

Projet Prgrammation Fonctionnelle 
Sujet 2 : Automate Cellulaire
pour lancer : ocamlfind ocamlc main.ml -o main -linkpkg -package graphics -package unix
puis ./main
*)
open Graphics;; (* pour l'affichage graphique *)
open Unix;;
(* ------------------ Les types de bases pour notre automate ------------------ *)
type state = Alive | Dead;;

type cellule = Cellule of state * int * int ;;

type voisinage = Voisinage of (int * int) list;;

type automate = Automate of voisinage * (cellule list);;


(* ------------------ Fonctions permettant la création d'un automate avec toutes ces cellules mortes ------------------ *)
(* Cette fonction crée une liste de cellulue de même coordonées y *)
let rec create_column_auto i m = 
    if i = 0
    then
        [Cellule (Dead, i, m)]
    else 
        Cellule (Dead, i, m)::create_column_auto (i-1) m
;;

(* Cette fonction crée une liste de cellule avec des coordonnées allant de (0,0) à (n,m) *)
let rec create_automate n m =
    if m = 0
    then
        create_column_auto n m
    else
        (create_column_auto n m)@create_automate n (m-1)
;;

(* ------------------ Fonctions permettant l'affichage graphique de l'automate ------------------ *)
(* Cette fonction change la couleur courante du graphique, cela nous permet de dessiner des carrés noir pour les vivant et blanc pour les morts *)
let change_color state = 
    match state with
    | Dead -> Graphics.set_color white
    | Alive -> Graphics.set_color black
;;

(* Cette fonction prend une cellule et la taille de la cellule dans le graph et change la couleur courante puis dessine le carré correspondant à la cellule *)
let cellule_to_graph cell size = 
    match cell with
    | Cellule(state, x, y) -> change_color state; Graphics.fill_rect (x*size) (y*size) size size;
;;

(* Cette fonction va, pour chaque cellule dans la liste, appeller la fonction cellule_to_graph avec cette cellule *)
let rec auto_to_graph_l list size =
    match list with
    | h::l -> cellule_to_graph h size; auto_to_graph_l l size;
    | [] -> print_string("");
;;

(* Prend un automate et la taille du coté des cellules et dessine dans le graph *)
let auto_to_graph auto sizeCell =
    match auto with
    | Automate(_, list)-> auto_to_graph_l list sizeCell
;;

(* ------------------ Fonctions permettant le changement d'état d'une cellule ------------------ *)
(* Cette fonction prend un état et renvoie l'opposé *)
let get_opposite_state state =
    match state with
    | Dead -> Alive
    | Alive -> Dead
;;

(* cette fonction prend une cellule et des coordonées
    si la cellule se trouve au coordonnées données alors on change son état sinon on ne fait rien *)
let sameCoord cell x y = 
    match cell with
    | Cellule(state, a, b) -> if a = x && b = y then Cellule(get_opposite_state state, a, b) else cell 
;;

let rec auto_change_state_l l x y =
    match l with
    | h::t -> sameCoord h x y::auto_change_state_l t x y
    | [] -> []
;;
(* Cette fonction permet de changer l'état d'une cellule situé en coordonnées (x,y) *)
let auto_change_state auto x y = 
    match auto with
    | Automate(v, cell_l_l) -> Automate(v, (auto_change_state_l cell_l_l x y))
;;

(* ------------------ Fonctions permettant le changement d'état de certaines cellules, selon une liste de coordonées ------------------ *)

let get_x h = 
    match h with
    | (x, _) -> x
;;

let get_y h = 
    match h with
    | (_, y) -> y
;;

let rec auto_etat_initiale auto l = 
    match l with
    | [] -> auto
    | h::t -> auto_etat_initiale (auto_change_state auto (get_x h) (get_y h)) t
;;

(* ------------------ Fonctions permettant de récupérer l'état d'une cellule ------------------ *)

(* Fonction qui test si une cellule est en coordonées (x,y) *)
let is_there cell x y =
    match cell with
    | Cellule(state, a, b) -> if a = x && b=y then true else false
;;

(* Fontion qui récupère l'état d'une cellule *)
let get_state cell =
    match cell with
    | Cellule (state, _,_) -> state
;;

(* Fonction qui récupère l'état d'une cellule en coordonées (x,y) *)
let rec get_state_l l x y =
    match l with
    | h::t -> if is_there h x y then get_state h else get_state_l t x y 
    | _ -> failwith "erreur x et y pas dans le tableau"
;;
(* Fonction qui prend un automate et renvoie l'état de la cellule se situant en coordonées (x,y) *)
let get_state auto x y =
    match auto with
    | Automate(v, l) -> get_state_l l x y 
;;

(* ------------------ Fonctions permettant savoir le nombre de voisin vivant des cellules ------------------ *)

let get_x cell = 
    match cell with
    | Cellule (_ ,x, _)-> x 
;;
let get_y cell =
    match cell with
    | Cellule (_,_,y) -> y
;;

(* Fonction qui test si une cellule est vivante et qui renvoie un boolean, true si la cellule est vivante, false sinon *)
let isAlive cell = 
    match cell with
    | Cellule(state, _,_ )-> match state with
                            | Dead -> false
                            | Alive -> true
;;

(* fonction qui teste si une cellule est voisine de la cellule située en (x,y), si elle est voisine et vivante on renvoie 1 sinon 0 *)
let rec is_voisin cell voisin x y n m =
    if not(isAlive cell)
    then 
        0
    else
        match voisin with
        | (a, b)::t -> if get_x cell + a >= n || get_y cell + b >= m || get_x cell + a < 0 || get_y cell + b <0 
                    then 
                    0 + is_voisin cell t x y n m
                    else 
                        begin 
                            if get_x cell + a = x && get_y cell + b = y 
                            then 
                                1
                            else 
                                0 + is_voisin cell t x y n m 
                        end
        | _ -> 0
;;
(* Cette fonction permet d'avoir le nombre de voisin vivant d'une cellule *)
let rec get_nb_voisin_alive_l l l_voisin x y n m=
    match l with 
    | h::t -> is_voisin h l_voisin x y n m + get_nb_voisin_alive_l t l_voisin x y n m
    | [] -> 0 
;;

(* Cette fonction permet de récupérer la liste des voisin *)
let get_liste_voisin voisin = 
    match voisin with
    | Voisinage(l) -> l
;;
(* Cette fonction permet de récupérer le nombre de cellule vivante voisine de la cellule situé en (x,y), elle appel get_nb_voisin_alive_l  *)
let rec get_nb_voisin_alive auto x y n m =
    match auto with
    | Automate(v, l) -> get_nb_voisin_alive_l l (get_liste_voisin v) x y n m
;;

(* ------------------ Fonctions permettant de passer d'un état d'automate au suivant ------------------ *)

(* détermine le nouvel état d'une cellule selon son nombre de cellule vivante voisine *)
let next_state_in auto x y n m =
    let state = get_state auto x y in
    let nbVoisinAlive = get_nb_voisin_alive auto x y n m in
    match state with 
    | Dead -> if nbVoisinAlive = 3 then [(x,y)] else []
    | Alive -> if nbVoisinAlive = 2 || nbVoisinAlive = 3 then [] else [(x, y);]
;;

(* Fonction permettant de récupérer la liste de coordonnées des cellules qui doivent changer d'état *)
let rec next_state_rec auto l_changement n m x y  =
    if n-1=x && m-1 = y
    then
        l_changement@next_state_in auto x y n m
    else
        match auto with
        | Automate(voisin, l) -> next_state_rec auto (l_changement@(next_state_in auto x y n m)) n m (if x = n-1 then 0 else x+1) (if x=n-1 then y+1 else y)
;;

(* Fonction secondaire *)
let next_state_list auto n m =
    next_state_rec auto [] n m 0 0
;;
(* Cette fonction renvoie un automate qui a passé une génération on appelle cette fonction *)
let next_state auto n m = 
    auto_etat_initiale auto (next_state_list auto n m)
    (* On utilise la fonction qui prend une liste de coordonnées et change les états correspondant *)
;;

(* ------------------ Fonctions gérant le changement d'état graphique d'un automate ------------------ *)

(* Cette fonction gère deux évenements, le premier étant le passage a l'état suivant, l'autre étant
le changement d'état d'une cellule par l'utilisateur *)
let rec boucle_auto_state auto tailleCellule nbColumn nbLine =
    let event = wait_next_event[Graphics.Button_down; Graphics.Key_pressed] in
    if event.keypressed then
        match event.Graphics.key with
        | 'e' -> () (* si on appuie sur e on quitte le programme *)
        | _ -> begin 
            let nextAutoKey = next_state auto nbColumn nbLine in
            ignore clear_graph;
            auto_to_graph nextAutoKey tailleCellule;
            boucle_auto_state nextAutoKey tailleCellule nbColumn nbLine;
            end
    else if event.button then
        (* on récupère les 2 coordonées *)
        let subx = float_of_int(event.mouse_x) in
        let x = (subx/.(float_of_int(tailleCellule)*.float_of_int(nbColumn)))*.float_of_int(nbColumn) in 

        let suby = float_of_int(event.mouse_y) in
        let y = (suby/.(float_of_int(tailleCellule)*.float_of_int(nbColumn)))*.float_of_int(nbColumn) in

        let coord = [int_of_float(x) , int_of_float(y)] in

        (* on change l'état de la cellule citué à ces coordonnées *)
        let nextAutoButton = auto_etat_initiale auto coord in
        (* on redessine le graphique *)
        ignore clear_graph;
        auto_to_graph nextAutoButton tailleCellule;
        boucle_auto_state nextAutoButton tailleCellule nbColumn nbLine;
;;

(* ------------------ Fonctions implémentant différents états initiaux du jeu de la vie ------------------ *)

(* fonction qui lance un automate avec un glider gun présent *)
let start_auto_glider_gun nbColumn nbLine tailleEcran =
    let tailleCellule = tailleEcran/nbColumn in
    let voisin = Voisinage [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1, 0); (1, 1)] in
    let auto = Automate(voisin , create_automate nbColumn nbLine) in
    let etat_initiale = [(2,45); (2,44); (3,45); (3,44); 
        (12,45);(12,44);(12,43);(13,46);(14,47);(15,47); (13,42); (14,41);(15,41); 
        (16,44); (17,46);(18,45);(18,44);(18,43);(19,44);(17,42);
        (22,47);(22,46);(22,45);(23,47);(23,46);(23,45);(24,48);(24,44);
        (26,49);(26,48);(26,44);(26,43);
        (36,47);(36,46);(37,47);(37,46);] in

    Graphics.set_window_title ("Press any key to go to next state and E to exit");

    let auto = auto_etat_initiale auto etat_initiale in

    auto_to_graph auto tailleCellule;

    (* on boucle à l'infini avec cette appel *)
    boucle_auto_state auto tailleCellule nbColumn nbLine ;
;;

(* fonction qui lance un automate qui as quelques structures *)
let start_auto_basique nbColumn nbLine tailleEcran =
    let tailleCellule = tailleEcran/nbColumn in
    let voisin = Voisinage [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1, 0); (1, 1)] in
    let auto = Automate(voisin , create_automate nbColumn nbLine) in

    let etat_initiale = [(2,48);(2,47);(3,46);(5,48);(4,49);(5,47);
                        (10,48);(11,49);(12,49);(13,49);(14,49);(14,48);(14,47);(13,46);(10,46);
                        (11,43);(12,42);(12,41);(12,40);(11,39);(10,38);
                        (10,33);(11,34);(12,34);(13,34);(14,34);(14,33);(14,32);(13,31);(10,31);] in

    let auto = auto_etat_initiale auto etat_initiale in
    Graphics.set_window_title ("Press any key to go to next state and E to exit");
    auto_to_graph auto tailleCellule;

    (* on boucle à l'infini avec cette appel *)
    boucle_auto_state auto tailleCellule nbColumn nbLine ;
;;

(* ------------------ Fonction gérant le démarage de l'automate ------------------ *)

(* Fonction servant a initialiser toute la partie graphique du projet *)
let init_graph nbColumn nbLine tailleEcran =
    let string_graph = " " ^ string_of_int(tailleEcran)^"x"^string_of_int(tailleEcran) in 
    open_graph string_graph ;
    Graphics.moveto 450 1300;
    Graphics.set_font "-*-fixed-medium-r-semicondensed--100-*-*-*-*-*-iso8859-1";
    Graphics.draw_string ("GAME OF LIFE");

    Graphics.moveto 150 1100;
    Graphics.draw_string ("press A for glider gun !");

    Graphics.moveto 150 900;
    Graphics.draw_string ("press B for a normal grid !");
;;

let start_graph =
    let nbColumn = 51 in
    let nbLine = 51 in
    let tailleEcran = 1500 in
    init_graph nbColumn nbLine tailleEcran;
    

    let choice = Graphics.read_key () in 
    begin match choice with
    | 'a' -> start_auto_glider_gun nbColumn nbLine tailleEcran
    | 'b' -> start_auto_basique nbColumn nbLine tailleEcran
    | _ -> start_auto_basique nbColumn nbLine tailleEcran
    end
;;

(* ------------------ start-up du projet ------------------ *)
start_graph ;;