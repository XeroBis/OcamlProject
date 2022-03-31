open Graphics;; (* pour l'affichage graphique *)

(* ------------------ Les types de bases pour notre automate ------------------ *)
type state = Alive | Dead;;

type cellule = Cellule of state * int * int ;;

type voisinage = Voisinage of (int * int) list;;

type automate = Automate of voisinage * (cellule list );;


(* ------------------ Fonctions permettant la création d'un automate avec toutes ces cellules mortes ------------------ *)
let rec create_column_auto i m = 
    if i = 0
    then
        [Cellule (Dead, i, m)]
    else 
        Cellule (Dead, i, m)::create_column_auto (i-1) m
;;

let rec create_automate n m =
    if m = 0
    then
        create_column_auto n m
    else
        (create_column_auto n m)@create_automate n (m-1)
;;

(* ------------------ Fonctions qui permettent l'affichage de l'automate dans le terminal ------------------ *)

let rec print_auto_list (list_cell: cellule list) =
    match list_cell with
    | [] -> print_string("")
    | Cellule(state, x, y)::l -> print_auto_list l;
                                    begin match state with
                                    | Dead -> print_string("D, x=" ^ string_of_int(x) ^ ", y=" ^ string_of_int(y)) ; print_newline();
                                    | Alive -> print_string("A"); 
                                    end                              
;;

let print_auto auto =
    match auto with
    | Automate(_,list) -> print_auto_list list;
;;

(* ------------------ Fonctions permettant l'affichage graphique de l'automate ------------------ *)
let change_color state = 
    match state with
    | Dead -> Graphics.set_color red
    | Alive -> Graphics.set_color black
;;

let cellule_to_graph cell size = 
    match cell with
    | Cellule(state, x, y) -> change_color state; Graphics.fill_rect (x*size) (y*size) size size;
;;

let rec auto_to_graph_l list size =
    match list with
    | h::l -> cellule_to_graph h size; auto_to_graph_l l size;
    | [] -> print_string("");
;;

let auto_to_graph auto sizeCell =
    match auto with
    | Automate(_, list)-> auto_to_graph_l list sizeCell
;;

(* ------------------ Fonctions permettant le changement d'état d'une cellule ------------------ *)

let get_opposite_state state =
    match state with
    | Dead -> Alive
    | Alive -> Dead
;;

let sameCoord cell x y = 
    match cell with
    | Cellule(state, a, b) -> if a = x && b = y then Cellule(get_opposite_state state, a, b) else cell 
;;

let rec auto_change_state_l l x y =
    match l with
    | h::t -> sameCoord h x y::auto_change_state_l t x y
    | [] -> []
;;

let auto_change_state auto x y = 
    match auto with
    | Automate(v, cell_l_l) -> Automate(v, (auto_change_state_l cell_l_l x y))
;;

(* ------------------ Fonctions permettant le changement d'état de certaines cellules, selon une liste de coordonées (sers pour l'état initial et le passage d'un état au suivant ) ------------------ *)

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

let is_there cell x y =
    match cell with
    | Cellule(state, a, b) -> if a = x && b=y then true else false
;;

let get_state cell =
    match cell with
    | Cellule (state, _,_) -> state
;;

let rec get_state_l l x y =
    match l with
    | h::t -> if is_there h x y then get_state h else get_state_l t x y 
    | _ -> failwith "erreur x et y pas dans le tableau"
;;

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

let isAlive cell = 
    match cell with
    | Cellule(state, _,_ )-> match state with
                            | Dead -> false
                            | Alive -> true
;;

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

let rec get_nb_voisin_alive_l l l_voisin x y n m=
    match l with 
    | h::t -> is_voisin h l_voisin x y n m + get_nb_voisin_alive_l t l_voisin x y n m
    | [] -> 0 
;;

let get_liste_voisin voisin = 
    match voisin with
    | Voisinage(l) -> l
;;

let rec get_nb_voisin_alive auto x y n m=
    match auto with
    | Automate(v, l) -> get_nb_voisin_alive_l l (get_liste_voisin v) x y n m
;;


(* ------------------ Fonctions permettant de passer d'un état d'automate au suivant ------------------ *)

let next_state_in auto x y n m =
    let state = get_state auto x y in
    let nbVoisinAlive = get_nb_voisin_alive auto x y n m in
    print_string("nbVoisinAlive : "^string_of_int(nbVoisinAlive) ^ ", x ="^ string_of_int(x)^", y= "^string_of_int(y));
    print_newline();
    match state with 
    | Dead -> if nbVoisinAlive = 3 then [(x,y)] else []
    | Alive -> if nbVoisinAlive = 2 || nbVoisinAlive = 3 then [] else [(x, y);]
;;

let rec next_state_rec auto l_changement n m x y  =
    if n-1=x && m-1 = y
    then
        l_changement@next_state_in auto x y n m
    else
        match auto with
        | Automate(voisin, l) -> next_state_rec auto (l_changement@(next_state_in auto x y n m)) n m (if x = n-1 then 0 else x+1) (if x=n-1 then y+1 else y)
;;

let next_state_list auto n m =
    next_state_rec auto [] n m 0 0
;;

let next_state auto n m = 
    auto_etat_initiale auto (next_state_list auto n m)
;;


(* ------------------ fonction gérant l'automate ainsi que le start-up de la partie graphique ------------------*)

let nbColumn = 10;;
let nbLine = 10;;

let voisin = Voisinage [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1, 0); (1, 1)];;

let auto = Automate(voisin , create_automate nbColumn nbLine);;

let tailleEcran = 1000;;

let string_graph = " " ^ string_of_int(tailleEcran)^"x"^string_of_int(tailleEcran);; 

open_graph string_graph ;;

let etat_initiale = [(1,1); (2,2);(1,2);(0,9);];;

let tailleCellulle = tailleEcran/nbColumn;;

auto_to_graph auto tailleCellulle;;

clear_graph;;

let auto = auto_etat_initiale auto etat_initiale;;

let test = if get_state auto 2 2 = Alive then "oui" else "non";;

print_string(test);;

let auto = next_state auto nbColumn nbLine;;

auto_to_graph auto tailleCellulle;;

ignore (Graphics.read_key ())