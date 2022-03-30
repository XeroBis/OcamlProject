open Graphics;; (* pour l'affichage graphique *)

(* ------------------ Les types de bases pour notre automate ------------------ *)
type state = Alive | Dead;;

type cellule = Cellule of state * int * int ;;

type voisinage = Voisinage of (int * int) list;;

type automate = Automate of voisinage * ((cellule list ) list);;


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
        [create_column_auto n m]
    else
        (create_column_auto n m)::create_automate n (m-1)
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

let rec print_auto_list_list (list_list_cell:(cellule list)list)  = 
    match list_list_cell with
    | h::l -> print_auto_list h; print_auto_list_list l;
    | []  -> print_string("")
;;

let print_auto auto =
    match auto with
    | Automate(_,list) -> print_auto_list_list list;
;;

(* ------------------ Fonctions permettant l'affichage graphique de l'automate ------------------ *)
let change_color state = 
    match state with
    | Dead -> Graphics.set_color red
    | Alive -> Graphics.set_color black
;;

let cellule_to_graph cell size = 
    match cell with
    | Cellule(state, x, y) -> change_color state; Graphics.fill_rect (x*100) (y*100) size size;
;;

let rec auto_to_graph_l list size =
    match list with
    | h::l -> cellule_to_graph h size; auto_to_graph_l l size;
    | [] -> print_string("");
;;

let rec auto_to_graph_l_l list size =
    match list with 
    | h::l -> auto_to_graph_l h size; auto_to_graph_l_l l size;
    | [] -> print_string("");
;;

let auto_to_graph auto sizeCell =
    match auto with
    | Automate(_, list)-> auto_to_graph_l_l list sizeCell
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

let rec auto_change_state_l_l l x y = 
    match l with
    | h::t -> (auto_change_state_l h x y)::auto_change_state_l_l t x y
    | [] -> []
;;

let auto_change_state auto x y = 
    match auto with
    | Automate(v, cell_l_l) -> Automate(v, (auto_change_state_l_l cell_l_l x y))
;;

(* ------------------ Fonctions permettant la prise en compte d'un état initial de cellule vivante ------------------ *)

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
    | h::t -> if is_there h x y then Some (get_state h) else get_state_l t x y 
    | [] -> None
;;

let rec get_state_l_l l x y =
    match l with
    | h::t -> begin match get_state_l h x y with
                | None -> get_state_l_l t x y
                | Some x -> x
                end
    | _ -> failwith "erreur x et y pas dans le tableau"
;;

let get_state auto x y =
    match auto with
    | Automate(v, l) -> get_state_l_l l x y 
;;

(* ------------------ Fonctions permettant de passer d'un état à un autre ------------------ *)

let get_x cell = 
    match cell with
    | Cellule (_ ,x, _)-> x 
;;
let get_y cell =
    match cell with
    | Cellule (_,_,y) -> y
;;

let rec is_voisin cell voisin x y =
    match voisin with
    | (a, b)::t -> if get_x cell + a = x && get_y cell + b = y then 1 + is_voisin cell t x y else is_voisin cell t x y
    | _ -> 0
;;

let rec get_nb_voisin_alive_l l voisin x y =
    match l with 
    | h::t -> is_voisin h voisin x y + get_nb_voisin_alive_l t voisin x y
    | [] -> 0 
;;

let rec get_nb_voisin_alive_l_l l voisin x y =
    match l with
    | h::t -> get_nb_voisin_alive_l h voisin x y
    | [] -> 0
;;

let get_liste_voisin voisin = 
    match voisin with
    | Voisinage(l) -> l
;;

let rec get_nb_voisin_alive auto x y =
    match auto with
    | Automate(v, l) -> get_nb_voisin_alive_l_l l (get_liste_voisin v) x y 
;;


let next_state auto l voisin x y =
    let state = get_state auto x y in
    let nbVoisinAlive = get_nb_voisin_alive auto x y in
    match state with 
    | Dead -> if nbVoisinAlive = 3 then auto_change_state auto x y else auto
    | Alive -> if nbVoisinAlive <> 2 && nbVoisinAlive <> 3 then auto_change_state auto x y else auto 
;;

let next_state auto n m x y  =
    match auto with
    | Automate(voisin, l) -> next_state auto l voisin x y
;;


(* ------------------ fonction gérant l'automate ainsi que le start-up de la partie graphique ------------------*)

let nbColumn = 10;;
let nbLine = 10;;

let voisin = Voisinage [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1, 0); (1, 1)];;

let auto = Automate(voisin , create_automate nbColumn nbLine);;

let tailleEcran = 1000;;

let string_graph = " " ^ string_of_int(tailleEcran)^"x"^string_of_int(tailleEcran);; 

open_graph string_graph ;;



let etat_initiale = [(1,1); (2,2);(1,2);];;

let tailleCellulle = tailleEcran/nbColumn;;

auto_to_graph auto tailleCellulle;;

clear_graph;;

let auto = auto_etat_initiale auto etat_initiale;;

let test = if get_state auto 2 2 = Alive then "oui" else "non";;

print_string(test);;

auto_to_graph auto tailleCellulle;;

ignore (Graphics.read_key ())