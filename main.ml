(*
module AutomateCellulaire = 
    struct
        type etat = int ;;


        type cellule =  {etat : int; x: int; y: int} ;;

        let regle_locale = regle_locale
    end
;;
 champ mutable  *)


type state = Alive | Dead;;

type cellule = Cellule of state * int * int

type voisinage = Voisinage of (int * int) list;;

type automate = Automate of voisinage * cellule list ;;



let voisin = Voisinage [(-1,1); (0, 1); (1, 1)];;

let create_auto n m = 
    let cel_list = [];
    for i =1 to n do
        for j = 1 to m do
            print_string("etst ");
        done
    done
;;

create_auto 2 2;;


