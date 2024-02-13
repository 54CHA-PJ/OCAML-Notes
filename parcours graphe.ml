(* parcours en largeur *)

type graphe = int list array;;
open Queue;;


let parcours_largeur g s = 
	let dejavu = Array.make (Array.length g) false in
	let atraiter = create () in
	add s atraiter;
	dejavu.(s) <- true;
	let rec ajoute_voisin l = match l with
		|[] -> ()
		|t::q when dejavu.(t) -> ajoute_voisin q
		|t::q -> 
					add t atraiter;
					dejavu.(t) <- true;
					ajoute_voisin q
	in 
	try while true do
		let v = take atraiter in
		print_int v;
		print_string " ";
		ajoute_voisin g.(v);
	done with Empty -> ();;
	
let gex = [| [1] ; [2;4;6] ; [4;5;3] ; [4] ; [5] ; [] ; [2] |];;

parcours_largeur gex 1;;








(* calcule de la distance du plus court chemin *)



let distpc g s = 
	let atraiter = create () in
	let dejavu = Array.make (Array.length g) false in
	let distance = Array.make (Array.length g) (-1) in
	add s atraiter;
	dejavu.(s) <- true;
	distance.(s) <- 0;
	let rec ajoute_voisin l u = match l with
		|[] -> ()
		|t::q when dejavu.(t) -> ajoute_voisin q u
		|t::q -> 
					add t atraiter;
					dejavu.(t) <- true;
					distance.(t) <- (distance.(u)) + 1;
					ajoute_voisin q u
	in 
	for i = 0 to (Array.length g) -1 do
		let v = take atraiter in
		print_int v;
		print_string " ";
		ajoute_voisin g.(v) v
		done; distance ;;

distpc gex 0;;	
	
	
	
	
	
	
	
	
	
	
	
	
	
(* parcours en profondeur, meme chose mais avec des PILES et pas des FILES *)

open Stack;;

let parcours_profondeur g s = 
	let atraiter = create () in
	let dejavu = Array.make (Array.length g) false in
	push s atraiter;
	dejavu.(s) <- true;
	let rec aux l = match l with
		|[] -> ()
		|t::q when (dejavu.(t)) -> aux q
		|t::q -> 
					push t atraiter;
					dejavu.(t) <- true;
					aux q
	in
	try while true do
		let v = pop atraiter in
		print_int v;
		print_string " ";
		aux g.(v)
	done with Empty -> () ;;

parcours_profondeur gex 1;;



























