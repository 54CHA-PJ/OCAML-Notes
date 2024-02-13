

type 'a tas = {mutable n : int ; t : 'a array };;

let echange t i j = 
	let x = t.(i) in t.(i) <- t.(j) ; t.(j) <- x;;

let rec monte t k = match k with
	| 0 -> ()
	| k -> let p = (k-1)/2 in if t.(k) > t.(p) then (echange t k p; monte t p);;

let rec descend t n k = 
	let fg = 2*k+1 in
	match k with
	| k when fg >= n -> ()
	| k -> let j = if fg = n-1 || t.(fg) > t.(fg+1) then fg else (fg+1) in 
			if t.(k) < t.(j) then (echange t k j; descend t n j);;

(* initialisation 'a tas taille max n *)
let init n x = { n = 0 ; t = Array.make n x };;

(* cr�er un tas � partir d'un tableau de valeurs *)
let cree_tas t =
	for k = 1 to Array.length t - 1 do
		monte t k done ;
	{n = Array.length t ; t = t};;
	
let v = [| 18 ; 14  ; 2 ; 0 ; 10 ; 11 ; 3 ; 6 ; 6 ;  7|] ;;
let t = cree_tas v ;;

(* extraction  *)

let extraire tas =
	let v = tas.t.(0) in
	tas.n <- tas.n - 1 ;
	tas.t.(0) <- tas.t.(tas.n) ; 
	descend tas.t tas.n 0;
	v;;

extraire t;; t;;

(* insertion *)

let insere tas x =
	let n = tas.n in
	tas.t.(n) <- x ; 
	monte tas.t n ;
	tas.n <- n+1 ;;

insere t 18 ;; t ;;
insere t 14 ;; t ;; 

let cree_tas t =
	for k = 1 to Array.length t - 1 do
		monte t k done ;;

let tri_tas t =
	cree_tas t;
	for k = Array.length t-1 downto 1 do 
	echange t 0 k; descend t k 0;
	done;;

tri_tas v;;	 v;;
(* file de priorit� *)

type ('a, 'b) data = {priorite : 'a ; valeur : 'b };;

let cree_file n (p,v) = { n = 0 ; t = Array.make n {priorite = p ; valeur = v} };;

let rec monte_file t k = match k with
	| 0 -> ()
	| i -> let j = (i-1)/2 in
	if t.(i).priorite > t.(j).priorite then (echange t i j ; monte_file t j)
;;

let rec descend_file t n k = match k with
	| i when 2*i+1 >= n -> ()
	| i -> let j = if 2*i+2 = n || t.(2*i+1).priorite > t.(2*i+2).priorite
	then 2*i+1 else 2*i+2 in
	if t.(i).priorite < t.(j).priorite then (echange t i j ; descend_file t n j)
;;
