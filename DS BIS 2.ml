type arete = {a:int; b:int};;
type graphe = {n:int; l : arete list};;

let gex1 = {n=5; l = [{a=0; b=4}; {a=4; b=3}; {a=0; b=3}; {a=1; b=3}; {a=0; b=3}; {a=3; b=0}]};;

let rec insere l s = match l with
	|[]-> [s]
	|t::q when t< s-> t::(insere q s)
	|t::q -> (if t = s then [] else [s])@l;;


let voisins g s =
	let l = g.l in
	let rec aux l v = match l with
		|[] -> v
		|t::q when t.a = s -> aux q (insere v (t.b))
		|t::q when t.b = s -> aux q (insere v (t.a))
		|t::q -> aux q v
	in let v = aux l [] in v;;


let rec appartient x l = match l with
	|[] -> false
	|t::q -> (t = x)||(appartient x q);;

voisins gex1 3;;
	
appartient 3 (voisins gex1 3);;

let coloration graph =
	let c = Array.make (graph.n) 0 in
	let couleur s =
		let vois = voisins graph s in
		let rec aux l k = match l with
			| [] -> k
			| t::q when c.(t) = k -> aux l (k+1)
			| t::q -> aux q k
		in aux vois 1
	in
	for s = 0 to graph.n - 1 do 
		c.(s) <- couleur s
		done; c;;

coloration gex1;;


