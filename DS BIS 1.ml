let indice a v = 
	let k = ref 0 in
		while (v.(!k) != a) do
			incr k
			done;!k;;

let echange a b v = 
	let i1,i2 = (indice a v, indice b v) in
		let t = v.(i1) in
			v.(i1)<- v.(i2);
			v.(i2)<- t ;;

let tricon v = 
	let n = Array.length v in
	for i = 0 to n-1 do
		let a = ref v.(i) in
			for k = i to n-1 do
				a := min !a v.(k);
				echange !a v.(i) v
				done; 
	done;;

let v = [|19;7;17;14;22;5;26;21;2;12|] in
tricon v;
v;;

let separation v i1 i2 =
	let p = v.(i1) in
	let i = ref 0 in
	for k = i1+1 to i2 do
		if (v.(i1+(!i)+1) < p) then
			(
			echange v.(i1+(!i)+1) p v;
			incr i
			)
		else echange v.(i1+(!i)+1) v.(i2-k+1+(!i)) v;
	done;!i;;

let v = [|19;7;17;14;22;5;26;21;2;12|] in
let i = separation v 0 9 in
i;;

let trifast v =
	let rec aux i j v = 
		if i < j then
			let a = (separation v i j) in
					aux i (a) v;
					aux (a) j v;
	in aux 0 (Array.length v - 1) v;;


let v = [|19;7;17;14;22;5;26;21;2;12|] in
trifast v;
v;;	
	



