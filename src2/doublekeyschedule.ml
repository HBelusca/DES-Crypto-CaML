(*double key schedule*)

include "key schedule.ml";;

(* récupération des 2 clés *)
let recup key =
if vect_length key <> 112 
	then failwith "erreur de taille"
	else let m = make_matrix 2 56 0 in
		for i=0 to 55 do
			m.(0).(i) <- key.(i);
			m.(1).(i) <- key.(i+56);
		done;
m;;

(* key schedule *)
let doubleks key =
let m = recup key in
let u = make_vect 2 [||] in
u.(0) <- KeySchedule m.(0);
u.(1) <- KeySchedule m.(1);
u;;

(* recup [|1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1;1;0;0;1;1;0;1|];;  *)
