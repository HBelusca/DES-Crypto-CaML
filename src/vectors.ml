(* Procédure de vecteurs *)
let ZeroVector(vector : int vect) =
	for i=0 to (vect_length vector)-1 do
		vector.(i) <- 0;
	done;
;;

let CopyVector (dest : int vect) (source : int vect) =
	for i=0 to (vect_length dest)-1 do
		dest.(i) <- 0;
	done;
	for i=0 to (min (vect_length dest) (vect_length source))-1 do
		dest.(i) <- source.(i);
	done;
;;
