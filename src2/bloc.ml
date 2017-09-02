let bloc_64 v =
	let a = vect_length v in
	let b = a / 64 in
	let m = make_matrix (b+1) 64 0 in
	for i = 0 to b do
		for j = (i*64) to ((i*64)+63) do
			if (j < a) then (m.(i)).(j mod 64) <- v.(j) else (m.(i)).(j mod 64) <- 0
		done;
	done;
m;;

let bloc_10 v =
	let a = vect_length v in
	let b = a / 10 in
	let m = make_matrix (b+1) 10 0 in
	for i = 0 to b do
		for j = (i*10) to ((i*10)+9) do
			if j < a then (m.(i)).(j mod 10) <- v.(j) else (m.(i)).(j mod 10) <- 0
		done;
	done;
m;;
