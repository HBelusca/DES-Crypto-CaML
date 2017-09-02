#open "num";;
let modulo (a : int) (b : int) = int_of_num(mod_num (num_of_int(a)) (num_of_int(b)));;

(* TOUTES CES PROCÉDURES FONCTIONNENT *)

(* ========================= Déclaration des procédures utilisées par la fonction Key. ========================= *)

(* Extension de la clé, de 56 bits à 64 bits (avec bit de parité). *)
let KeyExtendParity(key : int vect) = match (vect_length key) with
|56 -> let ExtendKey = make_vect 64 0 in (* 8*7 ; 8*8 *)
		for i=0 to 7 do
			ExtendKey.(8*i) <- key.(7*i);
			ExtendKey.(8*i+1) <- key.(7*i+1);
			ExtendKey.(8*i+2) <- key.(7*i+2);
			ExtendKey.(8*i+3) <- key.(7*i+3);
			ExtendKey.(8*i+4) <- key.(7*i+4);
			ExtendKey.(8*i+5) <- key.(7*i+5);
			ExtendKey.(8*i+6) <- key.(7*i+6);
			ExtendKey.(8*i+7) <- (key.(7*i)+key.(7*i+1)+key.(7*i+2)+key.(7*i+3)+key.(7*i+4)+key.(7*i+5)+key.(7*i+6)) mod 2;
		done;
		ExtendKey;
		
|_ -> failwith "<KeyExtendParity> : La clé doit mesurer 56 bits.";;


(* permutation computed 1 *)
let PC1(l : int vect) =
	let c = make_vect 28 0 and d = make_vect 28 0 and m = make_matrix 2 28 0 in
	(* Left *)
	c.(0) <- l.(56);
	c.(1) <- l.(48);
	c.(2) <- l.(40);
	c.(3) <- l.(32);
	c.(4) <- l.(24);
	c.(5) <- l.(16);
	c.(6) <- l.(8);
	
	c.(7) <- l.(0);
	c.(8) <- l.(57);
	c.(9) <- l.(49);
	c.(10) <- l.(41);
	c.(11) <- l.(33);
	c.(12) <- l.(25);
	c.(13) <- l.(17);
	
	c.(14) <- l.(9);
	c.(15) <- l.(1);
	c.(16) <- l.(58);
	c.(17) <- l.(50);
	c.(18) <- l.(42);
	c.(19) <- l.(34);
	c.(20) <- l.(26);
	
	c.(21) <- l.(18);
	c.(22) <- l.(10);
	c.(23) <- l.(2);
	c.(24) <- l.(59);
	c.(25) <- l.(51);
	c.(26) <- l.(43);
	c.(27) <- l.(35);
	
	(* Right *)
	d.(0) <- l.(62);
	d.(1) <- l.(54);
	d.(2) <- l.(46);
	d.(3) <- l.(38);
	d.(4) <- l.(30);
	d.(5) <- l.(22);
	d.(6) <- l.(14);
	
	d.(7) <- l.(6);
	d.(8) <- l.(61);
	d.(9) <- l.(53);
	d.(10) <- l.(45);
	d.(11) <- l.(37);
	d.(12) <- l.(29);
	d.(13) <- l.(21);
	
	d.(14) <- l.(13);
	d.(15) <- l.(5);
	d.(16) <- l.(60);
	d.(17) <- l.(52);
	d.(18) <- l.(44);
	d.(19) <- l.(36);
	d.(20) <- l.(28);
	
	d.(21) <- l.(20);
	d.(22) <- l.(12);
	d.(23) <- l.(4);
	d.(24) <- l.(27);
	d.(25) <- l.(19);
	d.(26) <- l.(11);
	d.(27) <- l.(3);

	m.(0) <- c;
	m.(1) <- d;
m;;

(* permutation computed 2 *)
let PC2(m : int vect vect) =
	let a = make_vect 56 0 and b = make_vect 48 0 in
	for i=0 to 27 do
		a.(i) <- (m.(0)).(i)
	done;
	for i=28 to 55 do
		a.(i) <- (m.(1)).(i-28)
	done;

	b.(0) <- a.(13);
	b.(1) <- a.(16);
	b.(2) <- a.(10);
	b.(3) <- a.(23);
	b.(4) <- a.(0);
	b.(5) <- a.(4);
	
	b.(6) <- a.(2);
	b.(7) <- a.(27);
	b.(8) <- a.(14);
	b.(9) <- a.(5);
	b.(10) <- a.(20);
	b.(11) <- a.(9);
	
	b.(12) <- a.(22);
	b.(13) <- a.(18);
	b.(14) <- a.(11);
	b.(15) <- a.(3);
	b.(16) <- a.(25);
	b.(17) <- a.(7);
	
	b.(18) <- a.(15);
	b.(19) <- a.(6);
	b.(20) <- a.(26);
	b.(21) <- a.(19);
	b.(22) <- a.(12);
	b.(23) <- a.(1);
	
	b.(24) <- a.(40);
	b.(25) <- a.(51);
	b.(26) <- a.(30);
	b.(27) <- a.(36);
	b.(28) <- a.(46);
	b.(29) <- a.(54);
	
	b.(30) <- a.(29);
	b.(31) <- a.(39);
	b.(32) <- a.(50);
	b.(33) <- a.(44);
	b.(34) <- a.(32);
	b.(35) <- a.(47);
	
	b.(36) <- a.(43);
	b.(37) <- a.(48);
	b.(38) <- a.(38);
	b.(39) <- a.(55);
	b.(40) <- a.(33);
	b.(41) <- a.(52);
	
	b.(42) <- a.(45);
	b.(43) <- a.(41);
	b.(44) <- a.(49);
	b.(45) <- a.(35);
	b.(46) <- a.(28);
	b.(47) <- a.(31);

b;;

(* let lshift (vector : int vect vect) (decalage : int) = match decalage with
|0 -> ();
|_ -> let t1=ref 0 and
	t2=ref 0 and t3=ref 0 and
	t4=ref 0 and t5=ref 0 in
	let nbPermut=ref 0 and taille_vecteur=(min (vect_length vector.(0)) (vect_length vector.(1))) in
	let index=ref (modulo decalage taille_vecteur) in

	while (!nbPermut < taille_vecteur) do
		t1 := !index;
		t2 := vector.(0).(!t1);
		t4 := vector.(1).(!t1);
		index := modulo (!index - decalage) taille_vecteur;

		while (!index <> !t1) do
			t3 := vector.(0).(!index);
			t5 := vector.(1).(!index);
			vector.(0).(!index) <- !t2;
			vector.(1).(!index) <- !t4;
			t2 := !t3;
			t4 := !t5;
			nbPermut := !nbPermut+1;
			index := modulo (!index - decalage) taille_vecteur;
		done;

		vector.(0).(!index) <- !t2;
		vector.(1).(!index) <- !t4;
		nbPermut := !nbPermut+1;
		index := !index+1;
	done;
;; *)

let lshift (m : int vect vect) (decalage : int) = match decalage with
|1 -> let aux1 = ref m.(0).(0) and aux2 = ref m.(1).(0) in
		for i =0 to 26 do
			m.(0).(i) <- m.(0).(i+1);
			m.(1).(i) <- m.(1).(i+1);
		done;
		m.(0).(27) <- !aux1;
		m.(1).(27) <- !aux2;
|2 -> let aux1 = ref m.(0).(0) and aux2 = ref m.(1).(0) and aux3 = ref m.(0).(1) and aux4 = ref m.(1).(1) in
		for i =0 to 25 do
			m.(0).(i) <- m.(0).(i+2);
			m.(1).(i) <- m.(1).(i+2);
		done;
		m.(0).(26) <- !aux1;
		m.(1).(26) <- !aux2;
		m.(0).(27) <- !aux3;
		m.(1).(27) <- !aux4;
|_ -> failwith "<lshift> : erreur, il faut un décalage de 1 ou 2 !!";;


(* ========================= FONCTION KeySchedule ========================= *)
let KeySchedule(k : int vect) =
	let r = make_matrix 16 48 0 and PermComp1 = ref (PC1 k) and decalage = ref 0 in
	
	for i=0 to 15 do
		if ((i=0) || (i=1) || (i=8) || (i=15)) then
			decalage := 1
		else
			decalage := 2;
		
		lshift !PermComp1 !decalage;
		r.(i) <- (PC2 !PermComp1);
	done;
	
	(*print_newline();
	print_newline();
	print_newline();
	print_newline();
	for i=0 to 15 do
		for j=0 to 47 do
			if (j mod 6 = 0) then print_char ` `;
			print_int r.(i).(j);
		done;
		print_newline();
	done;
	print_newline();
	print_newline();
	print_newline();
	print_newline();*)
	
r;;
