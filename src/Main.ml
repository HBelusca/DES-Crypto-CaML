include "des.ml";;
include "bitdebit.ml";;
include "key schedule.ml";;

(* Création des contrôles dont nous avons besoin *)
let progressbar = NewProgressBar {x=500;y=200} 100 15 0x0000FF 0 false 0
and	tbKey = NewTextBox {x=300;y=400} 450 20 0x000000 "" false 1
and	tbVector = NewTextBox {x=300;y=300} 450 20 0x000000 "" false 2
and	tbMsg = NewTextBox {x=300;y=500} 450 20 0x000000 "" false 3
and	inFN = NewTextBox {x=10;y=500} 450 20 0x000000 "" false 4
and	outFN = NewTextBox {x=550;y=500} 450 20 0x000000 "" false 5
and launchProcess = NewButton {x=400;y=100} 300 25 0x000080 0x008000 "Crypter/Décrypter" "Vous allez crypter/décrypter..." false (fun () -> ()) 6
and QuitBTN = NewButton {x=350;y=350} 300 25 0x000080 0x008000 "Quitter" "Êtes-vous sûr de vouloir quitter ?" false (fun () -> ()) 9;;

(* =================================== Point d'entrée pour crypter des messages. =================================== *)
let mainMessages() =
	ClearGraphicWindow();
	set_color 0x000000;
	MoveTo {x=300;y=520};
	draw_string "Message à crypter :";
	MoveTo {x=300;y=420};
	draw_string "Clé :";
	SetVisibility (TextBoxControl(!tbMsg)) true;
	SetVisibility (TextBoxControl(!tbKey)) true;
	
	let message = ref (WM_NULLMSG,0) in
	while (!message <> (WM_KEYPRESSED,16)) do
		message := gui();
	done;
	
	
	(* Récupération des différentes chaînes de caractères nécessaires. *)
	let messageACrypter = (GetStrFromTextBox !tbMsg) and Key = bitationKey ((GetStrFromTextBox !tbKey)) in
	(* Conversion en tableaux de bits. *)
	let messageBits = bitation messageACrypter in
	(* Calcul du nombre de blocs. *)
	let nbOfBlocks = ((vect_length (messageBits)-1)/64)+1 in
	(* Création du Vecteur du message à crypter. *)
	let messageVector = make_matrix nbOfBlocks 64 0 in
	(* Création du Vecteur du message crypté. *)
	let cipherMsg = make_vect nbOfBlocks [||] in
	
	(* =========================================== CRYPTAGE =========================================== *)
	(* Création des blocs de bits. *)
	for i=0 to (vect_length messageBits)-1 do
		messageVector.(i/64).(i mod 64) <- messageBits.(i);
	done;
	
	(* Cryptage du message... *)
	for i=0 to nbOfBlocks-1 do
		cipherMsg.(i) <- DES messageVector.(i) Key true;
	done;
	
	(* Affichage du message crypté. *)
	set_color 0x000000;
	MoveTo {x=300;y=320};
	draw_string "Message crypté :";
	let messageCrypte = ref "" in
	for i=0 to nbOfBlocks-1 do
		messageCrypte := !messageCrypte ^ debitation cipherMsg.(i);
	done;
	set_color 0x000000;
	MoveTo {x=300;y=300};
	draw_string !messageCrypte;	
	
	(* ========================================== DECRYPTAGE ========================================== *)
	(* Decryptage du message *)
	let messageBits2 = bitation !messageCrypte in
	let nbOfBlocks2 = ((vect_length (messageBits2)-1)/64)+1 in
	let messageVector2 = make_matrix nbOfBlocks2 64 0 in
	let DecipherMsg = make_vect nbOfBlocks2 [||] in
	
	(* Création des blocs de bits. *)
	for i=0 to (vect_length messageBits2)-1 do
		messageVector2.(i/64).(i mod 64) <- messageBits2.(i);
	done;
	
	(* Déryptage du message... *)
	for i=0 to nbOfBlocks2-1 do
		DecipherMsg.(i) <- DES messageVector2.(i) Key false;
	done;
	
	(* Affichage du message décrypté. *)
	set_color 0x000000;
	MoveTo {x=300;y=260};
	draw_string "Message décrypté :";
	let messageDecrypte = ref "" in
	for i=0 to nbOfBlocks2-1 do
		messageDecrypte := !messageDecrypte ^ debitation DecipherMsg.(i);
	done;
	set_color 0x000000;
	MoveTo {x=300;y=240};
	draw_string !messageDecrypte;
	
	message := (WM_NULLMSG,0);
	while (!message <> (WM_KEYPRESSED,16)) do
		message := gui();
	done;
;;

(* =================================== Point d'entrée pour crypter des fichiers. =================================== *)
let mainFiles() =
	ClearGraphicWindow();
	
	(*PrintString {x=350;y=460} 0xFF00FF "1- Crypter.";
	PrintString {x=350;y=440} 0xFF00FF "2- Décrypter.";*)
	
	set_color 0xFF00FF;
	MoveTo {x=350;y=460};
	draw_string "1- Crypter.";
	MoveTo {x=350;y=440};
	draw_string "2- Décrypter.";
	
	let choice = ref 0 in
	let message = ref (WM_NULLMSG,0) in
	while ((!message <> (WM_KEYPRESSED,1)) && (!message <> (WM_KEYPRESSED,2))) do
		message := gui();
		begin match fst(!message) with
		|WM_KEYPRESSED -> begin match snd(!message) with
							|1 -> choice := 1;
							|2 -> choice := 2;
							|_ -> ();
						end;
		|_ -> ();
		end;
	done;
	
	ClearGraphicWindow();
	set_color 0x000000;
	MoveTo {x=10;y=520};
	draw_string "Fichier source :";
	MoveTo {x=550;y=520};
	draw_string "Fichier cible :";
	MoveTo {x=300;y=420};
	draw_string "Clé :";
	SetVisibility (TextBoxControl(!inFN)) true;
	SetVisibility (TextBoxControl(!outFN)) true;
	SetVisibility (TextBoxControl(!tbKey)) true;
	
	message := (WM_NULLMSG,0);
	while (!message <> (WM_KEYPRESSED,16)) do
		message := gui();
	done;
	
	SetProgressBarValue !progressbar 0;
	SetVisibility (ProgressBarControl(!progressbar)) true;
	
	kernCryptOrDecryptFile (GetStrFromTextBox !inFN) (GetStrFromTextBox !outFN) (bitationKey (GetStrFromTextBox !tbKey)) (!choice) (fun x -> SetProgressBarValue !progressbar x;);
	
	SetVisibility (TextBoxControl(!inFN)) false;
	SetVisibility (TextBoxControl(!outFN)) false;
	SetVisibility (TextBoxControl(!tbKey)) false;
	SetVisibility (ProgressBarControl(!progressbar)) false;
	SetProgressBarValue !progressbar 0;
	ClearGraphicWindow();
;;

(* =================================== Point d'entrée pour crypter des images bitmap. =================================== *)
let mainBitmap() =
	ClearGraphicWindow();
	
	(*PrintString {x = 350; y = 460} 0xFF00FF "1- Crypter.";*)
	(*PrintString {x = 350; y = 440} 0xFF00FF "2- Décrypter.";*)
	
	set_color 0xFF00FF;
	MoveTo {x=350;y=460};
	draw_string "1- Crypter.";
	MoveTo {x=350;y=440};
	draw_string "2- Décrypter.";
	
	let choice = ref 0 in
	let message = ref (WM_NULLMSG,0) in
	while ((!message <> (WM_KEYPRESSED,1)) && (!message <> (WM_KEYPRESSED,2))) do
		message := gui();
		begin match fst(!message) with
		|WM_KEYPRESSED -> begin match snd(!message) with
							|1 -> choice := 1;
							|2 -> choice := 2;
							|_ -> ();
						end;
		|_ -> ();
		end;
	done;
	
	ClearGraphicWindow();
	set_color 0x000000;
	MoveTo {x=10;y=520};
	draw_string "Image source :";
	MoveTo {x=550;y=520};
	draw_string "Image cible :";
	MoveTo {x=300;y=420};
	draw_string "Clé :";
	MoveTo {x=300;y=320};
	draw_string "Vecteur d'initialisation :";
	SetVisibility (TextBoxControl(!inFN)) true;
	SetVisibility (TextBoxControl(!outFN)) true;
	SetVisibility (TextBoxControl(!tbKey)) true;
	SetVisibility (TextBoxControl(!tbVector)) true;
	
	message := (WM_NULLMSG,0);
	while (!message <> (WM_KEYPRESSED,16)) do
		message := gui();
	done;
		
	SetProgressBarValue !progressbar 0;
	SetVisibility (ProgressBarControl(!progressbar)) true;
	
	kernCryptOrDecryptBitmap (GetStrFromTextBox !inFN) (GetStrFromTextBox !outFN) (bitationKey (GetStrFromTextBox !tbKey)) (bitationVector (GetStrFromTextBox !tbVector)) (!choice) (fun x -> SetProgressBarValue !progressbar x;);
	
	SetVisibility (TextBoxControl(!inFN)) false;
	SetVisibility (TextBoxControl(!outFN)) false;
	SetVisibility (TextBoxControl(!tbKey)) false;
	SetVisibility (TextBoxControl(!tbVector)) false;
	SetVisibility (ProgressBarControl(!progressbar)) false;
	SetProgressBarValue !progressbar 0;
	ClearGraphicWindow();
;;



(* =================================== Point d'entrée du programme. =================================== *)
let mainMenu() =
	ClearGraphicWindow();
	
	SetVisibility (TextBoxControl(!inFN)) false;
	SetVisibility (TextBoxControl(!outFN)) false;
	SetVisibility (TextBoxControl(!tbKey)) false;
	SetVisibility (TextBoxControl(!tbMsg)) false;
	SetVisibility (TextBoxControl(!tbVector)) false;
	SetVisibility (ProgressBarControl(!progressbar)) false;
	SetProgressBarValue !progressbar 0;
	SetVisibility (ButtonControl(!QuitBTN)) true;
	
	PrintString {x=300;y=620} 0x006699 "CRYPTAGE DES par Hermès BÉLUSCA et Guillaume FRECHE";
	PrintString {x=300;y=600} 0x006699 "===================================================";
	PrintString {x=360;y=500} 0xFF00FF "Voulez-vous crypter ou décrypter :";
	PrintString {x=350;y=460} 0xFF00FF "1- Un message.";
	PrintString {x=350;y=440} 0xFF00FF "2- Un fichier.";
	PrintString {x=350;y=420} 0xFF00FF "3- Une image bitmap 24 bits non compressée.";
	
	RefreshControls();
;;

let main() =
	OpenGraphicWindow("highest");
	mainMenu();
	
	let message = ref (WM_NULLMSG,0) in
	while (!message <> (WM_CAPTURE,9)) do
		message := gui();
		begin match fst(!message) with
		|WM_KEYPRESSED -> begin match snd(!message) with
							|1 -> SetVisibility (ButtonControl(!QuitBTN)) false; mainMessages(); mainMenu();
							|2 -> SetVisibility (ButtonControl(!QuitBTN)) false; mainFiles(); mainMenu();
							|3 -> SetVisibility (ButtonControl(!QuitBTN)) false; mainBitmap(); mainMenu();
							|_ -> ();
						end;
		|_ -> ();
		end;
	done;
	CloseGraphicWindow();
;;
