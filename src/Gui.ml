(* ================================================ Fonctions Graphiques ================================================ *)
#open "graphics";;

(* ================================================ *)
(* Initialisation et taille *)
let OpenGraphicWindow(parameters : string) =
	open_graph parameters;
;; (* "highest" *)

let CloseGraphicWindow() =
	close_graph();
;;

let ClearGraphicWindow() =
	clear_graph();
;;

type coords = {mutable x : int;
				mutable y : int};;

let GetGraphicWindowSize() = {x = size_x() ; y = size_y()};;
let SetGraphicWindowSize(coords : coords) =
	let stringParameters = (string_of_int(coords.x)^" x "^string_of_int(coords.y)) in
	print_endline("Redimensionnement de la fenêtre graphique à : " ^ stringParameters);
	open_graph stringParameters;
;;


(* ================================================ *)
(* Dessin *)
let Plot (coords : coords) (rgbColor : color) =
	set_color rgbColor;
	plot coords.x coords.y;
;;

let MoveTo(coords : coords) =
	moveto coords.x coords.y;
;;

let CurrentPos() =
	let (xCoord,yCoord)=current_point() in
	{x = xCoord ; y = yCoord};
;;

let LineTo (coords : coords) (rgbColor : color) =
	lineto coords.x coords.y;
;;

let Arc (coords : coords) (horizRad : int) (vertRad : int) (startAngle : int) (endAngle : int) (rgbColor : color) (isFilled : bool) =
	set_color rgbColor;
	draw_arc coords.x coords.y horizRad vertRad startAngle endAngle;
	if (isFilled) then fill_arc coords.x coords.y horizRad vertRad startAngle endAngle;
;;

let Ellipse (coords : coords) (horizRad : int) (vertRad : int) (rgbColor : color) (isFilled : bool) =
	set_color rgbColor;
	draw_ellipse coords.x coords.y horizRad vertRad;
	if (isFilled) then fill_ellipse coords.x coords.y horizRad vertRad;
;;

let Circle (coords : coords) (radius : int) (rgbColor : color) (isFilled : bool) =
	set_color rgbColor;
	draw_circle coords.x coords.y radius;
	if (isFilled) then fill_circle coords.x coords.y radius;
;;

let Rectangle (coords : coords) (width : int) (heigth : int) (rgbColor : color) (isFilled : bool) =
	if ((width <> 0) && (heigth <> 0)) then begin
		set_color rgbColor;
		MoveTo coords;
		LineTo {x = coords.x; y = coords.y+heigth} rgbColor;
		LineTo {x = coords.x+width; y = coords.y+heigth} rgbColor;
		LineTo {x = coords.x+width;y = coords.y} rgbColor;
		LineTo {x = coords.x; y = coords.y} rgbColor;
		if (isFilled) then fill_rect coords.x coords.y width heigth;
	end;
;;

(* ================================================ *)
(* Affichage de texte *)
let PrintChar (coords : coords) (rgbColor : color) (char : char) =
	MoveTo coords;
	set_color rgbColor;
	draw_char char;
	fst(text_size (string_of_char(char)));
;;

let PrintString (coords : coords) (rgbColor : color) (string : string) =
	MoveTo coords;
	set_color rgbColor;
	draw_string string;
	fst(text_size string);
;;

(* ================================================ *)
(* Remplissage *)


(* ========================================================================================================================================================================== *)
(* ========================================================================================================================================================================== *)
(* API *)
let ClipBoard = ref "";;

type ProgressBar = {posPB : coords;
					widthPB : int;
					heigthPB : int;
					rgbColorPB : color;
					mutable valeurPB : int;
					mutable isVisiblePB : bool;
					idPB : int};;
type Button = {posBT : coords;
				widthBT : int;
				heigthBT : int;
				rgbColorFocusOffBT : color;
				rgbColorFocusOnBT : color;
				textFocusOffBT : string;
				textFocusOnBT : string;
				mutable isVisibleBT : bool;
				mutable CommandBT : (unit->unit);
				idBT : int};;
type TextBox = {posTB : coords;
				widthTB : int;
				heigthTB : int;
				rgbColorTB : color;
				mutable textTB : string;
				mutable isVisibleTB : bool;
				idTB : int};;
				
type Control = NULL | ButtonControl of Button | ProgressBarControl of ProgressBar | TextBoxControl of TextBox;;

type Wnd_Message = WM_NULLMSG | WM_KEYPRESSED | WM_CAPTURE;;

type ControlList = {mutable list : (coords*coords*(Control ref)) vect ; mutable max : int};;
let CreateCtrlList(size : int) = {list = (make_vect size ({x = 0;y = 0},{x = 0;y = 0},(ref NULL))) ; max = 0};;
let isEmpty(list : ControlList) = (list.max = 0);;
let isFull(list : ControlList) = (list.max = (vect_length list.list)-1);;
let add (element : coords*coords*(Control ref)) (list : ControlList) = match isFull(list) with
|true -> false;
|_ -> list.list.(list.max) <- element;
		list.max <- list.max + 1;		
		true;
;;
let delete (index : int) (list : ControlList) = match isEmpty(list) with
|true -> false;
|_ ->	for i=index to list.max do
			list.list.(i) <- list.list.(i+1);
		done;
		list.max <- list.max-1;
		true;
;;

let ControlList = CreateCtrlList(20);;

(* ========================================================================================================================================================================== *)
(* ========================================================================================================================================================================== *)
(* Contrôles utilisateur *)

(* ============================= *)
(* Barre d'avancement : ProgressBar *)
(* ============================= *)
let RefreshProgressBar(handle : ProgressBar) =
	if (handle.isVisiblePB) then begin
		Rectangle handle.posPB handle.widthPB handle.heigthPB background true;
		Rectangle handle.posPB handle.widthPB handle.heigthPB 0x000000 false;
		
		let xCoord = handle.posPB.x and yCoord = handle.posPB.y and width = (min handle.valeurPB 100)*(handle.widthPB-2)/100 and heigth = (handle.heigthPB-2) in
		Rectangle {x = handle.posPB.x+1; y = handle.posPB.y+1} width heigth (handle.rgbColorPB) true;
		PrintString {x = xCoord+handle.widthPB+10; y = yCoord-4} 0x000000 "        ";
		PrintString {x = xCoord+handle.widthPB+10; y = yCoord-4} 0x000000 (string_of_int(handle.valeurPB)^" %");
		();
	end;
;;

let NewProgressBar (posPB : coords) (width : int) (heigth : int) (rgbColor : color) (valeur : int) (isVisible : bool) (id : int) =
	let returnObject = ref {posPB=posPB ; widthPB=width ; heigthPB=heigth ; rgbColorPB=rgbColor ; valeurPB=valeur ; isVisiblePB=isVisible ; idPB=id} in
	add (posPB, {x = posPB.x+width; y = posPB.y+heigth}, ref (ProgressBarControl(!returnObject))) ControlList;
	
	RefreshProgressBar !returnObject;
	returnObject;
;;

let SetProgressBarValue (handle:ProgressBar) (valeur:int) =
	if (valeur <> handle.valeurPB) then begin
		handle.valeurPB <- valeur;
		RefreshProgressBar handle;
	end;
;;

let IncrProgressBar(handle : ProgressBar) = SetProgressBarValue handle (handle.valeurPB+1);;
let DecrProgressBar(handle : ProgressBar) = SetProgressBarValue handle (handle.valeurPB-1);;


(* ============= *)
(* Bouton : Button *)
(* ============= *)
let RefreshButton(handle : Button) =
	if (handle.isVisibleBT) then begin
		Rectangle handle.posBT handle.widthBT handle.heigthBT background true;
		Rectangle handle.posBT handle.widthBT handle.heigthBT handle.rgbColorFocusOffBT false;
		
		let xCoord = handle.posBT.x+(handle.widthBT-fst(text_size handle.textFocusOffBT))/2 and yCoord = handle.posBT.y+(handle.heigthBT-snd(text_size handle.textFocusOffBT))/2 in
		PrintString {x = xCoord; y = yCoord} handle.rgbColorFocusOffBT handle.textFocusOffBT;
		();
	end;
;;

let NewButton (posBT : coords) (width : int) (heigth : int) (rgbColorFocusOff : color) (rgbColorFocusOn : color) (textFocusOff : string) (textFocusOn : string) (isVisible : bool) Command (id : int) =
	let returnObject = ref {posBT=posBT ; widthBT=width ; heigthBT=heigth ; rgbColorFocusOffBT=rgbColorFocusOff ; rgbColorFocusOnBT=rgbColorFocusOn ; textFocusOffBT=textFocusOff ; textFocusOnBT=textFocusOn ; isVisibleBT=isVisible ; CommandBT=Command ; idBT=id} in
	add (posBT, {x = posBT.x+width; y = posBT.y+heigth}, ref (ButtonControl(!returnObject))) ControlList;
	
	RefreshButton !returnObject;
	returnObject;
;;

let focusOnButton(handle : Button) =
	if (handle.isVisibleBT) then begin
		Rectangle handle.posBT handle.widthBT handle.heigthBT background true;
		Rectangle handle.posBT handle.widthBT handle.heigthBT handle.rgbColorFocusOnBT false;
		
		let xCoord = handle.posBT.x+(handle.widthBT-fst(text_size handle.textFocusOnBT))/2 and yCoord = handle.posBT.y+(handle.heigthBT-snd(text_size handle.textFocusOnBT))/2 in
		PrintString {x = xCoord; y = yCoord} handle.rgbColorFocusOnBT handle.textFocusOnBT;
		();
	end;
;;

let focusOffButton(handle : Button) =
	if (handle.isVisibleBT) then begin
		Rectangle handle.posBT handle.widthBT handle.heigthBT background true;
		Rectangle handle.posBT handle.widthBT handle.heigthBT handle.rgbColorFocusOffBT false;
		
		let xCoord = handle.posBT.x+(handle.widthBT-fst(text_size handle.textFocusOffBT))/2 and yCoord = handle.posBT.y+(handle.heigthBT-snd(text_size handle.textFocusOffBT))/2 in
		PrintString {x = xCoord; y = yCoord} handle.rgbColorFocusOffBT handle.textFocusOffBT;
		();
	end;
;;


(* ===================== *)
(* Zone de texte : TextBox *)
(* ===================== *)
let RefreshTextBox(handle : TextBox) =
	if (handle.isVisibleTB) then begin
		Rectangle handle.posTB handle.widthTB handle.heigthTB background true;
		Rectangle handle.posTB handle.widthTB handle.heigthTB handle.rgbColorTB false;
		PrintString {x = handle.posTB.x+1; y = handle.posTB.y} 0x000000 handle.textTB;
		();
	end;
;;

let NewTextBox (posTB : coords) (width : int) (heigth : int) (rgbColor : color) (text : string) (isVisible : bool) (id : int) =
	let returnObject = ref {posTB=posTB ; widthTB=width ; heigthTB=heigth ; rgbColorTB=rgbColor ; textTB=text ; isVisibleTB=isVisible ; idTB=id} in
	add (posTB, {x = posTB.x+width; y = posTB.y+heigth}, ref (TextBoxControl(!returnObject))) ControlList;
	
	RefreshTextBox !returnObject;
	returnObject;
;;

let captureTextBox(handle : TextBox) =
	let enterKey = ref false in
	
	(* ================================================ *)
	(* ================================================ *)
	(* TODO -- A DEBUGGER !!!!!!!!!! *)
	(* ================================================ *)
	(* ================================================ *)
	let width = ref 0 and enterKey = ref false and i = ref 0 in
	width := PrintString {x = handle.posTB.x+1; y = handle.posTB.y} 0x000000 handle.textTB;
	while (not !enterKey) do
		let status = wait_next_event [Key_pressed] in
		begin match int_of_char(status.key) with
		|027 -> ();
		|009 -> ();
		|024 -> (* Couper *)
				ClipBoard := handle.textTB;
				handle.textTB <- "";
				RefreshTextBox handle;
		|003 -> (* Copier *)
				ClipBoard := handle.textTB;
				RefreshTextBox handle;
		|022 -> (* Coller *)
				handle.textTB <- handle.textTB ^ !ClipBoard;
				RefreshTextBox handle;
		|008 -> if ((string_length handle.textTB) > 0) then begin
					handle.textTB <- sub_string handle.textTB 0 ((string_length handle.textTB)-1);
					if (fst(text_size handle.textTB)+fst(text_size "x") > handle.widthTB) then begin
						i := !i-1;
						width := PrintString {x = handle.posTB.x+1; y = handle.posTB.y} 0x000000 (sub_string handle.textTB (!i) ((string_length handle.textTB)-(!i)));
					end
					else
						width := !width - PrintString {x = handle.posTB.x+1+(!width)-fst(text_size " "); y = handle.posTB.y} 0x000000 " ";
				end;
		|010 -> enterKey := true;
		|013 -> enterKey := true;
		|_   -> handle.textTB <- handle.textTB ^ string_of_char(status.key);
				if (fst(text_size handle.textTB) > handle.widthTB) then
					i := !i+1;
				width := PrintString {x = (handle.posTB.x)+1; y = handle.posTB.y} 0x000000 (sub_string handle.textTB (!i) ((string_length handle.textTB)-(!i)));
		end;
	done;
;;

let GetStrFromTextBox(handle : TextBox) = handle.textTB;;

let focusOnTextBox(handle : TextBox) =
	if (handle.isVisibleTB) then
		Rectangle {x = handle.posTB.x-1; y = handle.posTB.y-1} (handle.widthTB+2) (handle.heigthTB+2) 0x008000 false;
;;

let focusOffTextBox(handle : TextBox) =
	if (handle.isVisibleTB) then
		Rectangle {x = handle.posTB.x-1; y = handle.posTB.y-1} (handle.widthTB+2) (handle.heigthTB+2) background false;
;;


(* ========================================================================================================================================================================== *)
(* ========================================================================================================================================================================== *)
(* KERNEL *)
let focusOn(handle : Control) = match handle with
|ButtonControl bt -> focusOnButton bt;
|TextBoxControl tb -> focusOnTextBox tb;
|_ -> ();;

let focusOff(handle : Control) = match handle with
|ButtonControl bt -> focusOffButton bt;
|TextBoxControl tb -> focusOffTextBox tb;
|_ -> ();;

let capture(handle : Control) =
	focusOn handle;
	
	begin match handle with
	|TextBoxControl tb -> captureTextBox tb;
	|ButtonControl bt -> bt.CommandBT ();
	|_ -> ();
	end;
	
	focusOff handle;
;;

let gui() =
	let mouseMove = ref true and returnValue = ref (WM_NULLMSG, 0) and identificator = ref (-1) in
	while (!mouseMove) do
		let status = ref (wait_next_event [Button_down;Button_up;Key_pressed;Mouse_motion]) in
		if (not(!status.button) && not(!status.keypressed)) then begin
			mouseMove := true;
			for i=0 to ControlList.max do
				let (CIG, CSD, control) = ControlList.list.(i) in
				if (!control <> NULL) then begin
					let isVisible = ref false in
					begin match !control with
					|ButtonControl bt -> isVisible := bt.isVisibleBT;
					|ProgressBarControl pb -> isVisible := pb.isVisiblePB;
					|TextBoxControl tb -> isVisible := tb.isVisibleTB;
					|_ -> ();
					end;
					if (!isVisible && (CIG.x <= !status.mouse_x) && (!status.mouse_x <= CSD.x) &&
						(CIG.y <= !status.mouse_y) && (!status.mouse_y <= CSD.y)) then
						focusOn(!control)
					else
						focusOff(!control);
				end;
			done;
		end
		else if (!status.button) then begin
			status := wait_next_event [Button_down;Button_up;Key_pressed;Mouse_motion];
			if (not(!status.button)) then begin
				mouseMove := false;
				for i=0 to ControlList.max do
					let (CIG, CSD, control) = ControlList.list.(i) in
					if (!control <> NULL) then begin
						let isVisible = ref false in
						begin match !control with
						|ButtonControl bt -> isVisible := bt.isVisibleBT; identificator := bt.idBT;
						|ProgressBarControl pb -> isVisible := pb.isVisiblePB; identificator := pb.idPB;
						|TextBoxControl tb -> isVisible := tb.isVisibleTB; identificator := tb.idTB;
						|_ -> ();
						end;
						if (!isVisible && (CIG.x <= !status.mouse_x) && (!status.mouse_x <= CSD.x) &&
							(CIG.y <= !status.mouse_y) && (!status.mouse_y <= CSD.y)) then begin
							focusOn(!control);
							capture(!control);
							mouseMove := false;
							returnValue := (WM_CAPTURE, !identificator);
						end
						else
							focusOff(!control);
					end;
				done;
			end;
		end
		else if (!status.keypressed) then begin
			mouseMove := false;
			(*returnValue:= abs(int_of_char(!status.key)-0x30);*)
			returnValue := (WM_KEYPRESSED, abs(int_of_char(!status.key)-0x30));
		end;
	done;
	!returnValue;
;;


let RefreshControls() =
	for i=0 to ControlList.max do
		let (CIG, CSD, control) = ControlList.list.(i) in
		if (!control <> NULL) then
			begin match !control with
			|ButtonControl bt -> if (bt.isVisibleBT) then RefreshButton bt;
			|ProgressBarControl pb -> if (pb.isVisiblePB) then RefreshProgressBar pb;
			|TextBoxControl tb -> if (tb.isVisibleTB) then RefreshTextBox tb;
			|_ -> ();
			end;
	done;
;;

let SetVisibility (handle : Control) (isVisible : bool) = match handle with
|ButtonControl bt -> bt.isVisibleBT <- isVisible;
|ProgressBarControl pb -> pb.isVisiblePB <- isVisible; RefreshProgressBar pb;
|TextBoxControl tb -> tb.isVisibleTB <- isVisible; RefreshTextBox tb;
|_ -> ();
;;
