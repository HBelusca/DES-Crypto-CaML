#open "graphics";;

type coords;;
type Button;;
type ProgressBar;;
type TextBox;;
type Control;;
type ControlList;;

value OpenGraphicWindow : string -> unit
	  and CloseGraphicWindow : unit -> unit
	  and ClearGraphicWindow : unit -> unit
	  and GetGraphicWindowSize : unit -> coords
	  and SetGraphicWindowSize : coords -> unit
	  and Plot : coords -> color -> unit
	  and MoveTo : coords -> unit
	  and CurrentPos : unit -> coords
	  and LineTo : coords -> color -> unit
	  and Arc : coords -> int -> int -> int -> int -> color -> bool -> unit
	  and Ellipse : coords -> int -> int -> color -> bool -> unit
	  and Circle : coords -> int -> color -> bool -> unit
	  and Rectangle : coords -> int -> int -> color -> bool -> unit
	  and PrintChar : coords -> string -> int -> color -> char -> int
	  and PrintString : coords -> string -> int -> color -> string -> int;;


value NewProgressBar : coords -> int -> int -> color -> int -> bool -> ProgressBar ref
	  and SetProgressBarValue : ProgressBar -> int -> unit
	  and IncrProgressBar : ProgressBar -> unit
	  and DecrProgressBar : ProgressBar -> unit
	  and RefreshProgressBar : ProgressBar -> unit
	  and NewButton : coords -> int -> int -> color -> color -> string -> string -> bool -> Button ref
	  and NewTextBox : coords -> int -> int -> color -> string -> bool -> TextBox ref
	  and GetStrFromTextBox : TextBox -> string;;

value isEmpty : ControlList -> bool
	  and isFull : ControlList -> bool
	  and add : coords*coords*(Control ref) -> ControlList -> bool
	  and delete : int -> ControlList -> bool
	  and focusOn : Control -> unit
	  and focusOff : Control -> unit
	  and capture : Control -> unit;;

value gui : unit -> int;;