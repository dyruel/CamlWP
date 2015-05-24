(*camlart.ml*)

(**
   @author Chopin Morgan
   @since 19/02/2007
   @version 7.2.23
*)

(** Déclaration des variables *)
let grey = ref false
and color = ref false
and xsize = ref 800
and ysize = ref 600
and preview = ref 1.
and jpgname = ref "a.jpg"
and e = ref Expr.X
and f = ref (Expr.X,Expr.X,Expr.X)
and count = ref 0

exception End_of_program

let args =
[
 ("-grey",Arg.Set grey," Génération du fond d'écran en nuances de gris");
 ("-color",Arg.Set color," Génération du fond d'écran en couleur");
 ("-xsize",Arg.Set_int xsize," Largeur du fond d'écran (en pixels)");
 ("-ysize",Arg.Set_int ysize," Hauteur du fond d'écran (en pixels)");
 ("-preview",Arg.Set_float preview," Coefficient de réduction de la fenêtre de prévisualisation");
 ("-o",Arg.Set_string jpgname," Nom du fichier jpg");
]


let anon s = raise (Arg.Bad("Argument incorrect : "^s))

let suffixe () = 
  if (!count = 0) then 
    ""
  else
    (string_of_int !count)^"_"


let save_jpg f_save_ps expr (l,h) =
  let i = ref 0 in
    begin
      print_endline ("Génération du jpg en cours (cela peut prendre quelques instants)");
      print_endline ("-> Ecriture dans le fichier PostScript...");
      f_save_ps expr (l,h);
      print_endline ("-> Création de "^(suffixe ())^(!jpgname));
      i := Sys.command ("pstopnm -stdout -xborder 0 -yborder 0 -xsize "^(string_of_int l)^" -ysize "^(string_of_int h)^" -portrait tmp.ps 2> /dev/null | ppmtojpeg -quality=95 > "^((suffixe ())^(!jpgname)));
      print_endline ("-> Fait");
      count := !count + 1;
    end


let aide () =
  begin
    print_endline ("Touche 'g' pour générer une nouvelle image");
    print_endline ("Touche 's' pour sauvegarder l'image au format jpeg");
    print_endline ("Touche 'i' pour afficher l'expression");
    print_endline ("Touche 'q' pour quitter");
  end


let generer () =
  if (!grey = true) then
    e := Draw.genere_preview_en_gris !preview (!xsize,!ysize)
  else
    f := Draw.genere_preview_en_couleur !preview (!xsize,!ysize)


let sauver () =
  if (!grey = true) then
    save_jpg Draw.sauvegarde_postscript_en_gris !e (!xsize,!ysize)
  else
    save_jpg Draw.sauvegarde_postscript_en_couleur !f (!xsize,!ysize)


let afficher_expr () =
  if(!grey = true) then
    print_endline (Expr.string_of_expr !e)
  else
    match !f with
      | (a,b,c) -> 
	  begin
	    print_endline "Expression utilisée pour la couleur Rouge :";
	    print_endline (Expr.string_of_expr a);
	    print_endline "Expression utilisée pour la couleur Verte :";
	    print_endline (Expr.string_of_expr b);
	    print_endline "Expression utilisée pour la couleur Bleue :";
	    print_endline (Expr.string_of_expr c);
	  end


let key_event k = 
  match k with
    | 'g' -> generer ()
    | 's' -> sauver ()
    | 'i' -> afficher_expr ()
    | 'q' -> raise End_of_program
    | _ -> ()


let event () = 
  let liste_evenements = [Graphics.Key_pressed] in
    try
      while (true) do
	let s = Graphics.wait_next_event liste_evenements in
	  if s.Graphics.keypressed then key_event s.Graphics.key
          else ()
      done
    with 
	End_of_program -> ()


let anon s = raise (Arg.Bad("Argument incorrect : "^s))

	
let _ = 
  if !Sys.interactive then () else
    begin
      Arg.parse (Arg.align args) anon (Sys.argv.(0)^" s'utilise de la manière suivante:");
      Random.self_init();
      Graphics.open_graph (" "^(string_of_int (int_of_float ((float) !xsize /. !preview)))^"x"^(string_of_int (int_of_float ((float) !ysize /. !preview))));
      Graphics.set_window_title "OcamlArt";
      aide ();
      event ();
      Graphics.close_graph ();
      if (Sys.command ("rm tmp.ps") < 0) then 
	print_endline "Erreur : effacement de tmp.ps"
    end
