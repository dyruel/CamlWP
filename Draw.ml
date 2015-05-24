(*Draw.ml*)

(**
   @author Chopin Morgan
   @since 19/02/2007
   @version 7.2.23
*)


(** Convertie une coordonnée de l'intervalle [-n,n] vers un réel de l'intervalle [-1,1] *)
let coordonnees_vers_reel x n = (float) x /. (float) n

(** Convertieun réel de l'intervalle [-1,1] en un entier compris entre 0 et 255 *)
let reel_vers_composante x = 
  if (x <= 0.) then  int_of_float(127. *. (1. +. x))
  else int_of_float(128. +. ( 127. *. x))

(** Dessine un point de couleur [(r,g,b)] à la postion [(x,y)] dans la fenêtre graphique *)
let dessine_point_sur_ecran (r,g,b) (x,y) = 
  begin
    Graphics.set_color(Graphics.rgb r g b);
    Graphics.plot x y;
  end

(**  Dessine un point de couleur [(r,g,b)] à la postion [(x,y)] dans un fichier PostScript *)
let dessine_point_dans_postscript (r,g,b) (x,y) =
  begin
    GraphicsPs.set_color(GraphicsPs.rgb r g b);
    GraphicsPs.plot x y;
  end

(* ...en niveau de gris...*)

(** Affiche en niveau de gris une expression feval dans un fenêtre graphique de taille [(l,h)] en employant la fonction de dessin [fdessin] *)
let dessine_en_gris fdessin feval (l,h) =
  for x = 0 to (l - 1) do
    for y = 0 to (h - 1) do
      let e = feval((coordonnees_vers_reel x l),(coordonnees_vers_reel y h)) in
      let nivo_gris = int_of_float((255. /. 2.) +. ((255. /. 2.) *. e)) in
	fdessin (nivo_gris,nivo_gris,nivo_gris) (x,y)
    done
  done

(**  *)
let dessine_en_gris_sur_ecran feval (l,h) = dessine_en_gris (dessine_point_sur_ecran) (feval) (l,h)

(** *)
let dessine_en_gris_dans_postscript (eval) (l,h) = dessine_en_gris (dessine_point_dans_postscript) (eval) (l,h)

(** *)
let genere_preview_en_gris z (l,h) =
  let expression = Expr.random_expr ((Random.int 5) + 7) in
    begin
      dessine_en_gris_sur_ecran (Expr.eval (expression)) (int_of_float ((float) l /. z),int_of_float ((float) h /. z));
      expression;
    end

(** *)
let sauvegarde_postscript_en_gris e (l,h) =
  begin
    GraphicsPs.open_graph (" "^(string_of_int l )^"x"^(string_of_int h ));
    GraphicsPs.open_ps "tmp.ps";
    dessine_en_gris_dans_postscript (Expr.eval (e)) (l,h);
    GraphicsPs.close_graph ();
  end


(*...ou en couleur*)

(** Affiche en couleur une expression feval dans un fenêtre graphique de taille [(l,h)] en employant la fonction de dessin [fdessin] *)
let dessine_en_couleur fdessin (fevalR,fevalG,fevalB) (l,h) =
  for x = 0 to (l - 1) do
    for y = 0 to (h - 1) do
      let r = fevalR((coordonnees_vers_reel x l),(coordonnees_vers_reel y h)) 
      and g = fevalG((coordonnees_vers_reel x l),(coordonnees_vers_reel y h))
      and b = fevalB((coordonnees_vers_reel x l),(coordonnees_vers_reel y h)) in
	fdessin ((reel_vers_composante r),(reel_vers_composante g),(reel_vers_composante b)) (x,y)
    done
  done

let dessine_en_couleur_sur_ecran (fevalR,fevalG,fevalB) (l,h) = dessine_en_couleur (dessine_point_sur_ecran) (fevalR,fevalG,fevalB) (l,h)
let dessine_en_couleur_dans_postscript (fevalR,fevalG,fevalB) (l,h) = dessine_en_couleur (dessine_point_dans_postscript) (fevalR,fevalG,fevalB) (l,h)


let genere_preview_en_couleur z (l,h) =
  let expressionR = Expr.random_expr ((Random.int 5) + 5) and expressionG = Expr.random_expr ((Random.int 5) + 5) and expressionB = Expr.random_expr ((Random.int 5) + 5) in
    begin
      dessine_en_couleur_sur_ecran ((Expr.eval (expressionR)),(Expr.eval (expressionG)),(Expr.eval (expressionB))) (int_of_float ((float) l /. z),int_of_float ((float) h /. z));
      (expressionR,expressionG,expressionB);
    end


let sauvegarde_postscript_en_couleur (eR,eG,eB) (l,h) = 
  begin
    GraphicsPs.open_graph (" "^(string_of_int l )^"x"^(string_of_int h ));
    GraphicsPs.open_ps "tmp.ps";
    dessine_en_couleur_dans_postscript (Expr.eval eR,Expr.eval eG,Expr.eval eB) (l,h);
    GraphicsPs.close_graph ();
  end
