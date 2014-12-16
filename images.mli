(**
   Module Images

   Permet de charger et sauvegarder des images stockées dans des fichiers dans divers formats : png, jpg, gif, bmp, pgm, ppm.

   Necessite la suite logicielle ImageMagick.



   L'utilisation de ce module en mode interprete necessite l'appel à l'interpreteur avec les options :

   - [ocaml graphics.cma images.cmo]


   La production d'un executable utilisant ce module doit se faire avec la commande :

   - [ocamlc -o <nom_executable>  graphics.cma images.cmo  <source_a_compiler>]

   @author FIL - IEEA - Univ. Lille1 (mars 2010)

   @see <http://www.imagemagick.org/> le site d'ImageMagick.
*)

(** {2 Lecture et ecriture d'images dans des fichiers} *)

(**
   [liste_formats] = liste des formats autorises.
*)
val liste_formats : string list


(**
   [lire_image s] = tableau de pixels represente par leur couleur, correspondant a l'image stockee dans le fichier nomme [s].

   {b CU :} le fichier nomme [s] doit exister et l'extension de son nom doit correspondre a l'un des formats autorises .
*)
val lire_image : string -> Graphics.color array array

(**
   [sauver_image t s] sauvegarde l'image representee par le tableau de pixels [t] dans un fichier nomme [s]. Le format de sauvegarde est determine par l'extension choisie dans le nom [s]. 

   {b CU :} le format doit etre l'un des formats autorises.
*)
val sauver_image : Graphics.color array array -> string -> unit

(**  {2 Dessiner une image dans une fenetre graphique} *)

(**
   [dessiner_image t] dessine dans la fenetre graphique l'image representee par le tableau de pixels [t]. Le dessin est fait dans le coin inferieur gauche de la fenetre graphique.

   {b CU :} une fenetre graphique doit etre prealablement ouverte.
*)
val dessiner_image : Graphics.color array array -> unit

val dessiner_image_position : int -> int -> Graphics.color array array -> unit
