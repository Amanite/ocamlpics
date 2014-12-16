open Graphics

open Images

let array_of_pic s = lire_image s

let save_pic t s = sauver_image t s

let draw_pic x y t = dessiner_image_position x y t

(* renaming functions in order to make this understandable to mostly anyone*)

let monalisa = array_of_pic "monalisa_128.jpg"

let monalisa2 = array_of_pic "monalisa_2.jpg"


let squared256 = 256*256

let cubed256 = squared256*256

(* defining two constants that will be used heavily in order to compute them only once *)

let rgb_of_int n = 
	let r = n/squared256 in
	let g = n/256 - (256*r) in
	let b = n - (squared256 * r + (256*g)) in
	[|r;g;b|]
(* retrieving rgb values from a color integer *) 

let int_of_rgb t = t.(0) * squared256 + t.(1)*256 + t.(2)

(* creating an integer rgb value from an explicit array *)

let likeness n1 n2 =
	let rgb1 = rgb_of_int n1
	and rgb2 = rgb_of_int n2 in
	let delta_red = abs (rgb1.(0) - rgb2.(0)) in
	let delta_green = abs (rgb1.(0) - rgb2.(0)) in
	let delta_blue = abs (rgb1.(0) - rgb2.(0)) in
	let sum_delta = delta_green + delta_blue + delta_red in
	let resultat = float_of_int (sum_delta) /. float_of_int(3*256) in
	let percentage = (1. -. resultat) *. 100. in
	percentage
	
	(* likeness m n returns the likeness in percentage of two pixels represented by integers *)
	
let comparison_array t1 t2 = 
	let n = Array.length t1 in
	let total_likeness = ref 0. in
	for i = 0 to (n-1) do
		total_likeness := !total_likeness +. (likeness t1.(i) t2.(i))
	done;
	!total_likeness /. float_of_int(n)
	
	(* returns the likeness between two arrays of pixels. One array of pixels here represents one row (line) of pixels *)
	
let comparison_matrix t1 t2 =
	let n = Array.length t1 in
	let likeness = ref 0. in
	for i = 0 to (n-1) do
		likeness := !likeness +. (comparison_array t1.(i) t2.(i))
	done;
	!likeness /. (float_of_int n) 
	
	(* returns the likeness between two matrixes (= pictures) as a percentage *)

let draw_random_triangle (x,y) =
	let random_color = rgb (Random.int(255)) (Random.int(255)) (Random.int(255))
	and a = (Random.int(x),Random.int(y))
	and b = (Random.int(x),Random.int(y))
	and c = (Random.int(x),Random.int(y)) in
	set_color random_color;
	fill_poly [|a;b;c|]
	(* generates a random triangle of a random color that may not be bigger than the x and y given as parameters *) 

let usage() =
	Printf.printf "Genetic algorithm building pictures based on evolutive pressure and random number generation \n
	Syntax : ./pixevolution <file>, where file is a valid picture file, format supported : .png, .jpg, .bmp, .gif, .ppm, .pgm"
	
let original = array_of_pic Sys.argv.(1)
	
let initialization() =
	Unix.sleep 1;
	let x = Array.length original.(0)
	and y = Array.length original in
	open_graph " 1000x1000";
	Random.self_init();
	draw_pic 600 400 original

let runloop s =
	let run = ref true  (* creating a flag to halt execution if needed *) in
	let x = Array.length original.(0)
	and y = Array.length original in (* determining the size of the original picture*)
	let canvas = ref (Array.make_matrix x y (int_of_rgb [|0;0;0|])) in (* creating a blank canvas *)
	let best_attempt = ref (Array.make_matrix x y (int_of_rgb [|0;0;0|]))
	and record = ref 0. in (* record being the score performed by the most resembling picture so far *)
	let mutate() =
		draw_random_triangle (x,y);
		let attempt = dump_image (get_image 0 0 x y) in
		comparison_matrix attempt original in
	let redraw() =
		dessiner_image_position 0 0 !best_attempt in
	while !run do
		let new_attempt = mutate() in
		if new_attempt >= !record then begin
			canvas := dump_image(get_image 0 0 x y);
			record := new_attempt;
			best_attempt := !canvas;
			redraw()end
		else begin
			canvas := !best_attempt;
			redraw() end
	done
	
let _ = if Array.length(Sys.argv) <> 2 then usage()
		else initialization();
			 runloop (Sys.argv.(1))
