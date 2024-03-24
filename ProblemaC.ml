open Printf;; (* Module used *)

type color = W | B (* W: White , B: Black *)
type image = L of color (* leaf of one color *)
           | N of image * image * image * image (* node with four children *)

(*Construction of the quadtree*)
let rec construir_quadtree m i j n =
  if n = 1 then (* If n is equal to 1, it means that we have reached a quadrant of size 1x1, i.e. a leaf of the quadtree *)
    L (if m.(i).(j) = 0 then W else B)
  else
    let k = n / 2 in
    let nw = construir_quadtree m i j k in (* builds the subtree of the upper left quadrant *)
    let ne = construir_quadtree m i (j+k) k in (* builds the subtree of the upper right quadrant *)
    let se = construir_quadtree m (i+k) (j+k) k in (* builds the subtree of the lower right quadrant *)
    let sw = construir_quadtree m (i+k) j k in  (* builds the subtree of the lower left quadrant *)
    match nw, ne, se, sw with
      | L c1, L c2, L c3, L c4 when c1 = c2 && c2 = c3 && c3 = c4 ->
        L c1 (* If all subtrees are leaves and have the same colour, then a single leaf node with colour c1 is returned *)
      | _, _, _, _ ->
        N (nw, ne, se, sw) (* Otherwise, a node is returned with the subtrees as its children *)

(* This function returns a tuple containing the number of leaves and nodes in the quadtree *)
let contador_folhas_nos t =
  let rec aux n_l n_n = function
    | L _ -> (n_l + 1, n_n) (* If the quadtree is a leaf, increase the number of leaves *)
    | N (nw, ne, se, sw) -> (* Increase the number of nodes by 1 and then count the number of leaves and nodes for each child *)
      let n_n_0 = n_n + 1 in 
      let n_l_1, n_n_1 = aux n_l n_n_0 nw in
      let n_l_2, n_n_2 = aux n_l_1 n_n_1 ne in
      let n_l_3, n_n_3 = aux n_l_2 n_n_2 se in
      aux n_l_3 n_n_3 sw
  in
  aux 0 0 t
  
(* Function for calculating the shortest branch. *)
let rec ramo_mais_curto t =
  let rec aux current_length = function (* Auxiliary function that recursively traverses the quadtree and calculates the length of the shortest branch. It has a parameter (current_length) that represents the length of the shortest branch currently found *)
    | L _ -> current_length (* Stops home is a leaf and returns the value of current_length *)
    | N (nw, ne, se, sw) -> (* Calculating the length of the shortest branch in the four children *)
      let nw_shortest = aux (current_length + 1) nw in
      let ne_shortest = aux (current_length + 1) ne in
      let se_shortest = aux (current_length + 1) se in
      let sw_shortest = aux (current_length + 1) sw in
      min (min (min nw_shortest ne_shortest) se_shortest) sw_shortest (* Determine the smallest value using the min function repeatedly. This will give the length of the shortest branch among the children.*)
  in
  aux 0 t
  
(* Function for calculating the longest branch *)
let rec ramo_mais_longo t =
  let rec aux current_length = function
    | L _ -> current_length (* Stops home is a leaf and returns the value of current_length *)
    | N (nw, ne, se, sw) -> (* Calculating the length of the longest branch in the four children *)
      let nw_longest = aux (current_length + 1) nw in
      let ne_longest = aux (current_length + 1) ne in
      let se_longest = aux (current_length + 1) se in
      let sw_longest = aux (current_length + 1) sw in
      max (max (max nw_longest ne_longest) se_longest) sw_longest (* Determine the largest value using the max function repeatedly. This will give the length of the longest branch among the children *)
  in
  aux 0 t

(* This function rotates the matrix 90° to the left *)
let rotate_left m =
  let n = Array.length m in
  let m_rotated = Array.make_matrix n n (-1) in (* Creates a new matrix m_rotated with the same size as the matrix m, filled with -1. This matrix is used to store the matrix resulting from the 90º rotation to the left *)
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      m_rotated.(n - 1 - j).(i) <- m.(i).(j) (* The elements of the m matrix are copied to the m_rotated matrix, but with altered indices, i.e. the element in row i and column j of the m matrix will be placed in row n - 1 - j and column i of the m_rotated matrix *)
    done;
  done;
  m_rotated

(* This function inverts the colour of the pixels *)
let invert_colors m =
  Array.map (fun row -> (* Array.map applies a function to each element of an array and returns a new array with the results, in this case it applies it to each row of the matrix m *)
    Array.map (fun pixel -> (* Applies Array.map to each pixel in the line *)
      if pixel = 0 then 1 else 0
    ) row
  ) m

(* This function applies a 180º rotation to the original matrix *)
let rotate_180 m =
  let n_rows = Array.length m in
  let n_cols = Array.length m.(0) in
  let result = Array.make_matrix n_rows n_cols (-1) in (* Creates a new result matrix with the same size as the m matrix, filled with -1. This matrix is used to store the matrix resulting from the 180º rotation to the left *)
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      result.(i).(j) <- m.(n_rows - 1 - i).(n_cols - 1 - j) (* The elements of matrix m are copied to the result matrix, but with altered indices, i.e. the element of row (n_rows - 1 - i) and column (n_cols - 1 - j) of matrix m will be placed in row i and column j of the result matrix *)
    done;
  done;
  result
    
(* This function allows you to print the matrices resulting from the previous operations on the terminal *)
let print_image image =
  let height = Array.length image in
  let width = Array.length image.(0) in
  Printf.printf "P1\n";
  Printf.printf "%d %d\n" width height;
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      Printf.printf "%d" image.(i).(j);
      if j < width - 1 then
        Printf.printf " "
    done;
    Printf.printf "\n"
  done


let () =
let header = input_line stdin in
if String.sub header 0 2 <> "P1" then (* Checks that the first two characters read from the keyboard and assigned to the header variable are P1 *)
  failwith "Formato de imagem inválido.";
let width, height = Scanf.sscanf (input_line stdin) "%d %d" (fun w h -> (w, h)) in (* Assigns to the width and height variables the size of the matrix, in this case the values will be the same since it is a square matrix. *)
let m = Array.make_matrix height width (-1) in (* Returns a two-dimensional array in which the first dimension is equivalent to the height variable and the second to width. *)
for i = 0 to height - 1 do
  let line = input_line stdin in (* Reads an entire line from the input and assigns the value to the line variable *)
  let values = List.map int_of_string (String.split_on_char ' ' line) in (* Splits the line of text into substrings separated by spaces and converts each substring into an integer *)
  List.iteri (fun j v ->
    if v <> 0 && v <> 1 then (* Checks whether the value v is different from 0 or 1 *)
      failwith "Valor inválido encontrado."
    else
      m.(i).(j) <- v
  ) values
done;  
(*The List.iteri function receives two arguments: a lambda function and a list.
The lambda function is applied to each element in the list. It takes two arguments, j (the index of the element) and v (the value of the element) *)
  
let quadtree = construir_quadtree m 0 0 width in
let n_leaves, n_nodes = contador_folhas_nos quadtree in
let shortest = ramo_mais_curto quadtree in
let longest = ramo_mais_longo quadtree in
let rotated_image = rotate_left m in
let inverted_image = invert_colors rotated_image in
let rotated_180 = rotate_180 inverted_image in
printf "%d %d\n%d %d\n" n_leaves n_nodes shortest longest;
print_image rotated_image;
print_image inverted_image;
print_image rotated_180;

(* Bibliography:
  https://en.wikipedia.org/wiki/Netpbm
  https://www.geeksforgeeks.org/quad-tree/
  https://v2.ocaml.org/api/Array.html
*)
