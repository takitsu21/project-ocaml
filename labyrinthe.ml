c

module UF =
struct
  let max_rang = ref 0;;
  type t = {forest: (int * int) array}

  let create n =
    {forest=Array.init n (fun i -> (i, 0))};;


  let rec find uf x =

    if fst uf.forest.(x) = x then
      x
    else begin
      let racine = find uf (fst uf.forest.(x)) in
      uf.forest.(x) <- (racine, snd uf.forest.(x)); (* Compression de chemin. *)
      racine
    end

  let union uf n m =
    let i = find uf n in
    let j = find uf m in
    let rec aux uf i j =
      (* Union par rang. *)
      if snd uf.forest.(i) <= snd uf.forest.(j) then begin
        uf.forest.(i) <- (j, snd uf.forest.(i));
        if snd uf.forest.(i) = snd uf.forest.(j) then begin
          uf.forest.(j) <- (fst uf.forest.(j), snd uf.forest.(j) + 1);
          max_rang := max !max_rang (snd uf.forest.(j))
        end
      end
      else begin
        aux uf j i
      end
    in if i <> j then aux uf i j;;
end;;

let cases_adjacentes l h (d, x, y) =
  if d = 1
  then ((x * l) + y, (x * l) + y + l)
  else ((x * l) + y, (x * l) + y + 1)

(* let mur_au_hasard l h =
   let n = Random.int (((l-1) * h) + (l * (h-1))) in
   if n < (l-1) * h
   then (0, (n mod (l-1)), (n / (l-1)))
   else let n2 = n - ((l-1) * h) in
    (1,  (n2 mod l), (n2 / l)) *)

let mur_au_hasard l h =
  let x = Random.int h in
  let y = Random.int l in
  let d = Random.int 2 in
  if x = (l-1) then
    if y = l-1 then
      (0,x,y-1)
    else (0,x,y) else
  if y = (l- 1) then
    if x < h-1 then
      (1,x,y)
    else (1,x-1,y) else
    (d , x , y);;

open Format
let generate_lab l h =
  let mur_present = Array.make 2 (Array.make l (Array.init h (fun i -> true))) in
  let uf = UF.create (l*h) in
  let acc = ref 1 in
  while !acc < (l * h) - 1 do
    let (d, x, y) = mur_au_hasard l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in
    if i < (l * h) - 1 && j < (l * h) - 1 then begin
      printf "%d %d %d\n" x y ((l * h) - 1);
      if UF.find uf i <> UF.find uf j
      then begin
        UF.union uf i j;

        acc := !acc + 1;

      end
      else mur_present.(d).(x).(y) <- false;
    end
    (* else *)
  done;
  (mur_present, uf);;


#load "graphics.cma";;
open Graphics;;
open_graph " 600x400";;

set_line_width 2;


let trace_pourtour upleftx uplefty taille_case l h =
  moveto upleftx uplefty;
  lineto (upleftx + (taille_case * l)) uplefty;
  lineto (upleftx + (taille_case * l)) (uplefty + (taille_case * h));
  lineto upleftx (uplefty + (taille_case * h));
  lineto upleftx uplefty

let trace_mur upleftx uplefty taille_case (d, x, y) =
  if d = 0
  then begin
    moveto (upleftx + (x + taille_case)) (uplefty + (y + taille_case));
    lineto (upleftx + (x + taille_case)) (uplefty + (y + taille_case));
  end

  else begin
    moveto (upleftx + (x * taille_case)) (uplefty + (y * taille_case));
    lineto (upleftx + (x * taille_case)) (y );
  end;;

(* 1 POUR HORIZONTAL
   0 POUR VERTICAL *)
trace_mur 100 50 40 (mur_au_hasard 7 5);;
clear_graph ();;
trace_pourtour 100 50 40 7 5;;
generate_lab 5 5;;
