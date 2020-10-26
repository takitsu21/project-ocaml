
#load "graphics.cma";;
open Graphics;;

open_graph " 480x270";;

(* Pas sur a 100% *)
module UF =
struct
  type t = {forest: int array}
  let create n =
    {forest=Array.init n (fun i -> i)}

  let find uf x =
    let rec aux i =
      if uf.forest.(i) = x
      then i
      else aux (i+1) in
    aux 0

  let union uf n m =
    let xRacine = find uf n in
    let yRacine = find uf m in
    if xRacine = yRacine
    then uf
    else begin
      let rec aux i =
        if i = yRacine
        then uf
        else begin
          uf.forest.(i) <- yRacine;
          aux (i+1);
        end in
      aux xRacine
    end
end

(* TO FIX C KC *)
let case_adjacentes l h (d, x, y) =
  if d = 0
  then (h - (y + x), h - (y + x) + 1)
  else ((y * x) + l + 1,  (y * x) + (2 * l) + 1)

case_adjacentes 5 5 (1, 1, 2);;