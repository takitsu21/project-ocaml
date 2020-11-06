open Format;;
#load "graphics.cma";;
open Graphics;;
open_graph " 600x400";;

set_line_width 2;

module UF =
struct
  let max_rang = ref 0
  (* type t = {forest: (int * int) array} *)
  type t = {parent: int array; rank: int array}
  (* let create n =
     {forest=Array.init (n - 1) (fun i -> (i, 0))} *)

  let create n =
    { parent = Array.init n (fun i -> i); rank=Array.make n 0 }


  let rec find uf i =
    let pi = uf.parent.(i) in
    if pi == i then
      i
    else begin
      let ci = find uf pi in
      uf.parent.(i) <- ci; (* path compression *)
      ci
    end


  (* let rec find uf x =
     Printf.printf "on est la %d\n" x;
     if fst uf.forest.(x) <> x
     then begin
      let racine = find uf (fst uf.forest.(x)) in
      uf.forest.(x) <- (racine, snd uf.forest.(x));
      racine;
     end
     else (fst uf.forest.(x)) *)

  (* if fst uf.forest.(x) = x then
     x
     else
     begin
      let racine = find uf (fst uf.forest.(x)) in
      uf.forest.(x) <- (racine, snd uf.forest.(x)); (* Compression de chemin. *)
      racine
     end *)

  let union ({ parent = p; rank = r } as uf) x y =
    let cx = find uf x in
    let cy = find uf y in
    if cx != cy then begin
      if r.(cx) > r.(cy) then
        p.(cy) <- cx
      else if r.(cx) < r.(cy) then
        p.(cx) <- cy
      else begin
        r.(cx) <- r.(cx) + 1;
        p.(cy) <- cx
      end
    end


  (* if (snd uf.forest.(i) = snd uf.forest.(j)) then ()
     else begin
     if (snd uf.forest.(i) > snd uf.forest.(j)) then
      uf.forest.(j) <- (i, snd uf.forest.(j))
     else begin
      uf.forest.(i) <- (j, snd uf.forest.(i));

      if (snd uf.forest.(i) = snd uf.forest.(i))
      then uf.forest.(j) <- (fst uf.forest.(j), (snd uf.forest.(j) + 1))

     end
     end *)

  (* let rec aux uf i j =
     (* Union par rang. *)
     if snd uf.forest.(i) <= snd uf.forest.(j) then begin
      uf.forest.(i) <- (j, snd uf.forest.(i));
      if snd uf.forest.(i) = snd uf.forest.(j) then begin
        uf.forest.(j) <- (fst uf.forest.(j), snd uf.forest.(j) + 1);
        max_rang := max !max_rang (snd uf.forest.(j))
      end
     end
     else begin
      print_int i;
      aux uf j i
     end
     in if i <> j then aux uf i j; *)
end;;

let cases_adjacentes l h (d, x, y) =
  if d = 1
  then ((x * l) + y, (x * l) + y + l)
  else ((x * l) + y, (x * l) + y + 1)

let mur_au_hasard l h = (* renvoie un triplet (d, x, y) *)
  let n = Random.int ((l-1) * h + l * (h-1)) in
  if n < (l-1) * h
  then (0, n mod (l-1), n / (l-1))
  else let n2 = n - (l-1) * h in
    (1, n2 mod l, n2 / l)

let generate_lab l h =
  let mur_present = Array.make 2 (Array.make l (Array.init h (fun i -> true))) in
  Printf.printf "mur present length : %d\n" (Array.length mur_present.(0).(0));
  let uf = UF.create (l * h) in
  let acc = ref 1 in
  while !acc < (l * h) - 1 do
    let (d, x, y) = mur_au_hasard l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in

    try
      (* printf "%d\n" !acc; *)
      if UF.find uf i <> UF.find uf j
      then begin
        UF.union uf i j;
        Printf.printf "d = %d x = %d y = %d\n" d x y;


      end
    with invalid_arg -> acc := !acc + 1; mur_present.(d).(x).(y) <- false;
      (* else *)
  done;
  (uf, mur_present)

let trace_pourtour upleftx uplefty taille_case l h =
  moveto upleftx uplefty;
  lineto (upleftx + (taille_case * l)) uplefty;
  lineto (current_x()) (current_y() - (taille_case * h));
  lineto upleftx (current_y());
  lineto upleftx uplefty;;

let trace_mur upleftx uplefty taille_case (d, x, y) =
  if d = 0
  then begin
    moveto (upleftx + (y * taille_case) + taille_case) (uplefty - (x * taille_case));
    lineto (current_x()) (current_y() - taille_case);
  end

  else begin
    moveto (upleftx + (y * taille_case)) (uplefty - (x * taille_case)- taille_case);
    lineto (current_x() + taille_case) (current_y());
  end

(* 1 POUR HORIZONTAL
   0 POUR VERTICAL *)

let trace_lab upleftx uplefty taille_case l h =
  let mur_present = generate_lab l h in
  trace_pourtour upleftx uplefty taille_case l h;
  for d = 0 to 1 do
    for x = 0 to (l - 1) do
      for y = 0 to (h - 1) do
        if mur_present.(d).(x).(y)
        then trace_mur upleftx uplefty taille_case (d, x, y)
      done
    done
  done



let () =
  clear_graph();
  trace_lab 100 300 40 5 5;;
