#load "graphics.cma";;
open Graphics;;
open_graph " 600x400";
set_line_width 2;

module UF =
struct
  type t = {parent: int array; rank: int array}

  let create n =
    { parent = Array.init n (fun i -> i); rank=Array.make n 0 }

  let rec find uf i =
    let pi = uf.parent.(i) in
    if pi == i then
      i
    else begin
      let racine = find uf pi in
      uf.parent.(i) <- racine; (* path compression *)
      racine
    end

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
end;;


let cases_adjacentes l h (d, x, y) =
  if d = 1
  then ((x * l) + y, (x * l) + y + l)
  else ((x * l) + y, (x * l) + y + 1)

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

let gen_mur_present l h =
  let mur_present = Array.make 2 [||] in
  for d = 0 to 1 do
    mur_present.(d) <- Array.make l [||];
    for x = 0 to l - 1 do
      mur_present.(d).(x) <- Array.make h true
    done;
  done;
  mur_present

let generate_lab l h =
  let mur_present = gen_mur_present l h in
  let uf = UF.create (l * h) in
  let acc = ref 1 in
  while !acc < (l * h) do
    let (d, x, y) = mur_au_hasard l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in
    if UF.find uf i <> UF.find uf j
    then begin
      UF.union uf i j;
      mur_present.(d).(x).(y) <- false;
      acc := !acc + 1;
    end
  done;
  mur_present

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
    moveto (upleftx + (y * taille_case)) (uplefty - (x * taille_case) - taille_case);
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
      done;
    done;
  done;;



let () =
  clear_graph();
  trace_lab 100 850 40 5 5;;