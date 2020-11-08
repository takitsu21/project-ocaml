#load "graphics.cma";;
open Graphics;;
open_graph " 1920x1080";
set_line_width 2;;

module type UFsig =
sig
  type t
  val create : int -> t
  val find : t -> int -> int
  val union : t -> int -> int -> unit
end

module UF : UFsig =
struct
  type t = {parent: int array; rang: int array}

  let create n =
    { parent = Array.init n (fun i -> i); rang=Array.make n 0 }

  let rec find uf x =
    let pi = uf.parent.(x) in
    if pi == x then
      x
    else begin
      let racine = find uf pi in
      uf.parent.(x) <- racine; (* Compression de chemin *)
      racine
    end

  let union ({ parent = p; rang = r } as uf) x y =
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

open UF;;

let cases_adjacentes l h (d, x, y) =
  if d = 1
  then ((x * l) + y, (x * l) + y + l)
  else ((x * l) + y, (x * l) + y + 1);;

let mur_au_hasard l h =
  let x = Random.int h in
  let y = Random.int l in
  let d = Random.int 2 in
  if x = (h-1) then
    if y = l-1 then
      (0,x,y-1)
    else (0, x, y) else
  if y = (l- 1) then
    if x < h - 1 then
      (1, x, y)
    else (1,x-1,y) else
    (d , x , y);;

let test l h =
  for i = 0 to 100 do
    let (d, x, y) = mur_au_hasard l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in
    if i >= 35 || j >= 35 then Printf.printf "%d %d\n" i j;
  done;;

let gen_mur_present l h =
  let mur_present = Array.make 2 [||] in
  for d = 0 to 1 do
    mur_present.(d) <- Array.make h [||];
    for x = 0 to h - 1 do
      mur_present.(d).(x) <- Array.make l true
    done;
  done;
  mur_present;;

let generate_lab l h =
  let mur_present = gen_mur_present l h in
  let uf = create (l * h) in
  let acc = ref 1 in
  while !acc < (l * h) do
    let (d, x, y) = mur_au_hasard l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in
    Printf.printf "%d %d\n" i j;
    if find uf i <> find uf j
    then begin
      union uf i j;
      mur_present.(d).(x).(y) <- false;
      acc := !acc + 1;
    end
  done;
  mur_present;;

let trace_pourtour upleftx uplefty taille_case l h =
  moveto upleftx uplefty;
  lineto (upleftx + (taille_case * l)) uplefty; (* HAUT *)
  lineto (current_x()) (current_y() - (taille_case * (h - 1))); (* DROITE *)
  moveto (current_x()) (current_y() - taille_case); (* ouverture bas droite *)
  lineto upleftx (current_y()); (* BAS *)
  lineto upleftx (uplefty - taille_case);; (* GAUCHE *)

let trace_mur upleftx uplefty taille_case (d, x, y) =
  if d = 0
  then begin
    moveto (upleftx + (y * taille_case) + taille_case) (uplefty - (x * taille_case));
    lineto (current_x()) (current_y() - taille_case);
  end

  else begin
    moveto (upleftx + (y * taille_case)) (uplefty - (x * taille_case) - taille_case);
    lineto (current_x() + taille_case) (current_y());
  end;;

(* 1 POUR HORIZONTAL
   0 POUR VERTICAL *)

let trace_lab upleftx uplefty taille_case l h mur_present =
  trace_pourtour upleftx uplefty taille_case l h;
  for d = 0 to 1 do
    for x = 0 to (h - 1) do
      for y = 0 to (l - 1) do (* h - 2 pour éviter de faire les mur_present sur le coter droit *)
        if y = l - 1 && d = 0 then ()
        else begin
          if mur_present.(d).(x).(y)
          then trace_mur upleftx uplefty taille_case (d, x, y)
        end;
      done;
    done;
  done;;

let pacman_idx = ref 0;;

let move_pacman h key taille_case = match key with
  | 'z' -> pacman_idx := !pacman_idx - h;
  | 'q' -> pacman_idx := !pacman_idx - 1;
  | 's' -> pacman_idx := !pacman_idx + h;
  | 'd' -> pacman_idx := !pacman_idx + 1;
  | _ -> ();;

let dessine_pac x y c = begin
  let oeil(x,y) =
    set_color black;
    fill_circle (x + 5) (y + 7) 4 in
  let bouche(x,y) =
    set_color white;
    fill_arc (x) (y)  20  20 (-20) 10  in
  set_color c;
  fill_circle (x) (y) 20;
  oeil(x,y);
  bouche(x,y);
  set_color black;
end

let gen_pacman_array_position upleftx uplefty l h taille_case =
  let pacman_pos = Array.init (l * h) (fun i -> [|i; i|]) in

  let x = ref (upleftx + (taille_case / 2)) in
  let y = ref (uplefty - (taille_case / 2)) in
  pacman_pos.(0).(0) <- !x;
  pacman_pos.(0).(1) <- !y;

  for i = 1 to (l * h) - 1 do
    if (i mod l) = 0
    then begin
      x := upleftx + (taille_case / 2);
      y := !y - taille_case;
    end
    else begin
      x := !x + taille_case;
    end;
    pacman_pos.(i).(0) <- !x;
    pacman_pos.(i).(1) <- !y;
  done;
  pacman_pos

let draw_game upleftx uplefty l h taille_case =
  let mur_present = generate_lab l h in
  let pacman_pos = gen_pacman_array_position upleftx uplefty l h taille_case in
  let x = ref pacman_pos.(!pacman_idx).(0) in
  let y = ref pacman_pos.(!pacman_idx).(1) in
  clear_graph ();
  trace_lab upleftx uplefty taille_case l h mur_present;
  dessine_pac !x !y yellow;
  while true do
    let key = read_key() in
    try
      move_pacman h key taille_case;
      x := pacman_pos.(!pacman_idx).(0);
      y := pacman_pos.(!pacman_idx).(1);
      clear_graph ();
      dessine_pac !x !y yellow;
      trace_lab upleftx uplefty taille_case l h mur_present;
    with invalid_arg -> ();
  done;;

let is_win l h =
  !pacman_idx = (l * h) - 1;;

let check_wall mur_present (d, x, y) =
  mur_present.(d).(x).(y)

let () =
  draw_game 100 850 200 200 4;;