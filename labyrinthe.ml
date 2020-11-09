open Graphics;;
open_graph " 900x1080";;

let pacman_idx = ref 0;;
let fantome_idx = ref 0;;

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
    (d, x , y);;

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
      for y = 0 to (l - 1) do
        if y = l - 1 && d = 0 then ()
        else begin
          if mur_present.(d).(x).(y)
          then trace_mur upleftx uplefty taille_case (d, x, y)
        end;
      done;
    done;
  done;;

let verify_edges l h x =
  (x < l * h) && (x >= 0);;

let move_pacman l h key taille_case mur_present =
  let xl = !pacman_idx / l in
  let yl = !pacman_idx mod l in

  match key with
  | 'z' when verify_edges l h (!pacman_idx - l) ->
    if !pacman_idx >= l && not mur_present.(1).(xl - 1).(yl)
    then pacman_idx := !pacman_idx - l;
  | 'q' when verify_edges l h (!pacman_idx - 1) ->
    if yl >= 1 && not mur_present.(0).(xl).(yl - 1)
    then pacman_idx := !pacman_idx - 1;
  | 's' when verify_edges l h (!pacman_idx + l) ->
    if not mur_present.(1).(xl).(yl)
    then pacman_idx := !pacman_idx + l;
  | 'd' when verify_edges l h (!pacman_idx + 1) ->
    if !pacman_idx < ((l * h) - 1) && not mur_present.(0).(xl).(yl)
    then pacman_idx := !pacman_idx + 1;
  | _ -> ();;

let dessine_pac x y c =
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
  set_color black;;

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
  pacman_pos;;

let is_win l h =
  !pacman_idx = (l * h) - 1;;

let move_fantome l h =
  let pos = ref !fantome_idx in
  let xfantome = !fantome_idx / l in
  let yfantome = !fantome_idx mod l in
  let xpacman = !pacman_idx / l in
  let ypacman = !pacman_idx mod l in

  if xfantome <> xpacman
  then begin
    if xfantome < xpacman
    then pos := !pos + l
    else pos := !pos - l
  end
  else begin
    if yfantome < ypacman
    then
      if verify_edges l h (!pos + 1)
      then pos := !pos + 1
      else pos := !pos - l
    else
    if verify_edges l h (!pos - 1)
    then pos := !pos - 1
    else pos := !pos + l
  end;
  fantome_idx := !pos;;

open Unix;;

let ia (upleftx, uplefty, l, h, pacman_pos, taille_case, mur_present) =
  let x = ref (pacman_pos.(!fantome_idx).(0)) in
  let y = ref (pacman_pos.(!fantome_idx).(1)) in
  dessine_pac !x !y blue;
  while not (is_win l h) && (!pacman_idx <> !fantome_idx) do
    Unix.sleep 2;
    move_fantome l h;
    x := (pacman_pos.(!fantome_idx).(0));
    y := (pacman_pos.(!fantome_idx).(1));
    clear_graph ();
    dessine_pac !x !y blue;
    dessine_pac pacman_pos.(!pacman_idx).(0) pacman_pos.(!pacman_idx).(1) yellow;
    trace_lab upleftx uplefty taille_case l h mur_present;
  done;;

let draw_game upleftx uplefty l h taille_case =
  let mur_present = generate_lab l h in
  let pacman_pos = gen_pacman_array_position upleftx uplefty l h taille_case in
  let x = ref pacman_pos.(!pacman_idx).(0) in
  let y = ref pacman_pos.(!pacman_idx).(1) in
  let _ = Thread.create ia (upleftx, uplefty, l, h, pacman_pos,  taille_case, mur_present) in
  clear_graph ();
  trace_lab upleftx uplefty taille_case l h mur_present;

  dessine_pac !x !y yellow;
  while not (is_win l h) && (!pacman_idx <> !fantome_idx) do
    let key = read_key() in
    move_pacman l h key taille_case mur_present;
    x := pacman_pos.(!pacman_idx).(0);
    y := pacman_pos.(!pacman_idx).(1);
    clear_graph ();
    dessine_pac !x !y yellow;
    dessine_pac pacman_pos.(!fantome_idx).(0) pacman_pos.(!fantome_idx).(1) blue;
    trace_lab upleftx uplefty taille_case l h mur_present;
  done;
  clear_graph ();
  moveto 500 500;
  if (!pacman_idx = !fantome_idx)
  then draw_string "PERDU"
  else draw_string "GAGNE";;

let () =
  let l = 10 in
  let h = 10 in
  let upleftx = 50 in
  let uplefty = 850 in
  let taille_case = 40 in
  fantome_idx := (l - 1);
  pacman_idx := 0;
  set_line_width 2;
  draw_game upleftx uplefty l h taille_case;;