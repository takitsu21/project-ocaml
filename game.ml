(*
Réalisé par :
- BATISSE DYLANN
- JUNIN THIBAULT
*)

open Graphics;;

let pacman_idx = ref 0;;
let fantome_idx = ref 0;;
let center = ref (0, 0);;


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
    if pi = x then
      x
    else begin
      let racine = find uf pi in
      uf.parent.(x) <- racine; (* Compression de chemin *)
      racine
    end

  let union ({ parent = p; rang = r } as uf) x y =
    let cx = find uf x in
    let cy = find uf y in
    if cx <> cy
    then begin
      if r.(cx) < r.(cy)
      then p.(cx) <- cy
      else begin
        p.(cy) <- cx;
        if r.(cx) = r.(cy) then
          r.(cx) <- r.(cx) + 1
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
  if x = (h - 1) then begin
    if y = l - 1 then
      (0, x, y - 1)
    else (0, x, y)
  end
  else begin
    if y = (l - 1) then
      if x < h - 1 then
        (1, x, y)
      else (1, x - 1, y)
    else (d, x , y)
  end;;

let gen_mur_present l h =
  let mur_present = Array.make 2 [||] in
  for d = 0 to 1 do
    mur_present.(d) <- Array.make h [||];
    for x = 0 to h - 1 do
      mur_present.(d).(x) <- Array.make l true
    done;
  done;
  mur_present;;

let verify_edges l h x =
  (x < l * h) && (x >= 0);;

let case_voisines i l h =
  if i < l && i > 0 && i <(l-1) then [|i-1;i+1;i+l|]
  else if i < l && i > 0 && i = (l-1) then [|i-1;i+l|]
  else if i mod l = 0 && (i/l) = 0 then [|i+1;i+l|]
  else if (i mod l ) = 0 && (i / l)  < h-1 then [|i-l;i+1;i+l|]
  else if (i mod l ) = 0 && (i / l) = h-1 then [|i-l;i+1|]
  else if (i mod l ) = (h-1) && (i / l)  < h-1 then [|i-1;i-l;i+l|]
  else if (i mod l ) = (h-1) && (i / l) = h-1 then [|i-1;i-l|]
  else if (i/l) = h-1 then [|i-1;i-l;i+1|]
  else [|i-1;i-l;i+1;i+l|] ;;

let cases_voisines l h value =
  let liste = [|value-1;value+1;value+l;value-l|] in
  let array = ref [||] in
  for i = 0 to (Array.length liste) - 1 do
    if verify_edges l h liste.(i)
    then array := Array.append !array [|liste.(i)|]
  done;
  !array;;

let gen_voisines l h =
  Array.init (l*h) (fun i -> case_voisines i l h);;

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
  set_color white;
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

let make_sound () =
  sound 15000 500;;

let draw_player x y c taille_case =
  set_color c;
  fill_circle (x) (y) (taille_case / 3);
  set_color white;;

let move_pacman l h key mur_present =
  let xpacman = !pacman_idx / l in
  let ypacman = !pacman_idx mod l in

  match key with
  | 'z' when verify_edges l h (!pacman_idx - l) ->
    if !pacman_idx >= l && not mur_present.(1).(xpacman - 1).(ypacman)
    then pacman_idx := !pacman_idx - l else begin make_sound (); end;
  | 'q' when verify_edges l h (!pacman_idx - 1) ->
    if ypacman >= 1 && not mur_present.(0).(xpacman).(ypacman - 1)
    then pacman_idx := !pacman_idx - 1 else begin make_sound (); end;
  | 's' when verify_edges l h (!pacman_idx + l) ->
    if not mur_present.(1).(xpacman).(ypacman)
    then pacman_idx := !pacman_idx + l else begin make_sound (); end;
  | 'd' when verify_edges l h (!pacman_idx + 1) ->
    if !pacman_idx < ((l * h) - 1) && not mur_present.(0).(xpacman).(ypacman)
    then pacman_idx := !pacman_idx + 1 else begin make_sound (); end;
  | 'w' -> raise (invalid_arg "jeu fermer");
  | _ -> ();;

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

let evite value l h mur_present =
  let voisin = case_voisines value l h in
  let get_xpos case = case / l in
  let get_ypos case = case mod l in
  let ret = ref [||] in
  for i=0 to (Array.length voisin) - 1 do
    if voisin.(i) <> value then
      let x = get_xpos voisin.(i) in
      let y = get_ypos voisin.(i) in
      if voisin.(i) = value-1 then
        if (mur_present.(0).(x).(y) = true)
        then ret := Array.append !ret [|voisin.(i)|];
      if voisin.(i) = value-l then
        if (mur_present.(1).(x).(y) = true)
        then ret := Array.append !ret [|voisin.(i)|];
      if voisin.(i) = value+1 then begin
        let ix = get_xpos value in
        let iy = get_ypos value in
        if (mur_present.(0).(ix).(iy) = true)
        then ret := Array.append !ret [|voisin.(i)|]
      end;
      if voisin.(i) = value+l then
        let ix = get_xpos value in
        let iy = get_ypos value in
        if (mur_present.(1).(ix).(iy) = true)
        then ret := Array.append !ret [|voisin.(i)|]
  done;
  !ret;;

let gen_evite voisines mur_present l h =
  Array.init (Array.length voisines) (fun i -> evite i l h mur_present);;

let rectify_voisines voisines evites =
  let new_array = Array.init (Array.length voisines) (fun _ -> [||]) in
  for i = 0 to (Array.length voisines) - 1 do
    let tmp_array = ref [||] in
    for j = 0 to  (Array.length voisines.(i)) - 1 do
      if not (Array.mem voisines.(i).(j) evites.(i))
      then tmp_array := Array.append !tmp_array [|voisines.(i).(j)|]
    done;
    new_array.(i) <- !tmp_array;
  done;
  new_array;;

exception Break;;

let rec est_relie src dst _evite voisines =
  if src = dst then true
  else
    begin
      try
        for i = 0 to (Array.length voisines.(src)) - 1 do
          if _evite <> voisines.(src).(i) then begin
            if est_relie voisines.(src).(i) dst src voisines
            then raise Break
            else false;
          end
          else false
        done;
        false
      with Break -> true
    end;;

let ia (upleftx, uplefty, l, h, pacman_pos, taille_case, mur_present) =
  draw_player pacman_pos.(!fantome_idx).(0) pacman_pos.(!fantome_idx).(1) red taille_case;
  let v = (gen_voisines l h) in
  let voisines = rectify_voisines v (gen_evite v mur_present l h) in
  while not (is_win l h) && (!pacman_idx <> !fantome_idx) do
    Unix.sleep 2;
    (* ignore (Unix.select [] [] [] 0.1); *)
    draw_player pacman_pos.(!fantome_idx).(0) pacman_pos.(!fantome_idx).(1) black taille_case; (* On redessine en blanc a la position d'avant *)
    try
      for i = 0 to (Array.length voisines.(!fantome_idx)) - 1 do
        let relie = est_relie voisines.(!fantome_idx).(i) !pacman_idx !fantome_idx voisines in
        if relie then begin
          fantome_idx := voisines.(!fantome_idx).(i);
          raise Break
        end
      done;
    with Break -> ();
      if (not (is_win l h) && (!pacman_idx <> !fantome_idx)) (* double verification au cas ou pendant le Unix.sleep il y a un gagnant ou perdant *)
      then draw_player pacman_pos.(!fantome_idx).(0) pacman_pos.(!fantome_idx).(1) red taille_case
  done;
  clear_graph ();
  moveto (fst !center) (snd !center);
  if !pacman_idx = !fantome_idx
  then begin
    set_color black;
    fill_rect 0 0 (size_x ()) (size_y ());
    set_color foreground;
    set_color white;
    draw_string "PERDU";
  end
  else if (is_win l h)
  then begin
    set_color black;
    fill_rect 0 0 (size_x ()) (size_y ());
    set_color foreground;
    set_color white;
    draw_string "GAGNE";
  end;;

let case_voisines i l h =
  if i < l && i > 0 && i <(l-1) then [|i-1;i;i+1;i+l|]
  else if i < l && i > 0 && i = (l-1) then [|i-1;i;i;i+l|]
  else if (i mod l ) = (h-1) && (i / l)  < h-1 then [|i-1;i-l;i;i+l|]
  else if (i/l) = h-1 then [|i-1;i-l;i+1;i|]
  else if (i mod l ) = (h-1) && (i / l) = h-1 then [|i-1;i-l;i;i|]
  else if (i mod l ) = 0 && (i / l) = h-1 then [|i;i-l;i+1;i|]
  else if i mod l = 0 && (i/l) = 0 then [|i;i;i+1;i+l|]
  else if (i mod l ) = 0 && (i / l)  < h-1 then [|i;i-l;i+1;i+l|]
  else [|i-1;i-l;i+1;i+l|] ;;

let draw_game upleftx uplefty l h taille_case =
  let mur_present = generate_lab l h in
  let pacman_pos = gen_pacman_array_position upleftx uplefty l h taille_case in
  let _ = Thread.create ia (upleftx, uplefty, l, h, pacman_pos,  taille_case, mur_present) in
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color foreground;
  trace_lab upleftx uplefty taille_case l h mur_present;
  moveto (upleftx) (upleftx);
  draw_string "Z HAUT - Q GAUCHE - S BAS - D DROITE - W QUITTER";
  draw_player pacman_pos.(!pacman_idx).(0) pacman_pos.(!pacman_idx).(1) blue taille_case;
  while not (is_win l h) && (!pacman_idx <> !fantome_idx) do
    let key = read_key() in
    if (not (is_win l h) && (!pacman_idx <> !fantome_idx)) (* double verification au cas ou pendant le read_key() il y a un gagnant ou perdant *)
    then begin
      draw_player pacman_pos.(!pacman_idx).(0) pacman_pos.(!pacman_idx).(1) black taille_case; (* On redessine en blanc a la position d'avant *)
      move_pacman l h key mur_present;
      draw_player pacman_pos.(!pacman_idx).(0) pacman_pos.(!pacman_idx).(1) blue taille_case;
    end;
  done;
  clear_graph ();
  moveto (fst !center) (snd !center);
  if (is_win l h)
  then begin
    set_color black;
    fill_rect 0 0 (size_x ()) (size_y ());
    set_color foreground;
    set_color white;
    draw_string "GAGNE";
  end;
  if !pacman_idx = !fantome_idx
  then begin
    set_color black;
    fill_rect 0 0 (size_x ()) (size_y ());
    set_color foreground;
    set_color white;
    draw_string "PERDU"
  end;;

let () =
  let l = 10 in
  let h = 10 in
  let taille_case = ref 40 in
  let upleftx = !taille_case / 2 in
  let uplefty = (h + 1) * !taille_case in
  let _ = Random.self_init () in
  let width = (l + 1) * !taille_case in
  let height = (h + 2) * !taille_case in
  let graph_size = " " ^ string_of_int width ^ "x" ^ string_of_int height in
  center := (width / 2, height / 2);
  open_graph graph_size;
  fantome_idx := (l - 1);
  pacman_idx := 0;
  draw_game upleftx uplefty l h !taille_case;
  ignore @@ read_key ();;