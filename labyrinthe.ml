open Graphics;;


let pacman_idx = ref 0;;
let fantome_idx = ref 0;;
let center = ref (0, 0);;
let current_case_path_finding = ref 0;;
let checkpoint = ref 0;;
let old_pos = ref 0;;


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
  (x < l * h) && (x > 0);;
(* let xp = x / l in
   let yp = x mod l in *)

(* Printf.printf "%d %d\n" !xp !yp; *)
(* if x > l && x > h
   then begin xp := !xp / l; yp := !xp / h; end;
   (x < l * h) && (x > 0) && (!xp <= l) && (!yp <= h) && (!xp >= 0) && (!yp >= 0);; *)

(* (x < l * h) && (x > 0);; *)

(* let verify_path_connexe mur_present l h d x y=
   if not mur_present.(d).(x).(y)
   then
   let (i, j) = cases_adjacentes l h (d, x, y)
   else *)

(* let rec verify_path mur_present l h d x y =

   if mur_present.(d).(x).(y)
   then []
   else  *)

(* let verify_collide mur_present l h value (d, x, y) =
   if (value < l * h) && (x > 0) && not mur_present.(0).(x).(y) *)

let cases_voisines l h value =
  let liste = [|value-1;value+1;value+l;value-l|] in
  let array = ref [||] in
  for i = 0 to (Array.length liste) - 1 do
    if verify_edges l h liste.(i)
    then array := Array.append !array [|liste.(i)|]
  done;
  !array;;


(* let test =
   Array.init 5 (fun i -> Array.init 5 (cases_voisines 5 5 i)) *)
(* let xarray = [|x-1;x+1;x+l;x-l|] in
   let yarray =
   let new_array = ref [||] in
   for i = 0 to 3 do
   if verify_edges l h array.(i) && not mur_present.(d).(x).(y)
   then new_array := Array.append !new_array [|array.(i)|]
   done;
   !new_array;; *)

let generate_lab l h =
  let mur_present = gen_mur_present l h in
  let voisines = List.init (l * h) (fun i -> i) in
  let uf = create (l * h) in
  let acc = ref 1 in
  while !acc < (l * h) do
    let (d, x, y) = mur_au_hasard l h in
    let (i, j) = cases_adjacentes l h (d, x, y) in
    if find uf i <> find uf j
    then begin
      union uf i j;
      mur_present.(d).(x).(y) <- false;
      (* voisines.(!acc - 1) <- cases_voisines mur_present l h !acc (d, x, y); *)
      acc := !acc + 1;
    end
  done;
  List.iter (fun i -> Printf.printf "%d" i) voisines;
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



let make_sound () =
  sound 15000 500;;

let update_voisines l h mur_present tmp_path q =
  let xpath = !current_case_path_finding / l in
  let ypath = !current_case_path_finding mod l in
  let acc = ref 0 in
  (* if not tmp_path.(!current_case_path_finding) *)
  (* then begin *)

  if verify_edges l h (!current_case_path_finding + l) && not tmp_path.(!current_case_path_finding + l) && not mur_present.(1).(xpath).(ypath)
  then acc := !acc + 1;
  if verify_edges l h (!current_case_path_finding - 1) && not tmp_path.(!current_case_path_finding - 1) && ypath >= 1 && not mur_present.(0).(xpath).(ypath - 1)
  then acc := + 1;
  if verify_edges l h (!current_case_path_finding + l) && not tmp_path.(!current_case_path_finding + l) && not mur_present.(1).(xpath).(ypath)
  then acc := !acc + 1;
  if verify_edges l h (!current_case_path_finding + 1) && not tmp_path.(!current_case_path_finding + 1) && !current_case_path_finding < ((l * h) - 1) && not mur_present.(0).(xpath).(ypath)
  then acc := !acc + 1;

  if verify_edges l h (!current_case_path_finding - l) && not tmp_path.(!current_case_path_finding - l) && !current_case_path_finding >= l && not mur_present.(1).(xpath - 1).(ypath)
  then begin current_case_path_finding := !current_case_path_finding - l; end
  else if verify_edges l h (!current_case_path_finding - 1) && not tmp_path.(!current_case_path_finding - 1) && ypath >= 1 && not mur_present.(0).(xpath).(ypath - 1)
  then begin current_case_path_finding := !current_case_path_finding - 1; end
  else if verify_edges l h (!current_case_path_finding + l) && not tmp_path.(!current_case_path_finding + l) && not mur_present.(1).(xpath).(ypath)
  then begin current_case_path_finding := !current_case_path_finding + l; end
  else if verify_edges l h (!current_case_path_finding + 1) && not tmp_path.(!current_case_path_finding + 1) && !current_case_path_finding < ((l * h) - 1) && not mur_present.(0).(xpath).(ypath)
  then current_case_path_finding := !current_case_path_finding + 1;

  if tmp_path.(!current_case_path_finding) then acc := !acc -1;
  if !acc <= 0 then current_case_path_finding := Queue.pop q;


  if !current_case_path_finding = !checkpoint then current_case_path_finding := !fantome_idx; ignore @@ Queue.pop q; Queue.push !current_case_path_finding q;
  if !acc > 1 then checkpoint := !current_case_path_finding; Queue.push !checkpoint q;;

(* Printf.printf "top queue = %d\n" (Queue.top q); *)
(* Queue.iter (fun i -> Printf.printf "%d\n" i) q; *)
(* try
   Printf.printf "tmp_path %b\n" tmp_path.(!current_case_path_finding)
   with _ -> Printf.printf "erreur : %d" !current_case_path_finding; raise (invalid_arg "test");; *)


let draw_player x y c taille_case =
  set_color c;
  fill_circle (x) (y) (taille_case / 3);
  set_color black;;

let path_finding l h mur_present tmp_path pos_array q =
  (* cases_voisines mur_present l h;;  *)
  let old_tmp = ref [||] in
  let visited = [] in
  old_tmp := tmp_path;
  tmp_path.(!current_case_path_finding) <- true;
  while !pacman_idx <> !current_case_path_finding do

    update_voisines l h mur_present tmp_path q;
    tmp_path.(!current_case_path_finding) <- true;
    draw_player (pos_array.(!checkpoint).(0)) (pos_array.(!checkpoint).(1)) yellow 20;
    draw_player (pos_array.(!current_case_path_finding).(0)) (pos_array.(!current_case_path_finding).(1)) green 10;


    Printf.printf "path = %d pacman = %d checkpoint = %d top queue = %d\n" !current_case_path_finding !pacman_idx !checkpoint (Queue.top q);
  done;
  tmp_path;;


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

(* let rec est_relie src dst evite voisines =
   if src = dst then true else begin
   for i = 0 to (Array.length voisines.(dst)) - 1 do
    if (est_relie voisines.(i) dst src voisines)
    then true;
   done;
   false
   end;; *)

let move_fantome l h =
  let pos = ref !fantome_idx in
  let xfantome = !fantome_idx / l in
  let yfantome = !fantome_idx mod l in
  let xpacman = !pacman_idx / l in
  let ypacman = !pacman_idx mod l in

  if yfantome = ypacman then begin
    if xfantome < xpacman
    then pos := !pos + l
    else pos := !pos - l
  end
  else if yfantome < ypacman
  then begin
    if verify_edges l h (!pos + 1)
    then pos := !pos + 1
    else pos := !pos - l
  end
  else begin
    if verify_edges l h (!pos - 1)
    then pos := !pos - 1
    else pos := !pos + l
  end;
  fantome_idx := !pos;;

let ia (upleftx, uplefty, l, h, pacman_pos, taille_case, mur_present) =
  draw_player pacman_pos.(!fantome_idx).(0) pacman_pos.(!fantome_idx).(1) red taille_case;
  while not (is_win l h) && (!pacman_idx <> !fantome_idx) do
    (* ignore (Unix.select [] [] [] 0.5); *)
    Unix.sleep 2;
    let q = Queue.create () in
    Queue.push !current_case_path_finding q;
    (* Queue.add !fantome_idx q; *)

    ignore @@ path_finding l h mur_present (Array.init (l * h) (fun i -> false)) pacman_pos q;
    (* print_string "["; *)
    (* List.iter (fun i -> Printf.printf "%d " i) p; *)
    (* print_string "]\n"; *)
    if (not (is_win l h) && (!pacman_idx <> !fantome_idx)) (* double verification au cas ou pendant le Unix.sleep il y a un gagnant ou perdant *)
    then begin
      draw_player pacman_pos.(!fantome_idx).(0) pacman_pos.(!fantome_idx).(1) white taille_case; (* On redessine en blanc a la position d'avant *)
      move_fantome l h;
      draw_player pacman_pos.(!fantome_idx).(0) pacman_pos.(!fantome_idx).(1) red taille_case;
    end;
  done;
  clear_graph ();
  moveto (fst !center) (snd !center);
  if !pacman_idx = !fantome_idx
  then begin
    draw_string "PERDU";
  end
  else if (is_win l h)
  then begin
    draw_string "GAGNE";
  end;;

let case_voisines i l h = Printf.printf " <case voisines> appélé pour %d\n " i;
  if i < l && i > 0 && i <(l-1) then [|i-1;i;i+1;i+l|]
  else if i < l && i > 0 && i = (l-1) then [|i-1;i;i;i+l|]
  else if i mod l = 0 && (i/l) = 0 then [|i;i;i+1;i+l|]
  else if (i mod l ) = 0 && (i / l)  < h-1 then [|i;i-l;i+1;i+l|]
  else if (i mod l ) = 0 && (i / l) = h-1 then [|i;i-l;i+1;i|]
  else if (i mod l ) = (h-1) && (i / l)  < h-1 then [|i-1;i-l;i;i+l|]
  else if (i mod l ) = (h-1) && (i / l) = h-1 then [|i-1;i-l;i;i|]
  else if (i/l) = h-1 then [|i-1;i-l;i+1;i|]
  else [|i-1;i-l;i+1;i+l|] ;;

let evite i l h mur_present =
  let voisin = cases_voisines l h i in
  let give_x case = case / l in
  let give_y case = case mod l in
  let tab = ref [||] in
  for j=0 to (Array.length voisin) - 1 do
    if voisin.(j) <> i then
      let x = give_x voisin.(j) in
      let y = give_y voisin.(j) in

      if voisin.(j) = i-1 then begin
        if (mur_present.(0).(x).(y) = true) then tab := Array.append !tab [|voisin.(j)|] end;
      if voisin.(j) = i-l then begin
        if (mur_present.(1).(x).(y) = true) then tab := Array.append !tab [|voisin.(j)|] end;
      if voisin.(j) = i+1 then begin
        let ix = give_x i in
        let iy = give_y i in
        if (mur_present.(0).(ix).(iy) = true) then tab := Array.append !tab [|voisin.(j)|] end;
      if voisin.(j) = i+l then
        begin
          let ix = give_x i in
          let iy = give_y i in
          if (mur_present.(1).(ix).(iy) = true) then tab := Array.append !tab [|voisin.(j)|] end;
  done;
  !tab;;


let draw_game upleftx uplefty l h taille_case =
  let mur_present = generate_lab l h in
  let pacman_pos = gen_pacman_array_position upleftx uplefty l h taille_case in
  let _ = Thread.create ia (upleftx, uplefty, l, h, pacman_pos,  taille_case, mur_present) in
  clear_graph ();
  trace_lab upleftx uplefty taille_case l h mur_present;
  draw_player pacman_pos.(!pacman_idx).(0) pacman_pos.(!pacman_idx).(1) blue taille_case;
  while not (is_win l h) && (!pacman_idx <> !fantome_idx) do
    let key = read_key() in
    if (not (is_win l h) && (!pacman_idx <> !fantome_idx)) (* double verification au cas ou pendant le read_key() il y a un gagnant ou perdant *)
    then begin
      draw_player pacman_pos.(!pacman_idx).(0) pacman_pos.(!pacman_idx).(1) white taille_case; (* On redessine en blanc a la position d'avant *)
      move_pacman l h key mur_present;
      draw_player pacman_pos.(!pacman_idx).(0) pacman_pos.(!pacman_idx).(1) blue taille_case;
    end;
  done;
  clear_graph ();
  moveto (fst !center) (snd !center);
  if (is_win l h)
  then draw_string "GAGNE";
  if !pacman_idx = !fantome_idx
  then begin
    draw_string "PERDU";
  end;;

let gen_voisines l h =
  let ret = Array.init (l*h) (fun i -> cases_voisines l h i) in
  ret;;

let rec est_relie src dst _evite l h mur_present voisines =
  Printf.printf "src = %d dst = %d\n" src dst;
  if src = dst then true
  else
    let evite_array = ref ([||]) in

    for i = 0 to (Array.length voisines) - 1 do
      for j = 0 to (Array.length voisines.(i)) - 1 do
        evite_array := evite voisines.(i).(j) l h mur_present;
        if (Array.length !evite_array) = 0
        then false
        else if (est_relie !evite_array.(0) dst src l h mur_present voisines) then true else true
        (* else false *)
      done;
    done;
    false;;

(* est_relie 4 0 3 l h mur_present voisines;; *)

(* let rec solve src dst correct_path was_here l h mur_present =
   if (src = dst)
   then true;
   if was_here.(src) || (evite src l h mur_present).() = src then false
   else begin
    let src_pos = src mod l in
    was_here.(src) <- true;
    if src != 0
    then if (src != 0)
      then if solve (src-1) dst correct_path was_here l h mur_present
        then correct_path.(src) <- true; true;
    if src_pos != l - 1
    then if solve (src+1) dst correct_path was_here l h mur_present
      then correct_path.(src) <- true; true;
    if src_pos != h - 1 then if solve (src+1) dst correct_path was_here l h mur_present
      then correct_path.(src) <- true; true;
    if src_pos != (l * h - 1) then if solve (src+l) dst correct_path was_here l h mur_present
      then correct_path.(src) <- true; true;




    let solve_maze src dst l h mur_present =
      let was_here = ref (Array.init (l * h) (fun i -> false)) in
      let correct_path = ref (Array.init (l * h) (fun i -> false)) in
      let is_solved = solve src dst !correct_path !was_here l h mur_present in *)





let () =
  let l = 10 in
  let h = 10 in
  let taille_case = ref 40 in
  let upleftx = !taille_case / 2 in
  let uplefty = (h + 1) * !taille_case in
  let _ = Random.self_init () in
  let width = (l + 2) * !taille_case in
  let height = (h + 2) * !taille_case in
  let graph_size = " " ^ string_of_int width ^ "x" ^ string_of_int height in
  center := (width / 2, height / 2);

  open_graph graph_size;
  fantome_idx := (l - 1);
  current_case_path_finding := !fantome_idx;
  checkpoint := !fantome_idx;
  pacman_idx := 0;
  draw_game upleftx uplefty l h !taille_case;
  ignore @@ read_key ();;