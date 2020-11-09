let rec affiche_hello l =
  let _ = Thread.create affiche_hello l in
    Unix.sleep 2;
    print_string "hello" ; print_newline();

    affiche_hello l;;

let _ = Thread.create affiche_hello 5;;

let () = (* thread principal *)
  let s = read_line () in
  print_string s