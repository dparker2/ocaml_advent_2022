type directory = { parent : directory option; size : int ref }

let root = { parent = None; size = ref 0 }

let rec add_size dir size =
  dir.size := !(dir.size) + size;
  match dir.parent with None -> () | Some parent -> add_size parent size

let rec build_dirs curr_dir dirs =
  try
    let line = read_line () in
    match String.split_on_char ' ' line with
    | [ "$"; "cd"; "/" ] -> build_dirs root dirs
    | [ "$"; "cd"; ".." ] -> (
        match curr_dir.parent with
        | None -> build_dirs curr_dir dirs
        | Some parent -> build_dirs parent dirs)
    | [ "$"; "cd"; _ ] ->
        let new_dir = { parent = Some curr_dir; size = ref 0 } in
        build_dirs new_dir (new_dir :: dirs)
    | [ size; _ ] -> (
        match int_of_string_opt size with
        | None -> build_dirs curr_dir dirs
        | Some size ->
            add_size curr_dir size;
            build_dirs curr_dir dirs)
    | _ -> build_dirs curr_dir dirs
  with End_of_file -> dirs

let () =
  let dirs = build_dirs root [ root ] in
  let sizes = List.map (fun d -> !(d.size)) dirs in
  let sum_under_100k =
    List.fold_left (fun sum x -> if x <= 100000 then sum + x else sum) 0 sizes
  in
  Printf.printf "Part 1: %d\n" sum_under_100k;
  let needed_space = !(root.size) - 40000000 in
  let delete_size =
    List.fold_left
      (fun min_size x ->
        if x < min_size && x > needed_space then x else min_size)
      max_int sizes
  in
  Printf.printf "Part 2: %d\n" delete_size
