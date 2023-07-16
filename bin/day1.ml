let build_list () =
  let rec rec_build_list list =
    try
      let line = read_line () in
      match line with
      | "" -> rec_build_list (0 :: list)
      | line ->
          let cals = int_of_string line in
          let new_l = (cals + List.hd list) :: List.tl list in
          rec_build_list new_l
    with End_of_file -> list
  in
  rec_build_list [ 0 ]

let rec sumi list i =
  match (list, i) with
  | [], _ | _, 0 -> 0
  | head :: rest, i -> head + sumi rest (i - 1)

let () =
  let list = build_list () in
  let part1 = List.fold_left max min_int list in
  Printf.printf "Part 1: %d\n" part1;
  let sorted = List.sort (fun x y -> compare y x) list in
  let part2 = sumi sorted 3 in
  Printf.printf "Part 2: %d\n" part2
