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

let sum3 list =
  match list with
  | i1 :: i2 :: i3 :: _ -> i1 + i2 + i3
  | _ -> invalid_arg "Not 3 in list"

let () =
  let list = build_list () in
  let part1 = List.fold_left max min_int list in
  Printf.printf "Part 1: %d\n" part1;
  let sorted = List.sort (fun x y -> compare y x) list in
  let part2 = sum3 sorted in
  Printf.printf "Part 2: %d\n" part2
