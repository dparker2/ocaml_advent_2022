let part1_strategy chars =
  match chars with
  | [ "A"; "X" ] -> 1 + 3
  | [ "A"; "Y" ] -> 2 + 6
  | [ "A"; "Z" ] -> 3 + 0
  | [ "B"; "X" ] -> 1 + 0
  | [ "B"; "Y" ] -> 2 + 3
  | [ "B"; "Z" ] -> 3 + 6
  | [ "C"; "X" ] -> 1 + 6
  | [ "C"; "Y" ] -> 2 + 0
  | [ "C"; "Z" ] -> 3 + 3
  | _ -> raise (Invalid_argument "Unexpected pattern")

let part2_strategy chars =
  match chars with
  | [ "A"; "X" ] -> 3 + 0
  | [ "A"; "Y" ] -> 1 + 3
  | [ "A"; "Z" ] -> 2 + 6
  | [ "B"; "X" ] -> 1 + 0
  | [ "B"; "Y" ] -> 2 + 3
  | [ "B"; "Z" ] -> 3 + 6
  | [ "C"; "X" ] -> 2 + 0
  | [ "C"; "Y" ] -> 3 + 3
  | [ "C"; "Z" ] -> 1 + 6
  | _ -> raise (Invalid_argument "Unexpected pattern")

let build_list () =
  let rec rec_build_list list =
    try
      let line = read_line () in
      let chars = String.split_on_char ' ' line in
      rec_build_list (chars :: list)
    with End_of_file -> list
  in
  rec_build_list []

let calc_scores games strategy =
  let rec rec_calc_scores games score =
    match games with
    | [] -> score
    | game :: rest ->
        let next_score = score + strategy game in
        rec_calc_scores rest next_score
  in
  rec_calc_scores games 0

let () =
  let games = build_list () in
  let part1 = calc_scores games part1_strategy in
  Printf.printf "Part 1: %d\n" part1;
  let part2 = calc_scores games part2_strategy in
  Printf.printf "Part 2: %d\n" part2
