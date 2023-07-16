(* Reads from stdin and builds a string list of each line *)
let read_rucksacks () =
  let rec rec_build_list list =
    try
      let line = read_line () in
      rec_build_list (line :: list)
    with End_of_file -> list
  in
  rec_build_list []

(* Splits each string in a list into a tuple of halves *)
let split_rucksacks strings =
  let split string =
    let halfLen = String.length string / 2 in
    let half1 = String.sub string 0 halfLen in
    let half2 = String.sub string halfLen halfLen in
    (half1, half2)
  in
  List.map split strings

(* Chunks strings in a list into tuples of 3 *)
let chunk3 strings =
  let rec rec_chunk3 list =
    match list with
    | [] -> []
    | hd1 :: hd2 :: hd3 :: rest -> (hd1, hd2, hd3) :: rec_chunk3 rest
    | _ -> invalid_arg "list not a multiple of 3"
  in
  rec_chunk3 strings

(* Given a length 2 tuple of strings, returns a string of common characters *)
let common_item2 (a, b) =
  let common_reducer commons ch =
    if String.contains b ch then String.make 1 ch ^ commons else commons
  in
  String.fold_left common_reducer "" a

(* Given a length 3 tuple of strings, returns a string of common characters *)
let common_item3 (a, b, c) =
  let ab_commons = common_item2 (a, b) in
  common_item2 (ab_commons, c)

let priority ch =
  if Char.lowercase_ascii ch == ch then Char.code ch - 96 else Char.code ch - 38

let sum_first_priority strings =
  List.fold_left (fun acc str -> acc + priority str.[0]) 0 strings

let () =
  let rucksacks = read_rucksacks () in
  let halved_sacks = split_rucksacks rucksacks in
  let part1_commons = List.map common_item2 halved_sacks in
  let part1 = sum_first_priority part1_commons in
  Printf.printf "Part1: %d\n" part1;
  let chunked_sacks = chunk3 rucksacks in
  let part2_commons = List.map common_item3 chunked_sacks in
  let part2 = sum_first_priority part2_commons in
  Printf.printf "Part2: %d\n" part2
