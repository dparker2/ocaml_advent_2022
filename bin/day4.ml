let read_stdin () =
  let rec rec_readin list =
    try
      let line = read_line () in
      rec_readin (line :: list)
    with End_of_file -> list
  in
  rec_readin []

type assignmentT = { low : int; high : int }

(* "X-Y" -> { low: X; high: Y } *)
let to_assignment rangestr =
  let range = String.split_on_char '-' rangestr in
  match List.map int_of_string range with
  | [ low; high ] -> { low; high }
  | _ -> invalid_arg "Not a range"

(* "A-B,X-Y" -> ({ low: A; high: B }, { low: X; high: Y }) *)
let to_assignments line =
  let pairstr = String.split_on_char ',' line in
  match pairstr with
  | [ a; b ] -> (to_assignment a, to_assignment b)
  | _ -> invalid_arg "Cannot find two assignments"

let fully_overlap (a, b) =
  (a.low >= b.low && a.high <= b.high) || (b.low >= a.low && b.high <= a.high)

let partial_overlap (a, b) =
  (a.low <= b.high && a.high >= b.low) || (b.low <= a.high && b.high >= a.low)

let count_trues list =
  List.fold_left (fun acc b -> acc + if b then 1 else 0) 0 list

let () =
  let input = read_stdin () in
  let assignments = List.map to_assignments input in
  let part1 = count_trues (List.map fully_overlap assignments) in
  Printf.printf "Part 1: %d\n" part1;
  let part2 = count_trues (List.map partial_overlap assignments) in
  Printf.printf "Part 2: %d\n" part2
