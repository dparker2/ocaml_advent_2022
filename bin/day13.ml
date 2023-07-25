(* extract_num "13,23]" -> (13, "23]") *)
let extract_num str =
  let i = ref 0 in
  let acc = ref "" in
  while str.[!i] != ']' && str.[!i] != ',' do
    acc := !acc ^ Char.escaped str.[!i];
    i := !i + 1
  done;
  (int_of_string !acc, String.sub str !i (String.length str - !i))

let str_tail str = String.sub str 1 (String.length str - 1)

let rec compare_signal left right =
  (* Printf.printf "%s and %s ?\n" left right; *)
  match (left.[0], right.[0]) with
  | '[', '[' | ']', ']' | ',', ',' ->
      compare_signal (str_tail left) (str_tail right)
  | ']', _ -> -1
  | _, ']' -> 1
  | '[', _ ->
      let num, rest = extract_num right in
      compare_signal left ("[" ^ string_of_int num ^ "]" ^ rest)
  | _, '[' ->
      let num, rest = extract_num left in
      compare_signal ("[" ^ string_of_int num ^ "]" ^ rest) right
  | _, _ ->
      let lnum, lrest = extract_num left in
      let rnum, rrest = extract_num right in
      (* Printf.printf "Comparing %c and %c\n" l r; *)
      if lnum = rnum then compare_signal lrest rrest else compare lnum rnum

let read_lines () =
  let rec rec_read_line lines1 lines2 =
    try
      let line = read_line () in
      if line = "" then rec_read_line lines1 lines2
      else rec_read_line lines2 (line :: lines1)
    with End_of_file -> (List.rev lines1, List.rev lines2)
  in
  rec_read_line [] []

let sum_of_neg_indices list =
  list
  |> List.mapi (fun i b -> if b = -1 then i + 1 else 0)
  |> List.fold_left ( + ) 0

let signal_index list str =
  let rec find i = function
    | [] -> -1
    | hd :: tl -> if hd == str then i else find (i + 1) tl
  in
  find 1 list

let () =
  let lefts, rights = read_lines () in
  List.map2 compare_signal lefts rights
  |> sum_of_neg_indices
  |> Printf.printf "Part 1: %d\n";
  let sorted_signals =
    List.concat [ [ "[[2]]"; "[[6]]" ]; lefts; rights ]
    |> List.sort compare_signal
  in
  signal_index sorted_signals "[[2]]" * signal_index sorted_signals "[[6]]"
  |> Printf.printf "Part 2: %d\n"
