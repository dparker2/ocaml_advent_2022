type stackT = char list
type stacksT = stackT array
type instrT = int * int * int

(* Returns ints of instructions, if the string is an instruction *)
let parse_instr str : instrT option =
  try
    Scanf.sscanf str "move %d from %d to %d" (fun num from_stack to_stack ->
        Some (num, from_stack - 1, to_stack - 1))
  with Scanf.Scan_failure _ -> None

(* Reads from stdin and builds a crates string list and the procedures *)
let read_stdin () =
  let rec rec_readin crates_section procedures =
    try
      let line = read_line () in
      match line with
      | "" -> rec_readin crates_section procedures
      | _ -> (
          match parse_instr line with
          | Some instr -> rec_readin crates_section (instr :: procedures)
          | None -> rec_readin (line :: crates_section) procedures)
    with End_of_file -> (crates_section, procedures)
  in
  let r1, r2 = rec_readin [] [] in
  (r1, List.rev r2)

(* Initializes the mutable array, given the " 1   2   3 " stacks line *)
let init_stacks stack_nums : stacksT =
  let trimed_stack_nums = String.trim stack_nums in
  let n_stacks_char = trimed_stack_nums.[String.length trimed_stack_nums - 1] in
  let n_stacks_int = int_of_string (Char.escaped n_stacks_char) in
  Array.init n_stacks_int (fun _ -> [])

(* Mutates the array, building out the lists containing the crates *)
let fill_stacks stacks_arr crates_strs =
  let n_stacks = Array.length stacks_arr in
  List.iter
    (fun crate_str ->
      for i = 0 to n_stacks - 1 do
        match crate_str.[1 + (i * 4)] with
        | ' ' -> ()
        | c -> Array.set stacks_arr i (c :: stacks_arr.(i))
      done)
    crates_strs

(* Creates a new array of crate stacks containing the result of the
   procedures and strategy *)
let simulate stacks procedures strategy =
  let new_arr = Array.copy stacks in
  let mover = strategy new_arr in
  List.iter mover procedures;
  new_arr

(* Helper to print the heads of each list in the array *)
let print_heads array =
  Array.iter
    (fun list ->
      let head = List.hd list in
      print_char head)
    array;
  print_newline ()

(* This strategy moves 1 head at a time from list to list *)
let part1_strategy array (num, from_stack, to_stack) =
  (* Printf.printf "Moving %d from %d to %d\n" num from_stack to_stack; *)
  for _ = 1 to num do
    match array.(from_stack) with
    | [] -> invalid_arg "Cannot move: out of crates"
    | crate :: rest ->
        Array.set array to_stack (crate :: array.(to_stack));
        Array.set array from_stack rest
  done

(* This strategy removes num heads from one stack then
   cons them in reverse order to other stack. Preserving
   the order of the moved crates. *)
let part2_strategy array (num, from_stack, to_stack) =
  let rec rec_strategy = function
    | 0 -> ()
    | i -> (
        match array.(from_stack) with
        | [] -> invalid_arg "Cannot move: out of crates"
        | crate :: rest ->
            Array.set array from_stack rest;
            rec_strategy (i - 1);
            Array.set array to_stack (crate :: array.(to_stack)))
  in
  rec_strategy num

let () =
  let crates_section, procedures = read_stdin () in
  (* List.iter print_endline crates_section;
     List.iter
       (fun (num, from, to_) -> Printf.printf "%d,%d,%d\n" num from to_)
       procedures; *)
  let stacks = init_stacks (List.hd crates_section) in
  fill_stacks stacks (List.tl crates_section);
  let part1_stacks = simulate stacks procedures part1_strategy in
  print_string "Part 1: ";
  print_heads part1_stacks;
  let part2_stacks = simulate stacks procedures part2_strategy in
  print_string "Part 2: ";
  print_heads part2_stacks
