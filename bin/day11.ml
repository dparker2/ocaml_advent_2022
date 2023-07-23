type monkey = {
  mutable inspections : int;
  mutable items : int list;
  initial_items : int list;
  operation : string;
  op_num : int;
  div_test : int;
  if_true : int;
  if_false : int;
}

let empty_monkey =
  {
    inspections = 0;
    items = [];
    initial_items = [];
    operation = "";
    op_num = 0;
    div_test = 0;
    if_true = 0;
    if_false = 0;
  }

let add_items monkey =
  match String.split_on_char ':' (read_line ()) with
  | [ _; list ] ->
      let items =
        list |> String.split_on_char ',' |> List.map String.trim
        |> List.map int_of_string |> List.rev
      in
      { monkey with items; initial_items = items }
  | _ -> monkey

let add_operation monkey =
  match String.split_on_char ' ' (read_line ()) with
  | [ _; _; _; "new"; "="; "old"; op; "old" ] ->
      { monkey with operation = op ^ " old" }
  | [ _; _; _; "new"; "="; "old"; op; value ] ->
      { monkey with operation = op; op_num = int_of_string value }
  | _ -> monkey

let last_word_as_int string =
  string |> String.split_on_char ' ' |> List.rev |> List.hd |> int_of_string

let add_div_test monkey =
  { monkey with div_test = last_word_as_int (read_line ()) }

let add_if_true monkey =
  { monkey with if_true = last_word_as_int (read_line ()) }

let add_if_false monkey =
  { monkey with if_false = last_word_as_int (read_line ()) }

let load_monkeys () =
  let rec rec_load_monkeys monkeys =
    try
      read_line () |> ignore;
      let monkey =
        empty_monkey |> add_items |> add_operation |> add_div_test
        |> add_if_true |> add_if_false
      in
      (try read_line () |> ignore with End_of_file -> ());
      rec_load_monkeys (monkey :: monkeys)
    with End_of_file -> List.rev monkeys
  in
  rec_load_monkeys []

let calc_worry monkey worry =
  match monkey.operation with
  | "*" -> worry * monkey.op_num
  | "+" -> worry + monkey.op_num
  | "* old" -> worry * worry
  | _ -> invalid_arg "Unknown operation"

let do_round monkeys modifier monkey =
  let calc_this_worry = calc_worry monkey in
  monkey.items |> List.rev
  |> List.iter (fun worry ->
         let new_worry = worry |> calc_this_worry |> modifier in
         let to_index =
           if new_worry mod monkey.div_test == 0 then monkey.if_true
           else monkey.if_false
         in
         let to_monkey = List.nth monkeys to_index in
         to_monkey.items <- new_worry :: to_monkey.items;
         monkey.inspections <- monkey.inspections + 1);
  monkey.items <- []

let do_rounds monkeys modifier n =
  for _ = 1 to n do
    List.iter (do_round monkeys modifier) monkeys
  done

let reset_monkeys monkeys =
  monkeys
  |> List.iter (fun monkey ->
         monkey.items <- monkey.initial_items;
         monkey.inspections <- 0)

let monkey_business monkeys =
  match
    monkeys
    |> List.map (fun monkey -> monkey.inspections)
    |> List.sort (fun x y -> compare y x)
  with
  | n1 :: n2 :: _ -> n1 * n2
  | _ -> 0

let common_div monkeys =
  monkeys
  |> List.map (fun monkey -> monkey.div_test)
  |> List.fold_left (fun acc div -> acc * div) 1

let () =
  let monkeys = load_monkeys () in
  do_rounds monkeys (fun x -> x / 3) 20;
  Printf.printf "Part 1: %d\n" (monkey_business monkeys);
  reset_monkeys monkeys;
  let gcd = common_div monkeys in
  do_rounds monkeys (fun x -> x mod gcd) 10000;
  Printf.printf "Part 2: %d\n" (monkey_business monkeys)
