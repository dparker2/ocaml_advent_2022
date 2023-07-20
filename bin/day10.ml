let signals = Array.make 241 1

let instructions =
  let rec rec_read_lines lines =
    try rec_read_lines ((read_line () |> String.split_on_char ' ') :: lines)
    with End_of_file -> lines
  in
  rec_read_lines [] |> List.rev

let run instrs =
  let rec rrun idx = function
    | [] -> ()
    | instr :: rest -> (
        let signal = signals.(idx - 1) in
        match instr with
        | [ "noop" ] ->
            signals.(idx) <- signal;
            rrun (idx + 1) rest
        | [ "addx"; x ] ->
            signals.(idx) <- signal;
            signals.(idx + 1) <- signal + int_of_string x;
            rrun (idx + 2) rest
        | _ -> rrun idx rest)
  in
  rrun 1 instrs

let strgth cycle = signals.(cycle - 1) * cycle

let print_crt () =
  for idx = 0 to 239 do
    let pixel = idx mod 40 in
    if pixel == 0 then print_newline ();
    if signals.(idx) - 1 <= pixel && pixel <= signals.(idx) + 1 then
      print_char '#'
    else print_char '.'
  done

let () =
  run instructions;
  strgth 20 + strgth 60 + strgth 100 + strgth 140 + strgth 180 + strgth 220
  |> Printf.printf "Part 1: %d\n";
  print_string "Part 2:";
  print_crt ();
  print_newline ()
