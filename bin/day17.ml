type shape = { height : int; offsets : (int * int) list }

let shapes =
  [
    { height = 1; offsets = [ (0, 0); (0, 1); (0, 2); (0, 3) ] };
    { height = 3; offsets = [ (0, 1); (1, 0); (1, 1); (1, 2); (2, 1) ] };
    { height = 3; offsets = [ (0, 0); (0, 1); (0, 2); (1, 2); (2, 2) ] };
    { height = 4; offsets = [ (0, 0); (1, 0); (2, 0); (3, 0) ] };
    { height = 2; offsets = [ (0, 0); (0, 1); (1, 0); (1, 1) ] };
  ]
  |> List.to_seq |> Seq.cycle

let jetcycle = read_line () |> String.to_seq |> Seq.cycle

let fix_move chamber shape oldpos (x, y) =
  try
    if
      shape.offsets
      |> List.for_all (fun (dx, dy) -> chamber.(x + dx).(y + dy) = '.')
    then (x, y)
    else oldpos
  with Invalid_argument _ -> oldpos

let push jets (x, y) =
  match Seq.uncons jets with
  | Some ('<', rest) -> ((x, y - 1), rest)
  | Some ('>', rest) -> ((x, y + 1), rest)
  | Some _ | None -> invalid_arg "push"

let drop (x, y) = (x - 1, y)

let stop chamber shape (x, y) =
  shape.offsets |> List.iter (fun (dx, dy) -> chamber.(x + dx).(y + dy) <- '#');
  x + shape.height

let rec new_rock chamber jets shape pos =
  let fix_this = fix_move chamber shape in
  let pushed, newjets = push jets pos in
  let pos2 = fix_this pos pushed in
  let dropped = drop pos2 |> fix_this pos2 in
  if dropped = pos2 then (stop chamber shape dropped, newjets)
  else new_rock chamber newjets shape dropped

(* let debug chamber =
   print_endline "Chamber:";
   for i = 0 to 20 do
     for j = 0 to 6 do
       print_char chamber.(20 - i).(j)
     done;
     print_newline ()
   done;
   print_newline () *)

let simulate_part1 n_rocks =
  let chamber = Array.make_matrix (n_rocks * 4) 7 '.' in
  let step (height, jets) shape =
    let stopheight, newjets = new_rock chamber jets shape (height + 3, 2) in
    (max stopheight height, newjets)
  in
  shapes |> Seq.take n_rocks |> Seq.fold_left step (0, jetcycle) |> fst

let simulate_part2 n_rocks =
  let cache = Hashtbl.create 2000 in
  let chamber = Array.make_matrix 10000 7 '.' in
  let rec step height realheight count jets shapes =
    if count = n_rocks then realheight
    else
      let shape, newshapes = Seq.uncons shapes |> Option.get in
      let stopheight, newjets = new_rock chamber jets shape (height + 3, 2) in
      let newtop = max stopheight height in
      let nextcount = count + 1 in
      let realheight, newcount =
        if newtop > 50 && realheight = height then
          let firstjet, _ = Seq.uncons jets |> Option.get in
          let hashkey = (Array.sub chamber (newtop - 50) 50, shape, firstjet) in
          match Hashtbl.find_opt cache hashkey with
          | None ->
              Hashtbl.add cache hashkey (newtop, nextcount);
              (realheight + (newtop - height), nextcount)
          | Some (h, n) ->
              let cyclecount = nextcount - n in
              let cycleh = newtop - h in
              let ncycles = (n_rocks - nextcount) / cyclecount in
              ((cycleh * ncycles) + newtop, (cyclecount * ncycles) + nextcount)
        else (realheight + (newtop - height), nextcount)
      in
      step newtop realheight newcount newjets newshapes
  in
  shapes |> step 0 0 0 jetcycle

(* let detect_cycle cache chamber_slice shape jet = match Hashtbl.find_opt cache (chamber_slice, shape, jet) with
   | Some height -> *)

let () =
  simulate_part1 2022 |> Printf.printf "Part 1: %d\n";
  simulate_part2 1000000000000 |> Printf.printf "Part 2: %d\n"
