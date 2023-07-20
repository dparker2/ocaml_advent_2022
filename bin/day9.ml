let read_lines =
  let rec rec_read_lines lines =
    try rec_read_lines (read_line () :: lines) with End_of_file -> lines
  in
  rec_read_lines []

(* R 4 U 4 -> R R R R U U U U *)
let to_motions lines =
  List.fold_left
    (fun acc line ->
      match String.split_on_char ' ' line with
      | [ dir; x ] -> (
          match int_of_string_opt x with
          | Some n -> List.init n (fun _ -> dir) @ acc
          | None -> acc)
      | _ -> acc)
    [] lines

let clamp1 x = if x < -1 then max ~-1 x else min 1 x

let move_head rope dir =
  let x, y = rope.(0) in
  rope.(0) <-
    (match dir with
    | "R" -> (x, y + 1)
    | "L" -> (x, y - 1)
    | "U" -> (x + 1, y)
    | "D" -> (x - 1, y)
    | _ -> (x, y))

let move_tail rope i =
  let headx, heady = rope.(i - 1) in
  let tailx, taily = rope.(i) in
  let dx = headx - tailx in
  let dy = heady - taily in
  if abs dx >= 2 || abs dy >= 2 then
    rope.(i) <- (tailx + clamp1 dx, taily + clamp1 dy)

let get_visited rope motions =
  let n = Array.length rope - 1 in
  let rec move visited = function
    | dir :: rest ->
        move_head rope dir;
        for i = 1 to n do
          move_tail rope i
        done;
        let new_visited =
          if List.exists (fun pos -> pos = rope.(n)) visited then visited
          else rope.(n) :: visited
        in
        move new_visited rest
    | _ -> visited
  in
  move [] motions

let () =
  let start = (0, 0) in
  let rope1 = Array.make 2 start in
  let rope2 = Array.make 10 start in
  let motions = read_lines |> to_motions in
  get_visited rope1 motions |> List.length |> Printf.printf "Part 1: %d\n";
  get_visited rope2 motions |> List.length |> Printf.printf "Part 2: %d\n"
