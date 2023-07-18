let read_stdin () =
  let line = read_line () in
  let n = String.length line in
  let matrix = Array.make_matrix n n 0 in
  let rec fill_array line i =
    String.iteri
      (fun j ch -> matrix.(i).(j) <- int_of_string (Char.escaped ch))
      line;
    fill_array (read_line ()) (i + 1)
  in
  try fill_array line 0 with End_of_file -> (matrix, n)

let right_iterator i j = (i, j + 1)
let up_iterator i j = (i + 1, j)
let left_iterator i j = (i, j - 1)
let down_iterator i j = (i - 1, j)

let get_visibility forest n =
  let visible = Array.make_matrix n n false in
  let rec fill_vis max_h (i, j) nextij =
    try
      let h = forest.(i).(j) in
      (* Printf.printf "(%d,%d) %d\n" i j h; *)
      if h > max_h then (
        visible.(i).(j) <- true;
        fill_vis h (nextij i j) nextij)
      else fill_vis max_h (nextij i j) nextij
    with Invalid_argument _ -> ()
  in
  for x = 0 to n - 1 do
    fill_vis min_int (x, 0) right_iterator;
    fill_vis min_int (0, x) up_iterator;
    fill_vis min_int (x, n - 1) left_iterator;
    fill_vis min_int (n - 1, x) down_iterator
  done;
  visible

let get_scenic_scores forest n =
  let scores = Array.make_matrix n n 0 in
  let rec calc_score h (i, j) nextij =
    try if h > forest.(i).(j) then 1 + calc_score h (nextij i j) nextij else 1
    with Invalid_argument _ -> 0
  in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let right = calc_score forest.(i).(j) (i, j + 1) right_iterator in
      let up = calc_score forest.(i).(j) (i + 1, j) up_iterator in
      let left = calc_score forest.(i).(j) (i, j - 1) left_iterator in
      let down = calc_score forest.(i).(j) (i - 1, j) down_iterator in
      scores.(i).(j) <- right * up * left * down
    done
  done;
  scores

let matrix_fold reducer init matrix =
  Array.fold_left (fun acc arr -> Array.fold_left reducer acc arr) init matrix

let () =
  let forest, n = read_stdin () in
  let visible = get_visibility forest n in
  let sum = matrix_fold (fun sum b -> if b then sum + 1 else sum) 0 visible in
  Printf.printf "Part 1: %d\n" sum;
  let scenic_scores = get_scenic_scores forest n in
  let max_score =
    matrix_fold (fun curr score -> max curr score) min_int scenic_scores
  in
  Printf.printf "Part 2: %d\n" max_score
