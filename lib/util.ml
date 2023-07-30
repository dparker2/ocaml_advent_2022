let list_of_input () =
  let rec f list = try f (read_line () :: list) with End_of_file -> list in
  f []

let list_of_str string =
  string |> String.split_on_char ',' |> List.map String.trim

let index_of f l =
  let i = ref (-1) in
  List.iteri (fun idx e -> if f e then i := idx) l;
  !i
