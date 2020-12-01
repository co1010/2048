(* readColony : string -> colony
 *)
let readColony file =
  let inch = open_in file in
  let rec repeat lines =
    try
      let line = input_line inch
      in
      repeat (line :: lines)
    with
      End_of_file -> close_in inch;
                     lines in
  let linesArray = Array.of_list (List.rev (repeat [])) in
  let rows = Array.length linesArray in
  let cols = String.length linesArray.(0) in
  let colony = Array.make_matrix (rows + 2) (cols + 2) false
  in
  for row = 1 to rows do
    for col = 1 to cols do
      colony.(row).(col) <- linesArray.(row - 1).[col - 1] = '*'
    done
  done;
  colony

