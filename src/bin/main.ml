(* file: main.ml
 * author: Bob Muller
 *
 * CS1103 Computer Science 1 Honors
 *
 * Conway's game of life.
 *
 * To run:
 *
 * > cd src
 * > dune exec bin/main.exe ../examples/one.txt
 *)
let displayWidth = 800.
let displayHeight = displayWidth

let cubeColor = Color.orange
let noCube = Color.white

type state = Paused | Running

(* toggle : state -> state *)
let toggle state =
  match state with
  | Paused -> Running
  | Running -> Paused

type colony = (int array) array
type count = (int array) array

type model = { state : state
             ; n : int
             ; colony : colony
             ; count : count
             }

type model2048 = { board : int array array
                 ; isWin : bool
                 ; isLoss : bool
                 }

let getFile () =
  Lib.fmt "%s/%s" (Unix.getcwd()) Sys.argv.(1)

(* readColony : string -> colony *)
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
  let colony = Array.make_matrix (rows + 2) (cols + 2) 0
  in
  for row = 1 to rows do
    for col = 1 to cols do
      colony.(row).(col) <- if linesArray.(row - 1).[col - 1] = '*' then 1 else 0
    done
  done;
  colony

(* touchPad : model x y event -> model
*)
let touchPad model x y event =
  let newState = toggle model.state in
  { state = newState
  ; n = model.n
  ; colony = model.colony
  ; count = model.count
  }

(* sumNeighbors : colony i j -> int *)
let sumNeighbors colony i j =
  let sum = colony.(i-1).(j-1) + colony.(i-1).(j) + colony.(i-1).(j+1) + colony.(i).(j-1) + colony.(i).(j+1) + colony.(i+1).(j-1) + colony.(i+1).(j) + colony.(i+1).(j+1)
  in
  sum

(* update : model -> model *)
let update model =
  if model.state = Paused then model else
  let colony = model.colony in
  let rows = Array.length colony in
  let cols = Array.length colony.(0) in
  let newCount = Array.make_matrix rows cols 0 in
  let newColony = Array.make_matrix rows cols 0 in
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do
      newCount.(i).(j) <- sumNeighbors colony i j
    done;
  done;
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do
      if colony.(i).(j) = 1 then
        if newCount.(i).(j) = 2 || newCount.(i).(j) = 3 then
          newColony.(i).(j) <- 1
        else ()
      else
        if newCount.(i).(j) = 3 then
          newColony.(i).(j) <- 1
        else ()
    done;
  done;
  { state = model.state
  ; n = model.n - 1
  ; colony = newColony
  ; count = newCount
  }



(* view : model -> Image.t *)
let view model =
  let colony = model.colony in
  let rows = Array.length colony in
  let cols = Array.length colony.(0) in
  let width = displayWidth /. (float (cols - 2)) in
  let height = displayHeight /. (float (rows - 2)) in
  let image = ref (Image.rectangle displayWidth displayHeight Color.white) in
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do
      let color = if colony.(i).(j) = 1 then cubeColor else noCube in
      let cube = Image.rectangle width height color in
      let x = float (i - 1) *. width in
      let y = float (j - 1) *. height in
      image.contents <- Image.placeImage cube (x, y) !image
    done;
  done;
  !image

(* finished : model -> bool *)
let finished model =
  if model.n = 0 then true else false

(* makeWorkSpace : colony -> colony *)
let makeWorkSpace colony =
  let rows = Array.length colony in
  let cols = Array.length colony.(0)
  in
  Array.make_matrix rows cols 0

(* makeModel : int -> string -> model *)
let makeModel n filename =
  let colony = readColony filename in
  let workSpace = makeWorkSpace colony
  in
  { state = Paused
  ; n
  ; colony
  ; count = workSpace
  }


(*random2and4 : unit -> int*)
(*returns either a 0, 2, or 4 to be added to a empty square,
  not implemented yet b/c I want to figure out the randomness factors for
  each possible return *)
let random2and4 = 0

(*populateBoard : unit -> model2048.board*)
let initialBoard =
  let board = Array.make_matrix 6 6 0 in
    for i = 1 to 5 do (*can make this more general if needed, but just used fix numbers since the board dimensions are known*)
      for j = 1 to 5 do
        let random = random2and4 in
        board.(i).(j) <- random;
      done ;
    done
  ; board

(*makeModel2048 : unit -> model2048*)
let makeModel2048 =
  let initialboard = initialBoard in
  let initWin = false in
  let initLoss = false in
  { board = initialboard
  ; isWin = initWin
  ; isLoss = initLoss
  }


let go n filename =
  let initialModel = makeModel2048
  in
  Animate.start initialModel
                 ~name: "Conway's Life Automata"
                 ~width: displayWidth
                 ~height: displayHeight
                 ~view: view
                 ~onTick: update
                 ~onMouse: touchPad
                 ~stopWhen: finished
                 ~rate: 0.5

let _ = go 1000 (getFile ())
