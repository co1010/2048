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

(* type state = Paused | Running

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
             } *)

type input = | UpArrow
             | DownArrow
             | LeftArrow
             | RightArrow
             | None

type model2048 = { board : int array array
                 ; isWin : bool
                 ; isLoss : bool
                 ; lastInput : input
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
(* let touchPad model x y event =
  let newState = toggle model.state in
  { state = newState
  ; n = model.n
  ; colony = model.colony
  ; count = model.count
  } *)

(* sumNeighbors : colony i j -> int *)
let sumNeighbors colony i j =
  let sum = colony.(i-1).(j-1) + colony.(i-1).(j) + colony.(i-1).(j+1) + colony.(i).(j-1) + colony.(i).(j+1) + colony.(i+1).(j-1) + colony.(i+1).(j) + colony.(i+1).(j+1)
  in
  sum

(* update : model -> model *)
(* let update model =
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
  } *)



(* view : model -> Image.t *)
(* let view model =
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
  !image *)

(* finished : model -> bool *)
(* let finished model =
  if model.n = 0 then true else false *)

(* makeWorkSpace : colony -> colony *)
let makeWorkSpace colony =
  let rows = Array.length colony in
  let cols = Array.length colony.(0)
  in
  Array.make_matrix rows cols 0

(* makeModel : int -> string -> model *)
(* let makeModel n filename =
  let colony = readColony filename in
  let workSpace = makeWorkSpace colony
  in
  { state = Paused
  ; n
  ; colony
  ; count = workSpace
  } *)

    (*++++++++++++++++++++++++++++++++++++++++++ Our Program ++++++++++++++++++++++++++++*)

(*NB might be an error in the big funcion below where with index out of bounds
  *)
(*upArrow : model -> model*)
let upArrow model =
  for row = 1 to 4 do
    for col = 1 to 4 do
      if model.board.(row).(col) = model.board.(row - 1).(col) then
        begin
          (model.board.(row).(col) <- model.board.(row).(col) * 2;)
        end
      else ();
      if model.board.(row).(col) = 0 then
        begin
         if model.board.(row - 1).(col) = model.board.(row - 2).(col) then
           (model.board.(row - 1).(col) <- model.board.(row - 1).(col) * 2;)
         else
           (model.board.(row).(col) <- model.board.(row - 1).(col);)
       end
    done ;
  done ;
  model


(*downArrow : model -> model*)
let downArrow model = model

(*leftArrow : model -> model*)
let leftArrow model = model

(*rightArrow : model -> model*)
let rightArrow model = model


let addMatching model =
  match model.lastInput with
  | UpArrow -> upArrow model
  | DownArrow -> downArrow model
  | LeftArrow -> leftArrow model
  | RightArrow -> rightArrow model
  | None -> model

(*needs implementing*)
(*condenseNumbers : model -> model*)
let condenseNumbers model = model

let update model =
  let newModel1 = addMatching model in
  let newModel2 = condenseNumbers newModel1
  in
  newModel2





(*either2or4 : unit -> int*)
(*returns either a 0, 2, or 4 to be added to a empty square,
  not implemented yet b/c I want to figure out the randomness factors for
  each possible return *)
let either2or4 () =
  let randInt = Random.int 10 in
  match randInt <= 9 with
  | true -> 2
  | false -> 4

(*populate2and4 : model2048.board -> unit*)
let populate2and4 model =
  let zeroLocalsRef = ref [||] in
  for row = 1 to 4 do
    for col = 1 to 4 do
      if model.board.(row).(col) = 0 then
        (zeroLocalsRef := Array.append !zeroLocalsRef [|(row, col)|];)
    done ;
  done ;
  let zeroLocals = !zeroLocalsRef in
  let zeroCount = Array.length zeroLocals in
  let whichSquare = Random.int zeroCount in
  let whichNumber = either2or4 () in
  let xandy = zeroLocals.(whichSquare) in
  let x = fst(xandy) in
  let y = snd(xandy) in
  model.board.(x).(y) <- whichNumber;
  ()

let blankModel =
  let initBoard = Array.make_matrix 6 6 0 in
  let initWin = false in
  let initLoss = false in
  { board = initBoard
  ; isWin = initWin
  ; isLoss = initLoss
  ; lastInput = None
  }

(*makeModel2048 : unit -> model2048*)
let makeModel2048 =
  let blankModel = blankModel in
  populate2and4 blankModel


(*Things that can go wrong
  - index out of bounds with any for loops
  - I tried to adjust the for loops to look at the middle fourxfour square
    by saying for i = 1 to 4 do, in order to adjust for fact that array matrixes
  are indexed from 0 to 5, but htat may be wrong*)


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
