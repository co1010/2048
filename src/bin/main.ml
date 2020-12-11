(* file: main.ml
 * author: Colin Clarke and Bergen Nelson
 *
 * CS1103 Computer Science 1 Honors
 *
 * Game of 2048.
 *
 * To run:
 *
 * > cd src
 * > dune exec bin/main.exe
 *)

let displayWidth = 800.
let displayHeight = displayWidth

let cubeColor = Color.orange
let noCube = Color.white

type gameOver = | False
                | Win
                | Loss

type input = | UpArrow
             | DownArrow
             | LeftArrow
             | RightArrow
             | None

type model = { board : int array array
             ; isOver : gameOver
             }


(*either2or4 : unit -> int*)
(*returns either a 0, 2, or 4 to be added to a empty square,
  not implemented yet b/c I want to figure out the randomness factors for
  each possible return *)
let either2or4 () =
  Random.self_init ();
  let randInt = Random.int 10 in
  match randInt = 0 with
  | true -> 4
  | false -> 2

(*populate2and4 : model -> model *)
let populate2and4 model =
  Random.self_init ();
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
  model

(* Checks if the tiles adjacent to board.(i).(j) match the value of board.(i).(j). *)
(* Returns true if any of them do, false if none of them match. *)
(* checkAdjTilesMatch : int array array -> int -> int -> bool *)
let checkAdjTilesMatch board i j = 
  let equal = ref false in
  let tile = board.(i).(j) in
  if      tile = board.(i+1).(j) then equal := true
  else if tile = board.(i-1).(j) then equal := true
  else if tile = board.(i).(j+1) then equal := true
  else if tile = board.(i).(j-1) then equal := true;
  !equal


(* checkGameOver : model -> gameOver *)
let checkGameOver model =
  let board = model.board in
  let gameContinue = ref false in
  let win = ref false in
  for row = 1 to 4 do
    for col = 1 to 4 do
      if not (!gameContinue || !win) then (
        let tile = board.(row).(col) in
        if tile = 0 then gameContinue := true
        else if tile = 2048 then win := true
        else if checkAdjTilesMatch board row col then gameContinue := true
      )
    done ;
  done ;
  if !gameContinue then False
  else if !win then Win
  else Loss


(*rightCond : model -> model*)
let upCond model =
  for row = 1 to 4 do (*maybe we only have to do the first three rows, anthing in the bottom row will be moved up anyways*)
      for col = 1 to 4 do
        let rowBelow = row - 1 in
        if model.board.(row).(col) = 0 then
          begin
            model.board.(row).(col) <- model.board.(rowBelow).(col);
            model.board.(rowBelow).(col) <- 0;
          end
      done;
    done;
  model

(*downCond : model -> model*)
let downCond model =
  for row = 1 to 4 do (*maybe we only have to do the first three rows, anthing in the top row will be down up anyways*)
      for col = 1 to 4 do
        let invRow = 5 - row in
        let rowAbove = invRow - 1 in
        if model.board.(invRow).(col) = 0 then
          begin
            model.board.(invRow).(col) <- model.board.(rowAbove).(col);
            model.board.(rowAbove).(col) <- 0;
          end
      done;
    done;
  model

(*leftCond : model -> model*)
let leftCond model =
    for row = 1 to 4 do
        for col = 1 to 4 do
          let colRight = col + 1 in
          if model.board.(row).(col) = 0 then
            begin
              model.board.(row).(col) <- model.board.(row).(colRight);
              model.board.(row).(colRight) <- 0;
            end
        done;
      done;
    model

(*rightCond : model -> model*)
let rightCond model =
for row = 1 to 4 do
    for col = 1 to 4 do
      let invCol = 5 - col in
      let colLeft = invCol - 1 in
      if model.board.(row).(invCol) = 0 then
        begin
          model.board.(row).(invCol) <- model.board.(row).(colLeft);
          model.board.(row).(colLeft) <- 0;
        end
    done;
  done;
model


(*condenseNumbers : model -> model*)
let condenseNumbers model string =
  match string with
  | "up" -> upCond model
  | "down" -> downCond model
  | "left" -> leftCond model
  | "right" -> rightCond model
  | _ -> model



(*upArrow : model -> model*)
let upArrow model =
  let board = model.board in
  for row = 1 to 4 do
    for col = 1 to 4 do
      let tile = ref board.(row).(col) in
      if !tile != 0 then (
        for i = (col+1) to 5 do
          if !tile = -1 || board.(row).(i) = 0 then ()
          else if !tile = board.(row).(i) then (
            board.(row).(col) <- !tile*2;
            board.(row).(i) <- 0;
            tile := -1;
          )
          else (
            tile := -1;
          )
        done;
      )
    done ;
  done ;
  model


(*downArrow : model -> model*)
let downArrow model =
  let board = model.board in
  for row = 4 downto 1 do
    for col = 4 downto 1 do
      let tile = ref board.(row).(col) in
      if !tile != 0 then (
        for i = (col-1) downto 0 do
          if !tile = -1 || board.(row).(i) = 0 then ()
          else if !tile = board.(row).(i) then (
            board.(row).(col) <- !tile*2;
            board.(row).(i) <- 0;
            tile := -1;
          )
          else (
            tile := -1;
          )
        done;
      )
    done ;
  done ;
  model

(*leftArrow : model -> model*)
let leftArrow model = 
  let board = model.board in
  for row = 1 to 4 do
    for col = 1 to 4 do
      let tile = ref board.(row).(col) in
      if !tile != 0 then (
        for i = (row+1) to 5 do
          if !tile = -1 || board.(i).(col) = 0 then ()
          else if !tile = board.(i).(col) then (
            board.(row).(col) <- !tile*2;
            board.(i).(col) <- 0;
            tile := -1;
          )
          else (
            tile := -1;
          )
        done;
      )
    done ;
  done ;
  model

(*rightArrow : model -> model*)
let rightArrow model =
  let board = model.board in
  for row = 4 downto 1 do
    for col = 4 downto 1 do
      let tile = ref board.(row).(col) in
      if !tile != 0 then (
        for i = (row-1) downto 0 do
          if !tile = -1 || board.(i).(col) = 0 then ()
          else if !tile = board.(i).(col) then (
            board.(row).(col) <- !tile*2;
            board.(i).(col) <- 0;
            tile := -1;
          )
          else (
            tile := -1;
          )
        done;
      )
    done ;
  done ;
  model



(* view : model -> Image.t *)
let view model = 
  let background = ref (Image.rectangle displayWidth displayHeight noCube) in
  let tileback = ref (Image.rectangle 0. 0. cubeColor) in
  for row = 1 to 4 do
    for col = 1 to 4 do
      if (model.board.(row).(col)) = 0 then 
        tileback := Image.rectangle (displayWidth/.4.) (displayHeight/.4.) noCube
      else
        tileback := Image.rectangle (displayWidth/.4.) (displayHeight/.4.) cubeColor;
      let text = Printf.sprintf "%d" model.board.(row).(col) in
      let textImage = Image.text text Color.black in
      let tile = Image.placeImage textImage (((displayWidth/.4.)/.2.), ((displayHeight/.4.)/.2.)) !tileback in
      let newImage = Image.placeImage tile ((float (row-1))*.(displayWidth/.4.), (float (col-1))*.(displayHeight/.4.)) !background in
      background := newImage
    done ;
  done ;
  !background



(* This function is called when a key is pressed, it's passed the model and the string of the key that was pressed *)
(* keyPress : model -> key -> model *)
let keyPress model key = 
  let () = Format.printf "%s\n" key in
  let modelAfterPress = 
    match key with
    | "up" -> upArrow model
    | "down" -> downArrow model
    | "left" -> leftArrow model
    | "right" -> rightArrow model
    | _ -> model
  in
  let finalModel = populate2and4 modelAfterPress in
  let isOver = checkGameOver finalModel in
  { board = finalModel.board
  ; isOver = isOver
  }



let blankModel =
  let initBoard = Array.make_matrix 6 6 0 in
  { board = initBoard
  ; isOver = False
  }

(*makeModel2048 : unit -> model2048*)
let makeModel2048 =
  let blankModel = blankModel in
  populate2and4 blankModel


(* finished : model -> bool *)
let finished model = if model.isOver = Loss || model.isOver = Win then true else false

let viewLast model = 
  let boardImage = view model in
  let loseText = Image.text "You Lose" Color.red
  in
  Image.placeImage loseText ((displayWidth/.2.)-.30., (displayHeight/.2.)) boardImage

(*Things that can go wrong
  - index out of bounds with any for loops
  - I tried to adjust the for loops to look at the middle fourxfour square
    by saying for i = 1 to 4 do, in order to adjust for fact that array matrixes
  are indexed from 0 to 5, but htat may be wrong*)


let go n =
  let initialModel = makeModel2048
  in
  Animate.start initialModel
                 ~name: "2048"
                 ~width: displayWidth
                 ~height: displayHeight
                 ~view: view
                 ~onKeyPress: keyPress
                 ~stopWhen: finished
                 ~viewLast: viewLast

let _ = go 1000
