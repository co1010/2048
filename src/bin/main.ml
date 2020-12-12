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

let margin = 20.
let backColor = Color.makeColor 119 110 101
let noTile = Color.makeColor 187 173 160
let colors = [|(Color.makeColor 238 228 218) ;
               (Color.makeColor 238 225 201) ;
               (Color.makeColor 243 178 122) ;
               (Color.makeColor 246 150 100) ;
               (Color.makeColor 247 124 95) ;
               (Color.makeColor 247 95 59) ;
               (Color.makeColor 237 208 115) ;
               (Color.makeColor 237 204 98) ;
               (Color.makeColor 237 201 80) ;
               (Color.makeColor 237 197 63) ;
               (Color.makeColor 237 194 46)|]

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


let printerArray a =
  for i = 1 to 4 do
    Lib.pfmt "[";
    for j = 1 to 4 do
      Lib.pfmt "%i; " a.(i).(j);
    done;
    Lib.pfmt "];\n";
  done;
  Lib.pfmt "\n";
  ()

(*rightCond : model -> model*)
let upCond model =
  let board = model.board in
  for row = 1 to 4 do
    for col = 1 to 4 do
      let tile = ref board.(row).(col) in
      if !tile = 0 then (
        for i = (col+1) to 5 do
          if !tile = -1  || board.(row).(i) = 0 then ()
          else(
            board.(row).(col) <- board.(row).(i);
            board.(row).(i) <- 0;
            tile := -1;
          )
        done;
      )
    done ;
  done ;
  model


(*downCond : model -> model*)
let downCond model =
  let board = model.board in
  for row = 4 downto 1 do
    for col = 4 downto 1 do
      let tile = ref board.(row).(col) in
      if !tile = 0 then (
        for i = (col-1) downto 0 do
          if !tile = -1  || board.(row).(i) = 0 then ()
          else(
            board.(row).(col) <- board.(row).(i);
            board.(row).(i) <- 0;
            tile := -1;
          )
        done;
      )
    done ;
  done ;
  model

(*leftCond : model -> model*)
let leftCond model =
  let board = model.board in
  for row = 1 to 4 do
    for col = 1 to 4 do
      let tile = ref board.(row).(col) in
      if !tile = 0 then (
        for i = (row+1) to 5 do
          if !tile = -1  || board.(i).(col) = 0 then ()
          else(
            board.(row).(col) <- board.(i).(col);
            board.(i).(col) <- 0;
            tile := -1;
          )
        done;
      )
    done ;
  done ;
  model

(*rightCond : model -> model*)
let rightCond model =
  let board = model.board in
  for row = 4 downto 1 do
    for col = 4 downto 1 do
      let tile = ref board.(row).(col) in
      if !tile = 0 then (
        for i = (row-1) downto 0 do
          if !tile = -1  || board.(i).(col) = 0 then ()
          else(
            board.(row).(col) <- board.(i).(col);
            board.(i).(col) <- 0;
            tile := -1;
          )
        done;
      )
    done ;
  done ;
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


let calculatePos i j =
  let x = 10.+.(float (i-1))*.(((displayWidth-.50.)/.4.)+.10.) in
  let y = 10.+.(float (j-1))*.(((displayHeight-.50.)/.4.)+.10.) in
  (x, y)

let getColor num =
  let rec loop num count =
    match num = 1 with
    | true -> count
    | false -> loop (num/2) (count+1) in
  let log = loop num 0 in
  colors.(log-1)


(* view : model -> Image.t *)
let view model =
  let background = ref (Image.rectangle displayWidth displayHeight backColor) in
  let tileback = ref (Image.rectangle 0. 0. noTile) in
  let text = ref "" in
  for row = 1 to 4 do
    for col = 1 to 4 do
      if (model.board.(row).(col)) = 0 then (
        tileback := Image.rectangle ((displayWidth-.50.)/.4.) ((displayHeight-.50.)/.4.) noTile;
        text := ""
      )
      else (
        tileback := Image.rectangle ((displayWidth-.50.)/.4.) ((displayHeight-.50.)/.4.) (getColor model.board.(row).(col));
        text := Printf.sprintf "%d" model.board.(row).(col);
      );
      let textImage = Image.text !text Color.black in
      let tile = Image.placeImage textImage ((((displayWidth-.50.)/.4.)/.2.)-.10., (((displayHeight-.50.)/.4.)/.2.)) !tileback in
      let newImage = Image.placeImage tile (calculatePos row col) !background in
      background := newImage
    done ;
  done ;
  !background



(* This function is called when a key is pressed, it's passed the model and the string of the key that was pressed *)
(* keyPress : model -> key -> model *)
let keyPress model key =
  let oldBoard = Array.make_matrix 6 6 0 in
  for row = 1 to 4 do
    for col = 1 to 4 do
      oldBoard.(row).(col) <- model.board.(row).(col)
    done ;
  done ;
  let modelAfterPress =
    match key with
    | "up" -> upArrow model
    | "down" -> downArrow model
    | "left" -> leftArrow model
    | "right" -> rightArrow model
    | _ -> model
  in
  let modelAfterCondense = condenseNumbers modelAfterPress key in
  let same = ref true in
  for row = 1 to 4 do
    for col = 1 to 4 do
      if !same && oldBoard.(row).(col) != modelAfterCondense.board.(row).(col) then
        same := false
    done ;
  done ;
  if !same then model else (
  let finalModel = populate2and4 modelAfterCondense in
  let isOver = checkGameOver finalModel in
  { board = finalModel.board
  ; isOver = isOver
  } )



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
  match model.isOver = Loss with
  | true -> let text = Image.text "You Lose." Color.red in
            Image.placeImage text ((displayWidth/.2.)-.40., (displayHeight/.2.)-.10.) boardImage
  | false -> let text = Image.text "You Win!" Color.red in
            Image.placeImage text ((displayWidth/.2.)-.40., (displayHeight/.2.)-.10.) boardImage

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
