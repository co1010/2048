# 2048

### Model
The model needs to have an int array array of size 6x6 to include a buffer ring around the actual 4x4 game board to avoid index errors. The model also needs to have information about whether the player has won, lost, or if the game is still ongoing.

### View function
Takes a model and outputs an image based on the model. If the game is over display a win or loss message.

### Update function
Takes a model and outputs a new model. Two step process.
- **addMatching() : model -> model**
  - Adds and combines matching numbers that will collide with button press
- **condenseNumbers() : model -> model**
  - Moves numbers in the direction of the button press.
  
### Other functions
#### addNewTile() : model -> model
Adds a 2 or 4 tile randomly to an empty tile. Assume there will be an empty tile.

#### checkGameEnd() : model -> int
Outputs 0 if game is not over, 1 if player loses, 2 if player wins. Might switch this to a type later.
Win if a tile is 2048.
Loss if there are no 0 tiles on game board AND there are no matching adjacent tiles.
Game not over otherwise.
