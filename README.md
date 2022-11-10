# Ultimate-Tic-Tac-Toe
---

## Team Members

- Nishanth Rachakonda
- Swapnil Rustagi
- Sruthi Praveen Kumar Geetha
- Abhinava Arasada

---

## Problem Statement / Synopsis

The main objective of the project is to implement a two-player (over the network) [ultimate tic-tac-toe game](https://en.wikipedia.org/wiki/Ultimate_tic-tac-toe) using brick library and other Haskell libraries. Two people can connect to a common server to play against each other.

---

## Goals

### To create:

#### Client side application: 

1. Connects to the server so that user can play against someone else
2. Renders the game in the terminal (using brick library) and allows the user to input the next move (mouse click to select the position on the board)
3. Validates each user move and alerts the user if the move is invalid.  (based on valid moves obtained from server)
4. (Optional) Fetches saved games history from server and allows viewing the game move-by-move
5. (Optional) Have a separate timed game mode where we maintain a timer to calculate the time taken by player for each move and limit the maximum amount of time taken by a player.

#### Server side application:

1. Matches two clients with each other for playing
2. Maintains game state
3. Computes valid positions for next move and sends it along with current game state to the client whose turn is next
4. Handles client disconnects: declare a game lost by a client if the client is unreachable
5. (Optional) Saved games history

Goals marked (Optional) will be implemented only if there is sufficient time. 
