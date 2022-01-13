# Route Planning for Metro Rail Network in Prolog

This is a Prolog program that implements Iterative Deepening Depth First Search (IDDFS) to identify the shortest 5 paths between any two metro stations in the Delhi metro rail network.

## File Structure

- `db.pl` - Contains database of stations/stops in each color-coded metro line.
- `main.pl` - Contains route planning logic

## Execution

- Load `main.pl`
- The predicate to query is `paths(X, Y, LL)` where X and Y are the start and end stations between which you'd like to find paths and LL is a list of the shortest 5 paths the program can find between the two stations. 

For a list of accepted stations, see `db.pl` - you can use any station name that is present in a color-coded metro line. Example - `paths(haiderpur, madipur, LL)` will return the shortest 5 paths between `haiderpur` and `madipur` in `LL`.