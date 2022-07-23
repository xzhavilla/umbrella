import { Equal } from "@effect-ts/core";
import { pipe } from "@effect-ts/system/Function";
import { $maze, Maze } from "./Maze";
import { $shift, Shift } from "./Shift";

type Row = number;
type Column = number;
export type Position = Readonly<[Row, Column]>;

const _Equal: Equal.Equal<Position> = Equal.array(Equal.number);

const start: Position = [0, 0];

const end = (maze: Maze): Position => [
  $maze.height(maze) - 1,
  $maze.width(maze) - 1,
];

const next =
  (shift: Shift) =>
  ([row, column]: Position): Position =>
    pipe(
      shift,
      $shift.match(
        () => [row - 1, column],
        () => [row + 1, column],
        () => [row, column - 1],
        () => [row, column + 1]
      )
    );

const isInMaze =
  (maze: Maze) =>
  ([row, column]: Position) =>
    row >= 0 &&
    column >= 0 &&
    row < $maze.height(maze) &&
    column < $maze.width(maze) &&
    $maze.isMazePart(maze[row][column]);

const isEnd = (maze: Maze) => (position: Position) =>
  _Equal.equals(end(maze), position);

export const $position = { Equal: _Equal, start, end, next, isInMaze, isEnd };
