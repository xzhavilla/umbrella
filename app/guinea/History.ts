import { Array, pipe } from "@effect-ts/core";
import { $maze, Maze } from "./Maze";
import { Position } from "./Position";

export type History = Array.Array<boolean>;

const empty: History = [];

const visit =
  (maze: Maze, [row, column]: Position) =>
  (history: History): History =>
    pipe(
      history,
      Array.unsafeUpdateAt(row * $maze.width(maze) + column, true as boolean)
    );

const hasVisited =
  (maze: Maze, [row, column]: Position) =>
  (history: History) =>
    history[row * $maze.width(maze) + column];

export const $history = { empty, visit, hasVisited };
