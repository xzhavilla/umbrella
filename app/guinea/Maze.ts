import { Array } from "@effect-ts/core";

export type Maze = Array.Array<string>;

const MAZE = "█";

const width = (track: Maze) => track[0].length;

const height = (track: Maze) => track.length;

const isMazePart = (s: string) => MAZE === s;

export const $maze = { width, height, isMazePart };
