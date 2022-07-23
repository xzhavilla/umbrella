import { Array, Show } from "@effect-ts/core";
import { pipe } from "@effect-ts/system/Function";
import { Bin } from "../../src/gene/Bin";

export type Shift = typeof UP | typeof DOWN | typeof LEFT | typeof RIGHT;

const UP = 0b00;
const DOWN = 0b01;
const LEFT = 0b10;
const RIGHT = 0b11;

const match =
  <A, B, C, D>(
    onUp: () => A,
    onDown: () => B,
    onLeft: () => C,
    onRight: () => D
  ) =>
  (shift: Shift) => {
    switch (shift) {
      case UP:
        return onUp();
      case DOWN:
        return onDown();
      case LEFT:
        return onLeft();
      case RIGHT:
        return onRight();
    }
  };

const _Show: Show.Show<Shift> = Show.makeShow(
  match(
    () => "↑",
    () => "↓",
    () => "←",
    () => "→"
  )
);

const fromBins = (bins: Array.Array<Bin>): Array.Array<Shift> =>
  pipe(
    bins,
    Array.chunksOf(2),
    Array.map(([b1, b0]) => ((b0 ?? 0) + 2 * (b1 ?? 0)) as Shift)
  );

export const $shift = { Show: _Show, fromBins, match };
