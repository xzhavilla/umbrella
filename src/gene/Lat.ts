import { Effect, Ord, Random, Show } from "@effect-ts/core";
import * as Bounded from "@effect-ts/core/Bounded";
import { pipe } from "@effect-ts/system/Function";
import { Gene, Gene$ } from "../Gene";
import { $probability } from "../Probability";

export type Lat = Gene &
  (
    | typeof bottom
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | typeof top
  );

const _Ord: Ord.Ord<Lat> = Ord.string;

const bottom = "a";
const top = "z";
const _Bounded: Bounded.Bounded<Lat> = Bounded.makeBounded<Lat>(
  _Ord.compare,
  top,
  bottom
);

const _Show: Show.Show<Lat> = Show.makeShow((gene) => gene.toUpperCase());

const _gene: Gene$<Lat> = {
  Show: _Show,
  random: pipe(
    Random.nextIntBetween(
      _Bounded.bottom.charCodeAt(0),
      _Bounded.top.charCodeAt(0)
    ),
    Effect.map((charCode) => String.fromCharCode(charCode) as Lat)
  ),
  mutate: (probability) => (gene) =>
    pipe(
      $probability.next,
      Effect.chain((n) =>
        n < probability ? $lat.random : Effect.succeed(gene)
      )
    ),
};

export const $lat = { ..._gene, Bounded: _Bounded, Ord: _Ord };
