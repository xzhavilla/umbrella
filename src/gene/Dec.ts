import { Effect, Ord, Random, Show } from "@effect-ts/core";
import * as Bounded from "@effect-ts/core/Bounded";
import { pipe } from "@effect-ts/system/Function";
import { Gene, Gene$ } from "../Gene";
import { $probability } from "../Probability";

export type Dec = Gene &
  (typeof bottom | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | typeof top);

const _Ord: Ord.Ord<Dec> = Ord.number;

const bottom = 0;
const top = 9;
const _Bounded: Bounded.Bounded<Dec> = Bounded.makeBounded<Dec>(
  _Ord.compare,
  top,
  bottom
);

const _Show: Show.Show<Dec> = Show.number;

const _gene: Gene$<Dec> = {
  Show: _Show,
  random: pipe(
    Random.nextIntBetween(_Bounded.bottom, _Bounded.top),
    Effect.map((gene) => gene as Dec)
  ),
  mutate: (probability) => (gene) =>
    pipe(
      $probability.next,
      Effect.chain((n) =>
        n < probability ? $dec.random : Effect.succeed(gene)
      )
    ),
};

export const $dec = { ..._gene, Bounded: _Bounded, Ord: _Ord };
