import { Effect, Ord, Random, Show } from "@effect-ts/core";
import * as Bounded from "@effect-ts/core/Bounded";
import { pipe } from "@effect-ts/system/Function";
import { Gene, Gene$ } from "../Gene";
import { $probability } from "../Probability";

export type Qua = Gene &
  (typeof bottom | 0b01|0b10 | typeof top);

const _Ord: Ord.Ord<Qua> = Ord.number;

const bottom = 0b00;
const top = 0b11;
const _Bounded: Bounded.Bounded<Qua> = Bounded.makeBounded<Qua>(
  _Ord.compare,
  top,
  bottom
);

const _Show: Show.Show<Qua> = Show.number;

const _gene: Gene$<Qua> = {
  Show: _Show,
  random: pipe(
    Random.nextIntBetween(_Bounded.bottom, _Bounded.top),
    Effect.map((gene) => gene as Qua)
  ),
  mutate: (probability) => (gene) =>
    pipe(
      $probability.next,
      Effect.chain((n) =>
        n < probability ? $qua.random : Effect.succeed(gene)
      )
    ),
};

export const $qua = { ..._gene, Bounded: _Bounded, Ord: _Ord };
