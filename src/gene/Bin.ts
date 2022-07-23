import { Effect, Ord, Random, Show } from "@effect-ts/core";
import * as Bounded from "@effect-ts/core/Bounded";
import { pipe } from "@effect-ts/system/Function";
import { $bounded } from "../Bounded";
import { Gene, Gene$ } from "../Gene";
import { $probability } from "../Probability";

export type Bin = Gene & (typeof bottom | typeof top);

const _Ord: Ord.Ord<Bin> = Ord.number;

const bottom = 0b0;
const top = 0b1;
const _Bounded: Bounded.Bounded<Bin> = Bounded.makeBounded(
  _Ord.compare,
  top,
  bottom
);

const _Show: Show.Show<Bin> = Show.makeShow((gene) =>
  _Bounded.bottom === gene ? "." : "#"
);

const other = (gene: Bin) => Math.abs(gene - _Bounded.top) as Bin;

const _gene: Gene$<Bin> = {
  Show: _Show,
  random: pipe(
    Random.nextIntBetween(_Bounded.bottom, _Bounded.top),
    Effect.map((n) => n as Bin)
  ),
  mutate: (probability) => (gene) =>
    pipe(
      $probability.next,
      Effect.map((n) => (n < probability ? other(gene) : gene))
    ),
};

const isZero = $bounded.isBottom(_Bounded);

const isOne = $bounded.isTop(_Bounded);

export const $bin = {
  ..._gene,
  Bounded: _Bounded,
  Ord: _Ord,
  other,
  isZero,
  isOne,
};
