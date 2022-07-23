import { Ord, Random } from "@effect-ts/core";
import * as Bounded from "@effect-ts/core/Bounded";

export type Probability = number;

const _Ord: Ord.Ord<Probability> = Ord.number;

const _Bounded: Bounded.Bounded<Probability> = Bounded.makeBounded<Probability>(
  _Ord.compare,
  1,
  0
);

const next = Random.nextRange(_Bounded.bottom, _Bounded.top);

export const $probability = { Bounded: _Bounded, Ord: _Ord, next };
