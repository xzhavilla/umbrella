import { Ord } from "@effect-ts/core";
import * as Bounded from "@effect-ts/core/Bounded";

const isTop =
  <A>(B: Bounded.Bounded<A>) =>
  (a: A) =>
    Ord.getEqual<A>(B).equals(a, B.top);

const isBottom =
  <A>(B: Bounded.Bounded<A>) =>
  (a: A) =>
    Ord.getEqual<A>(B).equals(a, B.bottom);

export const $bounded = { isTop, isBottom };
