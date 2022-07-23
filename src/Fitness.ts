import { Effect, Ord, Show } from "@effect-ts/core";
import * as Bounded from "@effect-ts/core/Bounded";
import { $bounded } from "./Bounded";
import { Chromosome } from "./Chromosome";
import { Gene } from "./Gene";

export type Fitness = number;

export interface FitnessFunction<A extends Gene> {
  (chromosome: Chromosome<A>): Effect.Effect<any, Error, Fitness>;
}

const _Ord: Ord.Ord<Fitness> = Ord.number;

const _Bounded: Bounded.Bounded<Fitness> = Bounded.makeBounded(
  _Ord.compare,
  Infinity,
  -Infinity
);

const _Show: Show.Show<Fitness> = Show.makeShow((fitness) =>
  fitness.toFixed(2)
);

const isMax = $bounded.isTop(_Bounded);

export const $fitness = { Bounded: _Bounded, Ord: _Ord, Show: _Show, isMax };
