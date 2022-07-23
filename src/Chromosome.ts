import { Array, Effect, Has, Random, Show } from "@effect-ts/core";
import { Gene, Gene$ } from "./Gene";
import { Probability } from "./Probability";

export type Chromosome<A extends Gene> = Array.Array<A>;

export interface Chromosome$<A extends Gene> {
  readonly getShow: (S: Show.Show<A>) => Show.Show<Chromosome<A>>;
  readonly ofSize: (
    size: number
  ) => Effect.RIO<
    Random.HasRandom & Has.Has<{ _: <A extends Gene>() => Gene$<A> }>,
    Chromosome<A>
  >;
  readonly crossOver: (
    a: Chromosome<A>,
    b: Chromosome<A>
  ) => Effect.RIO<Random.HasRandom, Readonly<[Chromosome<A>, Chromosome<A>]>>;
  readonly mutate: (
    probability: Probability
  ) => (
    chromosome: Chromosome<A>
  ) => Effect.RIO<
    Random.HasRandom & Has.Has<{ _: <A extends Gene>() => Gene$<A> }>,
    Chromosome<A>
  >;
}
