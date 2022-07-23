import { Effect, Random, Show } from "@effect-ts/core";
import { Probability } from "./Probability";

export type Gene = unknown;

export interface Gene$<A extends Gene> {
  readonly Show: Show.Show<A>;
  readonly random: Effect.RIO<Random.HasRandom, A>;
  readonly mutate: (
    probability: Probability
  ) => (gene: A) => Effect.RIO<Random.HasRandom, A>;
}

const getShow = <A extends Gene>(): Show.Show<A> =>
  Show.makeShow((gene) => `${gene}`);

export const $gene = { getShow };
