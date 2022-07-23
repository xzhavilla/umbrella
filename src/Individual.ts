import { Effect, Ord, Show } from "@effect-ts/core";
import { pipe } from "@effect-ts/system/Function";
import { Chromosome } from "./Chromosome";
import { HasChromosome } from "./Environment";
import { $fitness, Fitness, FitnessFunction } from "./Fitness";
import { Gene } from "./Gene";

export interface Individual<A extends Gene> {
  readonly chromosome: Chromosome<A>;
  readonly fitness: Fitness;
}

const getOrd = <A extends Gene>(): Ord.Ord<Individual<A>> =>
  pipe(
    $fitness.Ord,
    Ord.contramap(({ fitness }) => fitness)
  );

const getShow = <A extends Gene>(
  S: Show.Show<Chromosome<A>>
): Show.Show<Individual<A>> =>
  Show.makeShow(
    ({ chromosome, fitness }) =>
      `${S.show(chromosome)}\t(${$fitness.Show.show(fitness)})`
  );

const { _: chromosome$ } = Effect.deriveAccessM(HasChromosome)(["_"]);

const fromChromosome =
  <A extends Gene>(f: FitnessFunction<A>) =>
  (chromosome: Chromosome<A>) =>
    pipe(
      f(chromosome),
      Effect.map((fitness) => ({ chromosome, fitness }))
    );

const ofSize =
  (size: number) =>
  <A extends Gene>(f: FitnessFunction<A>) =>
    pipe(
      chromosome$(($) => $<A>().ofSize(size)),
      Effect.chain(fromChromosome(f))
    );

export const $individual = { getOrd, getShow, fromChromosome, ofSize };
