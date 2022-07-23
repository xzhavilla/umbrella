import { Array, Effect, Option, Record, Show } from "@effect-ts/core";
import * as Tuple from "@effect-ts/core/Collections/Immutable/Tuple";
import { flow, pipe } from "@effect-ts/system/Function";
import { Chromosome } from "./Chromosome";
import { HasChromosome } from "./Environment";
import { Gene } from "./Gene";
import { $individual, Individual } from "./Individual";
import { $probability } from "./Probability";

export type Population<A extends Gene> = Array.Array<Individual<A>>;

export interface ClusteringFunction<A extends Gene> {
  (
    species: Record.Dictionary<Population<A>>,
    individual: Individual<A>
  ): Record.Dictionary<Population<A>>;
}

const getShow = <A extends Gene>(
  S: Show.Show<Chromosome<A>>
): Show.Show<Population<A>> =>
  Show.makeShow(
    flow(Array.map($individual.getShow<A>(S).show), Array.join("\n"))
  );

const ofSize = (populationSize: number, chromosomeSize: number) =>
  flow(
    $individual.ofSize(chromosomeSize),
    Effect.replicate(populationSize - 1),
    Array.sequence(Effect.Applicative)
  );

const cluster =
  <A extends Gene>(
    f: ClusteringFunction<A> = (species, individual) => ({
      _: [...(species._ ?? []), individual],
    })
  ) =>
  (population: Population<A>) =>
    pipe(
      population.reduce(f, {} as Record.Dictionary<Population<A>>),
      (x) => {
        console.log(
          Object.entries(x).map(
            ([k, v]) => `${k}: ${v.length} (${v[0].fitness})`
          )
        );

        return x;
      },
      Record.values
    );

const select = <A extends Gene>(species: Array.Array<Population<A>>) => {
  const population = Array.flatten(species);

  return pipe(
    species,
    Array.chain(
      Array.takeLeft(Math.max(16, population.length / species.length / 2))
    ),
    Array.concatS(population),
    Array.takeLeft(population.length / 2),
    Effect.succeed
  );
};

const _select = <A extends Gene>(species: Array.Array<Population<A>>) => {
  const population = Array.flatten(species);

  return pipe(
    species,
    Array.mapEffectPar((population) =>
      pipe(
        population,
        Array.mapEffectPar(() => $probability.next),
        Effect.map(Array.zip(population)),
        Effect.map(Array.map(Tuple.toNative)),
        Effect.let(
          "survivors",
          Array.filterMapWithIndex((i, [n, individual]) =>
            0 === i ||
            population.length - 1 === i ||
            n < (population.length - i) / population.length
              ? Option.some(individual)
              : Option.none
          )
        ),
        Effect.let("top", ({ survivors }) =>
          Array.takeLeft(population.length / 4)(survivors)
        ),
        Effect.let("bottom", ({ survivors }) =>
          Array.takeRight(population.length / 4)(survivors)
        ),
        Effect.map(({ top, bottom }) => pipe(top, Array.concatS(bottom)))
      )
    ),
    Effect.map(Array.flatten),
    Effect.map(Array.concatS(population)),
    Effect.map(Array.takeLeft(population.length / 2))
  );
};

const isPair = <A>(as: Array.Array<A>): as is Readonly<[A, A]> =>
  2 === as.length;

const { _: chromosome$ } = Effect.deriveAccessM(HasChromosome)(["_"]);

const crossOver = <A extends Gene>(population: Population<A>) =>
  pipe(
    population,
    Array.chunksOf(2),
    Array.filter(isPair),
    Array.map(([a, b]) =>
      chromosome$(($) => $<A>().crossOver(a.chromosome, b.chromosome))
    ),
    Array.sequence(Effect.Applicative),
    Effect.map(Array.flatten)
  );

export const $population = { getShow, ofSize, cluster, select, crossOver };
