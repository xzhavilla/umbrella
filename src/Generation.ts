import { Array, Effect, Option, Ord, Show } from "@effect-ts/core";
import { constFalse, flow, pipe } from "@effect-ts/system/Function";
import { Chromosome } from "./Chromosome";
import { HasChromosome } from "./Environment";
import { $fitness, FitnessFunction } from "./Fitness";
import { Gene } from "./Gene";
import { $individual } from "./Individual";
import { $population, ClusteringFunction, Population } from "./Population";

export interface Generation<A extends Gene> {
  readonly population: Population<A>;
  readonly n: number;
}

const prefix = <A extends Gene>(generation: Generation<A>): string =>
  hasSolution(generation) ? " ¦>" : isInitial(generation) ? ">¦ " : " ¦ ";

const padding = <A extends Gene>({ n }: Generation<A>): string =>
  n < 100 ? "  " : "";

const getShow = <A extends Gene>(
  S: Show.Show<Chromosome<A>>
): Show.Show<Generation<A>> =>
  Show.makeShow(
    (generation) =>
      `${prefix(generation)} ${generation.n}#${padding(generation)}\t${pipe(
        generation,
        bestIndividual,
        Option.fold(() => "-", $individual.getShow<A>(S).show)
      )}${
        false
          ? `\n${$population.getShow<A>(S).show(generation.population)}`
          : ""
      }`
  );

const fromPopulation =
  (n: number) =>
  <A extends Gene>(population: Population<A>): Generation<A> =>
    pipe(
      population,
      Array.sort(Ord.inverted($individual.getOrd<A>())),
      (population) => ({ population, n })
    );

const initial = fromPopulation(0);

const { _: chromosome$ } = Effect.deriveAccessM(HasChromosome)(["_"]);

const next =
  <A extends Gene>(
    fitnessFunction: FitnessFunction<A>,
    clusteringFunction?: ClusteringFunction<A>
  ) =>
  (generation: Generation<A>) =>
    pipe(
      Effect.do,
      Effect.let("species", () =>
        $population.cluster(clusteringFunction)(generation.population)
      ),
      Effect.bind("parents", ({ species }) => $population.select(species)),
      Effect.bind("_offsprings", ({ parents }) =>
        $population.crossOver(parents)
      ),
      Effect.bind("mutants", ({ _offsprings }) =>
        pipe(
          _offsprings,
          Array.map((chromosome) =>
            chromosome$(($) => $<A>().mutate(5 / 7)(chromosome))
          ),
          Array.sequence(Effect.Applicative)
        )
      ),
      Effect.bind("offsprings", ({ mutants }) =>
        pipe(
          mutants,
          Array.map($individual.fromChromosome(fitnessFunction)),
          Array.sequence(Effect.Applicative)
        )
      ),
      Effect.map(({ parents, offsprings }) =>
        pipe(parents, Array.concatS(offsprings))
      ),
      Effect.map(fromPopulation(generation.n + 1))
    );

const isInitial = <A extends Gene>({ n }: Generation<A>) => 0 === n;

const bestIndividual = <A extends Gene>({ population }: Generation<A>) =>
  pipe(population, Array.head);

const hasSolution = flow(
  bestIndividual,
  Option.map(({ fitness }) => fitness),
  Option.map($fitness.isMax),
  Option.getOrElse(constFalse)
);

export const $generation = {
  getShow,
  fromPopulation,
  initial,
  next,
  isInitial,
  hasSolution,
  bestIndividual,
};
