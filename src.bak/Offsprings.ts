import { Candidates } from "./Candidates";
import { Crossover } from "./Crossover";
import { Individual } from "./Individual";
import { Mutation } from "./Mutation";
import { Probability } from "./Probability";
import { TupleArray } from "./TupleArray";
import { Countable } from "./Type/Countable";

export interface Offsprings<Individual_ extends Individual = Individual>
  extends Array<Individual_> {}

const _Offsprings: Countable<Offsprings> = {
  size: (offsprings) => offsprings.length,
};

export const Offsprings = {
  ..._Offsprings,
  ofSize: (size: number) => ({
    andCandidates: <Individual_ extends Individual>(
      candidates: Candidates<Individual_>
    ): Offsprings<Individual_> =>
      TupleArray.ofSize(2)
        .fromArray(
          candidates.slice(0, size + 1).map((candidate) => candidate.individual)
        )
        .map(([a, b]) => {
          // console.log(size, a.genes, b.genes);
          return [a, b];
        })
        .map((parents) =>
          Crossover.fromIndividuals(parents as [Individual_, Individual_])
        )
        .map((crossover) => crossover.to)
        .reduce(
          (offsprings: Array<Individual_>, siblings): Array<Individual_> =>
            offsprings.concat(siblings),
          []
        )
        .map((offspring) =>
          Mutation.ofSize(
            Math.floor(Math.random() * Individual.size(offspring))
          )
            .withProbability(Probability.random() / 3)
            .fromIndividual(offspring)
        )
        .map((mutation) => mutation.to)
        .slice(0, size),
  }),
};
