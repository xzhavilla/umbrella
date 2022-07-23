import { Effect, Show } from "@effect-ts/core";
import { flow, not, pipe } from "@effect-ts/system/Function";
import { Chromosome } from "./Chromosome";
import { HasChromosome, HasGene, HasLogger } from "./Environment";
import { FitnessFunction } from "./Fitness";
import { Gene } from "./Gene";
import { $generation } from "./Generation";
import { $population, ClusteringFunction } from "./Population";

const { _: chromosome$ } = Effect.deriveAccess(HasChromosome)(["_"]);
const { _: gene$ } = Effect.deriveAccess(HasGene)(["_"]);
const { notice } = Effect.deriveLifted(HasLogger)(["notice"], [], []);

export const solve =
  (m: number, n: number) =>
  <A extends Gene>(
    fitnessFunction: FitnessFunction<A>,
    clusteringFunction?: ClusteringFunction<A>,
    S?: Show.Show<Chromosome<A>>
  ) =>
    pipe(
      Effect.do,
      Effect.bind("population", () =>
        $population.ofSize(m, n)(fitnessFunction)
      ),
      Effect.let("initial", ({ population }) =>
        $generation.initial(population)
      ),
      Effect.bind("ShowGene", () => gene$(($) => $<A>().Show)),
      Effect.bind("ShowChromosome", ({ ShowGene }) =>
        chromosome$(($) => $<A>().getShow(ShowGene))
      ),
      Effect.let("ShowGeneration", ({ ShowChromosome }) =>
        $generation.getShow(S ?? ShowChromosome)
      ),
      Effect.tap(({ initial, ShowGeneration }) =>
        notice(ShowGeneration.show(initial))
      ),
      Effect.chain(({ initial, ShowGeneration }) =>
        pipe(
          flow(
            $generation.next(fitnessFunction, clusteringFunction),
            Effect.tap((generation) => notice(ShowGeneration.show(generation)))
          ),
          Effect.iterate(initial, not($generation.hasSolution))
        )
      )
    );
