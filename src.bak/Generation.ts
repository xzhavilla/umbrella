import { pipe } from "fp-ts/lib/pipeable";
import { Lens } from "monocle-ts";
import { Candidate } from "./Candidate";
import { Candidates } from "./Candidates";
import { FitnessFunction } from "./FitnessFunction";
import { Individual } from "./Individual";
import { Offsprings } from "./Offsprings";
import { Population } from "./Population";
import { Randomizable } from "./Type/Randomizable";

export interface Generation<Individual_ extends Individual = Individual> {
  readonly number: number;
  readonly candidates: Candidates<Individual_>;
}

const first = 1;

const lens = <Individual_ extends Individual>() => ({
  number: Lens.fromProp<Generation<Individual_>>()("number"),
  candidates: Lens.fromProp<Generation<Individual_>>()("candidates"),
});
// const optional = <Individual_ extends Individual>() => ({
//   bestCandidate: lens<Individual_>().candidates.composeOptional(Candidates.optic.optional<Individual_>().best)
// });

const fromFitnessFunctionAndNumberAndPopulation = <
  Individual_ extends Individual
>(
  f: FitnessFunction<Individual_>
) => (number: number) => (
  population: Population<Individual_>
): Generation<Individual_> => {
  const generation = {
    number: number,
    candidates: Candidates.fromFitnessFunction(f).andPopulation(population),
  };

  lens()
    .candidates.get(generation)
    .map((candidate) => console.log(Candidate.show(candidate)));

  return generation;
};

export const Generation = {
  // optic: {lens, optional},
  fromFitnessFunction: <Individual_ extends Individual>(
    f: FitnessFunction<Individual_>
  ) => ({
    andPopulation: fromFitnessFunctionAndNumberAndPopulation(f)(first),
    andGeneration: (
      generation: Generation<Individual_>
    ): Generation<Individual_> => {
      const candidates = generation.candidates; // Generation.optic.lens<Individual_>().candidates.get(generation);

      return pipe(
        Candidate.group(candidates)
          .map((candidates) =>
            pipe(
              //lens().number.get(generation) + 1)(
              Offsprings.ofSize(
                Math.floor(Candidates.size(candidates) / 2) - 1
              ).andCandidates(candidates),
              Candidates.fromFitnessFunction(f).andCandidates(candidates)
                .withOffsprings
              // Candidates.optic.getter<Individual_>().population.get
            )
          )
          .reduce(
            (acc: Array<Candidate>, candidates) => acc.concat(candidates),
            []
          ),
        (x) => Population.fromCandidates(x as any),
        fromFitnessFunctionAndNumberAndPopulation(f)(generation.number + 1)
      );
    },
  }),
  hasSolution: <Individual_ extends Individual>(
    generation: Generation<Individual_>
  ): boolean => Candidate.isSolution(Candidate.best(generation.candidates)),
  // pipe(
  //   generation,
  //   Generation.optic.optional().bestCandidate.getOption,
  //   fold(() => false, Candidate.isSolution)
  // ),
  iterator: {
    fromFitnessFunction: <Individual_ extends Individual>(
      f: FitnessFunction<Individual_>
    ) => ({
      andIndividual: (individual: Randomizable<Individual_>) => ({
        andSize: function* (
          size: number
        ): Generator<Generation<Individual_>, void> {
          for (
            let generation = pipe(
              Population.fromIndividual(individual).andSize(size).random(),
              Generation.fromFitnessFunction(f).andPopulation
            );
            true;
            generation = Generation.fromFitnessFunction(f).andGeneration(
              generation
            )
          ) {
            yield generation;

            if (Generation.hasSolution(generation)) {
              break;
            }
          }
        },
      }),
    }),
  },
};
