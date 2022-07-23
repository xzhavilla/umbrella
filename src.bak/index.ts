import { fold, none, Option, some } from "fp-ts/lib/Option";
import { pipe } from "fp-ts/lib/pipeable";
import { Candidate } from "./Candidate";
import { Fitness } from "./Fitness";
import { FitnessFunction } from "./FitnessFunction";
import { Generation } from "./Generation";
import { Individual } from "./Individual";
import { Randomizable } from "./Type/Randomizable";

export const select = <Individual_ extends Individual>(
  f: FitnessFunction<Individual_>,
  individual: Randomizable<Individual_>,
  size: number,
  limit: number = Infinity
): Generation<Individual_> | never => {
  let result = none as Option<Generation<Individual_>>;
  for (const generation of Generation.iterator
    .fromFitnessFunction(f)
    .andIndividual(individual)
    .andSize(size)) {
    result = some(generation);
    const best = Candidate.best(generation.candidates);
    console.info(
      `>¦  ${generation.number}.\t${Individual.show(
        Candidate.best(generation.candidates).individual
      )} ${Fitness.show(best.fitness)}`
    );

    if (generation.number >= limit) {
      break;
    }
  }

  return pipe(
    result,
    fold(
      () => {
        throw Error();
      },
      (generation) => {
        console.info(
          ` ¦> ${Individual.show(
            Candidate.best(generation.candidates).individual
          )}`
        );

        return generation;
      }
    )
  );
};
