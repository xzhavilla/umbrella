import {Candidate} from './Candidate';
import {FitnessFunction} from './FitnessFunction';
import {Individual} from './Individual';
import {Offsprings} from './Offsprings';
import {Population} from './Population';
import {Countable} from './Type/Countable';

export interface Candidates<Individual_ extends Individual = Individual> extends Array<Candidate<Individual_>> {
}

// const lens = <Individual_ extends Individual>() => ({
//   candidates: new Lens<Candidates<Individual_>, Array<Candidate<Individual_>>>(identity, a => _ => a)
// });
// const optional = <Individual_ extends Individual>() => ({
//   best: index<Individual_>().index(0)
// });
// const candidateTraversal = <Individual_ extends Individual>() => lens<Individual_>().candidates
//   .composeTraversal(fromTraversable(array)<Candidate<Individual_>>());
// const traversal = <Individual_ extends Individual>() => ({
//   candidate: candidateTraversal<Individual_>(),
//   individual: candidateTraversal<Individual_>().composeLens(Candidate.optic.lens<Individual_>().individual)
// });
// const index = <Individual_ extends Individual>() => new Index(
//   (i: number) => lens<Individual_>().candidates
//     .composeOptional(indexArray<Candidate<Individual_>>().index(i))
// );
// const getter = <Individual_ extends Individual>() => ({
//   population: new Getter<Candidates<Individual_>, Population<Individual_>>(
//     candidates => pipe(
//       candidates,
//       traversal<Individual_>().individual.asFold().getAll,
//       Population.fromIndividuals
//     )
//   )
// });

const _Candidates: Countable<Candidates> = {
  size: candidates => candidates.length
};

export const Candidates = {
  ..._Candidates,
  // optic: {lens, optional, traversal, index, getter},
  fromFitnessFunction: <Individual_ extends Individual>(f: FitnessFunction<Individual_>) => ({
    andPopulation: (population: Population<Individual_>): Candidates<Individual_> =>
      // Population.optic.lens<Individual_>().individuals
      //   .get(population)
      population.individuals
        .map(
          individual => Candidate
            .fromIndividual(individual)
            .andFitness(f(individual))
        )
        .sort(Candidate.compare)
        .reverse(),
    andCandidates: (candidates: Candidates<Individual_>) => ({
      withOffsprings: (offsprings: Offsprings<Individual_>): Candidates<Individual_> => {
        // lens<Individual_>().candidates
        //   .modify(
        /*candidates => */
        const children = Candidates.fromFitnessFunction(f).andPopulation(Population.fromIndividuals(offsprings));
        return candidates
          .reverse()
          .map(
            (parent, i) => i < children.length ? children[i] : parent
            // pipe(
            //   offsprings,
            //   Population.fromIndividuals,
            //   Candidates.fromFitnessFunction(f).andPopulation,
            //   // index<Individual_>().index(i).getOption,
            //   // fold(() => parent, identity)
            //   ates => i < ates.length ? ates[i] : parent
            // )
          )
          .sort(Candidate.compare)
          .reverse();
        // )(candidates)
      }
    })
  })
};
