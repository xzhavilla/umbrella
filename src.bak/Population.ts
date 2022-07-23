import {Lens} from 'monocle-ts';
import {Candidates} from './Candidates';
import {Individual} from './Individual';
import {Countable} from './Type/Countable';
import {Randomizable} from './Type/Randomizable';

export interface Population<Individual_ extends Individual = Individual> {
  readonly individuals: Array<Individual_>
}

const lens = <Individual_ extends Individual>() => ({
  individuals: Lens.fromProp<Population<Individual_>>()('individuals')
});

const _Population: Countable<Population> = {
  size: population => population.individuals.length
};

export const Population = {
  ..._Population,
  optic: {lens},
  fromIndividuals: <Individual_ extends Individual>(individuals: Array<Individual_>): Population<Individual_> => ({
    individuals: individuals
  }),
  fromCandidates: <Individual_ extends Individual>(candidates: Candidates<Individual_>): Population<Individual_> =>
    Population.fromIndividuals(candidates.map(candidate => candidate.individual)),
  fromIndividual: <Individual_ extends Individual>(individual: Randomizable<Individual_>) => ({
    andSize: (size: number): Randomizable<Population<Individual_>> => ({
      random: () => ({individuals: Array(size).fill(undefined).map(individual.random)})
    })
  })
};
