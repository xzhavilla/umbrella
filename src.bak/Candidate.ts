import {pipe} from 'fp-ts/lib/pipeable';
import {Lens} from 'monocle-ts';
import {Candidates} from './Candidates';
import {Fitness} from './Fitness';
import {Individual} from './Individual';
import {Comparable} from './Type/Comparable';
import {Groupable} from './Type/Groupable';
import {Showable} from './Type/Showable';

export interface Candidate<Individual_ extends Individual = Individual> {
  readonly fitness: Fitness
  readonly individual: Individual_
}

const lens = <Individual_ extends Individual>() => ({
  fitness: Lens.fromProp<Candidate<Individual_>>()('fitness'),
  individual: Lens.fromProp<Candidate<Individual_>>()('individual')
});

const _Candidate: Comparable<Candidate> & Groupable<Candidate> & Showable<Candidate> = {
  compare: (a, b) => Fitness.compare(lens().fitness.get(a), lens().fitness.get(b)),
  group: candidates => candidates[0].individual.$
    .group(candidates.map(candidate => ({...candidate.individual, candidate: candidate})))
    .map(individuals => individuals.map(individual => individual.candidate)),
  show: candidate => `${pipe(candidate, lens().fitness.get, Fitness.show)}\t${pipe(candidate, lens().individual.get, Individual.show)}`
};

export const Candidate = {
  ..._Candidate,
  optic: {lens},
  fromIndividual: <Individual_ extends Individual>(individual: Individual_) => ({
    andFitness: (fitness: Fitness): Candidate<Individual_> => ({
      fitness: fitness,
      individual: individual
    })
  }),
  best: <Individual_ extends Individual>(candidates: Candidates<Individual_>): Candidate<Individual_> => candidates[0],
  isSolution: <Individual_ extends Individual>(candidate: Candidate<Individual_>): boolean =>
    pipe(candidate, lens().fitness.get, Fitness.is.max)
};
