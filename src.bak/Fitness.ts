import {Order} from './Order';
import {Comparable} from './Type/Comparable';
import {Equatable} from './Type/Equatable';
import {Showable} from './Type/Showable';

const minFitness = -Infinity;
const maxFitness = Infinity;
type MinFitness = typeof minFitness
type MaxFitnes = typeof maxFitness
export type Fitness = MinFitness | number | MaxFitnes

const is = <A extends Fitness>(expected: A) => (fitness: Fitness): fitness is A => Fitness.equals(expected, fitness);

const _Fitness: Comparable<Fitness> & Equatable<Fitness> & Showable<Fitness> = {
  compare: (a, b) => a > b ? Order.GreaterThan : a < b ? Order.LessThan : Order.EqualTo,
  equals: (a, b) => Order.EqualTo === Fitness.compare(a, b),
  show: fitness => `|${fitness.toFixed(4)}|`
};

export const Fitness = {
  ..._Fitness,
  Min: minFitness as MinFitness,
  Max: maxFitness as MaxFitnes,
  is: {
    min: is(minFitness),
    max: is(maxFitness)
  }
};
