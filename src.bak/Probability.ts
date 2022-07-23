import {Lazy} from 'fp-ts/lib/function';
import {Order} from './Order';
import {Comparable} from './Type/Comparable';
import {Randomizable} from './Type/Randomizable';

const minProbability = 0;
const maxProbability = 1;
type MinProbability = typeof minProbability
type MaxProbability = typeof maxProbability
export type Probability = MinProbability | number | MaxProbability

const _Probability: Comparable<Probability> & Randomizable<Probability> = {
  compare: (a, b) => a > b ? Order.GreaterThan : a < b ? Order.LessThan : Order.EqualTo,
  random: Math.random
};

export const Probability = {
  ..._Probability,
  Min: minProbability as MinProbability,
  Max: maxProbability as MaxProbability,
  fold: <A>(onFalse: Lazy<A>, onTrue: Lazy<A>) => (probability: Probability): A =>
    Order.LessThan === Probability.compare(Probability.random(), probability) ? onFalse() : onTrue()
};
