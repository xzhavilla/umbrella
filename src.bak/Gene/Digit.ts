import {Gene} from '../Gene';
import {Order} from '../Order';
import {Probability} from '../Probability';
import {Comparable} from '../Type/Comparable';
import {Equatable} from '../Type/Equatable';
import {Mutable} from '../Type/Mutable';
import {Randomizable} from '../Type/Randomizable';
import {Showable} from '../Type/Showable';

type _Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
export type Digit = Gene<_Digit>

const of = <A extends _Digit>(a: A): Digit => ({$: Digit, value: a});
const cast = (a: number): _Digit => a as _Digit;

const _Digit: Comparable<Digit> & Equatable<Digit> & Mutable<Digit> & Randomizable<Digit> & Showable<Digit> = {
  compare: (a, b) => a.value > b.value ? Order.GreaterThan : a.value < b.value ? Order.LessThan : Order.EqualTo,
  equals: (a, b) => Order.EqualTo === Digit.compare(a, b),
  mutate: <A extends Digit>(from: A, probability: Probability = Probability.Max) => {
    const to = Digit.random();

    return Probability.fold(
      () => from,
      () => Digit.equals(from, to) ? Digit.mutate(from) : to
    )(probability);
  },
  random: () => of(cast(Math.floor(Math.random() * 10))),
  show: digit => '' + digit
};

export const Digit = {
  ..._Digit
};
