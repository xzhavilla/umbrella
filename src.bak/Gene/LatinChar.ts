import {Gene} from '../Gene';
import {Order} from '../Order';
import {Probability} from '../Probability';
import {Comparable} from '../Type/Comparable';
import {Equatable} from '../Type/Equatable';
import {Mutable} from '../Type/Mutable';
import {Randomizable} from '../Type/Randomizable';
import {Showable} from '../Type/Showable';

const minChar = 'a';
const maxChar = 'z';
type _LatinChar =
  | typeof minChar
  | 'b'
  | 'c'
  | 'd'
  | 'e'
  | 'f'
  | 'g'
  | 'h'
  | 'i'
  | 'j'
  | 'k'
  | 'l'
  | 'm'
  | 'n'
  | 'o'
  | 'p'
  | 'q'
  | 'r'
  | 's'
  | 't'
  | 'u'
  | 'v'
  | 'w'
  | 'x'
  | 'y'
  | typeof maxChar
export type LatinChar = Gene<_LatinChar>

const of = <A extends _LatinChar>(a: A): LatinChar => ({$: LatinChar, value: a});
const cast = (a: string): _LatinChar => a as _LatinChar;

const _LatinChar: Comparable<LatinChar> & Equatable<LatinChar> & Mutable<LatinChar> & Randomizable<LatinChar> & Showable<LatinChar> = {
  compare: (a, b) => a.value.charCodeAt(0) > b.value.charCodeAt(0) ? Order.GreaterThan : a.value.charCodeAt(0) < b.value.charCodeAt(0) ? Order.LessThan : Order.EqualTo,
  equals: (a, b) => Order.EqualTo === LatinChar.compare(a, b),
  mutate: <A extends LatinChar>(from: A, probability: Probability = Probability.Max) => {
    const to = LatinChar.random();

    return Probability.fold(
      () => from,
      () => LatinChar.equals(from, to) ? LatinChar.mutate(from) : to
    )(probability);
  },
  random: () => of(cast(String.fromCharCode(minChar.charCodeAt(0) + Math.floor(Math.random() * (1 + maxChar.charCodeAt(0) - minChar.charCodeAt(0)))))),
  show: char => char.value.toUpperCase()
};

export const LatinChar = {
  ..._LatinChar
};
