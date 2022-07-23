import { Gene } from "../Gene";
import { Order } from "../Order";
import { Probability } from "../Probability";
import { Comparable } from "../Type/Comparable";
import { Equatable } from "../Type/Equatable";
import { Mutable } from "../Type/Mutable";
import { Randomizable } from "../Type/Randomizable";
import { Showable } from "../Type/Showable";

const zero = 0;
const one = 1;
type Zero = typeof zero;
type One = typeof one;
type _Bit = Zero | One;
export type Bit = Gene<_Bit>;

const of = <A extends _Bit>(a: A): Bit => ({ $: Bit, value: a });
const is = <A extends _Bit>(expected: A) => (bit: Bit): bit is Gene<A> =>
  Bit.equals(of(expected), bit);
const cast = (a: number): _Bit => a as _Bit;

const _Bit: Comparable<Bit> &
  Equatable<Bit> &
  Mutable<Bit> &
  Randomizable<Bit> &
  Showable<Bit> = {
  compare: (a, b) =>
    a.value > b.value
      ? Order.GreaterThan
      : a.value < b.value
      ? Order.LessThan
      : Order.EqualTo,
  equals: (a, b) => Order.EqualTo === Bit.compare(a, b),
  mutate: <A extends Bit>(bit: A, probability: Probability = Probability.Max) =>
    Probability.fold(
      () => bit,
      () => of(cast(+!bit.value))
    )(probability),
  random: () => of(cast(Math.round(Math.random()))),
  show: (bit) => (is(zero)(bit) ? "." : "#"),
};

export const Bit = {
  ..._Bit,
  Zero: zero as Zero,
  One: one as One,
  is: {
    zero: is(zero),
    one: is(one),
  },
};
