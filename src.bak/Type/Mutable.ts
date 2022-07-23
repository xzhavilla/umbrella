import {Probability} from '../Probability';

export interface Mutable<A> {
  readonly mutate: <B extends A>(a: B, probability?: Probability) => A
}
