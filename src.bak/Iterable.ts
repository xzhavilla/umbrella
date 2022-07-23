import {Lazy} from 'fp-ts/lib/function';
import {IteratorResult} from './IteratorResult';

export interface Iterable<T = unknown, TReturn = unknown, TNext = unknown> {
  readonly [Symbol.iterator]: Lazy<Iterator<T, TReturn, TNext>>
}

const iteration = Symbol('Iteration');
const recursion = Symbol('Recursion');
type Iteration = typeof iteration
type Recursion = typeof recursion
type Strategy = Iteration | Recursion

export const Iterable = {
  Fold: {
    Strategy: {
      Iteration: iteration as Iteration,
      Recursion: recursion as Recursion
    }
  },
  fromIterator: <T, TReturn, TNext>(iterator: Iterator<T, TReturn, TNext>): Iterable<T, TReturn, TNext> => ({
    ...iterator,
    [Symbol.iterator]: () => iterator
  }),
  foldl: <A, B>(acc: Lazy<B>, f: (b: Lazy<B>, a: A) => B, strategy: Strategy = iteration) =>
    (as: Iterable<A>): B => {
      const iterator = as[Symbol.iterator]();

      switch (strategy) {
        case iteration:
          let b = acc();
          for (const a of Iterable.fromIterator(iterator)) {
            b = f(() => b, a);
          }

          return b;
        case recursion:
          return IteratorResult.fold(
            acc,
            (value: A) => Iterable.foldl(() => f(acc, value), f, strategy)(Iterable.fromIterator(iterator))
          )(iterator.next());
      }
    },
  foldr: <A, B>(z: Lazy<B>, f: (a: A, b: Lazy<B>) => B, strategy: Strategy = iteration) =>
    (as: Iterable<A>): B => {
      const iterator = as[Symbol.iterator]();

      switch (strategy) {
        case iteration:
          return Iterable
            .foldl(
              () => [],
              (as: Lazy<Array<A>>, a: A) => as().concat(a),
              strategy
            )(Iterable.fromIterator(iterator))
            .reduceRight((z, a) => f(a, () => z), z());
        case recursion:
          return IteratorResult.fold(
            z,
            (value: A) => f(value, () => Iterable.foldr(z, f, strategy)(Iterable.fromIterator(iterator)))
          )(iterator.next());
      }
    }
};
