import {Tuple} from './Tuple';

export type TupleArray<A, S extends number> = Array<Tuple<A, S>>

export const TupleArray = {
  ofSize: <S extends number>(size: S) => ({
    fromArray: <A>(array: Array<A>): TupleArray<A, S> =>
      array.reduce(
        (acc: TupleArray<A, S>, _, i, array): TupleArray<A, S> =>
          i > 0 && size - 1 === i % size
            ? [...acc, array.slice(i - size + 1, i + 1) as Tuple<A, S>]
            : acc,
        []
      )
  })
};
