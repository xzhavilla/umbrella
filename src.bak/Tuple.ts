import {Countable} from './Type/Countable';

export type Tuple<A, S extends number> = [A, ...Array<A>] & { length: S }

const _Tuple: Countable<Tuple<unknown, number>> = {
  size: tuple => tuple.length
};

export const Tuple = {
  ..._Tuple
};
