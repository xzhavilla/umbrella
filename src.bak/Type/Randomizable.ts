import {Lazy} from 'fp-ts/lib/function';

export interface Randomizable<A> {
  readonly random: Lazy<A>
}
