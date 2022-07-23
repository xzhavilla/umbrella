import {Order} from '../Order';

export interface Comparable<A> {
  readonly compare: <B extends A>(a: B, b: B) => Order
}
