export interface Countable<A> {
  readonly size: <B extends A>(a: B) => number
}
