export interface Showable<A> {
  readonly show: <B extends A>(a: B) => string
}
