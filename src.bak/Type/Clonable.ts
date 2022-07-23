export interface Clonable<A> {
  readonly clone: <B extends A>(a: B) => B
}
