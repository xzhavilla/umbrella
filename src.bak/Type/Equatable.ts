export interface Equatable<A> {
  readonly equals: <B extends A>(a: B, b: B) => boolean
}
