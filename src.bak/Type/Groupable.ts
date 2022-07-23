export interface Groupable<A> {
  group: <B extends A>(as: Array<B>) => Array<Array<B>>
}
