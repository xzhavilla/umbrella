export const IteratorResult = {
  fold: <T, TReturn, A>(
    onReturn: (value: TReturn) => A,
    onYield: (value: T) => A
  ) => (result: IteratorResult<T, TReturn>): A => {
    switch (result.done) {
      case undefined:
      case false:
        return onYield(result.value);
      case true:
        return onReturn(result.value);
    }
  }
};
