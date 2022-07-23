const lessThan = -1;
const equalTo = 0;
const greaterThan = 1;
type LessThan = typeof lessThan;
type EqualTo = typeof equalTo;
type GreaterThan = typeof greaterThan;
export type Order = LessThan | EqualTo | GreaterThan

export const Order = {
  LessThan: lessThan as LessThan,
  EqualTo: equalTo as EqualTo,
  GreaterThan: greaterThan as GreaterThan
};
