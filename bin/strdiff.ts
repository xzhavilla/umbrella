#!/usr/bin/env ts-node

import { select } from "../src";
import { Bin } from "../src/gene/Bin";
import { BitString } from "../src/Individual/BitString";

const a = process.argv[2];
const b = process.argv[3];
const length = Math.max(a.length, b.length);
const size = +process.argv[4] || length;
const limit = +process.argv[5] || Infinity;

select(
  (individual) =>
    length /
      individual.genes.reduce(
        (diff: number, bit, i) =>
          (a[i] === b[i] && Bit.is.zero(bit)) ||
          (a[i] !== b[i] && Bit.is.one(bit))
            ? diff
            : diff + 1,
        0
      ) -
    1,
  BitString.ofSize(length),
  size,
  limit
);
