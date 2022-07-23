#!/usr/bin/env ts-node

import {select} from '../src';
import {Individual} from '../src/Individual';
import {Elastic} from '../src/Individual/Elastic';
import {LatinString} from '../src/Individual/LatinString';
import {TupleArray} from '../src/TupleArray';

const original = process.argv[2];
const encoded = process.argv[3].toLowerCase().replace(/[^a-z]/g, '');
const strategy = process.argv[4];
const length = +process.argv[5] || Math.max(original.length, encoded.length);
const size = +process.argv[6] || Math.max(original.length, encoded.length);
const limit = +process.argv[7] || Infinity;

const individual = LatinString.ofSize(length);
const chars = TupleArray
  .ofSize(5)
  .fromArray(`${original} XXXX`.toLowerCase().replace(/[^a-z]/g, '').split(''))
  .reduce((array: Array<string>, tuple) => array.concat(tuple), []);

select(
  key => {
    const matrix = [...TupleArray.ofSize(Individual.size(key)).fromArray(chars), chars.slice(-1 * (chars.length % Individual.size(key)))];

    return chars.length / matrix[0]
      .map((col, i) => matrix.map(row => row[i]).filter(char => undefined !== char))
      .map((row, i) => ({key: key.genes[i], value: row}))
      .sort(({key: a}, {key: b}) => a.$.compare(a, b))
      .map(({value}) => value)
      .reduce((array: Array<string>, row) => array.concat(row), [])
      .reduce(
        (diff: number, char, i) => char === encoded[i]
          ? diff
          : diff + 1,
        0
      );
  },
  'elastic' === strategy
    ? Elastic(.5)(individual)
    : individual,
  size,
  limit
);
