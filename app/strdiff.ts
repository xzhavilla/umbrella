#!/usr/bin/env node

import { Effect, Show } from "@effect-ts/core";
import { pipe } from "@effect-ts/system/Function";
import * as Layer from "@effect-ts/system/Layer";
import { solve } from "../src";
import { Chromosome } from "../src/Chromosome";
import { $flc } from "../src/chromosome/Flc";
import { HasChromosome, HasGene, HasLogger } from "../src/Environment";
import { $bin, Bin } from "../src/gene/Bin";
import { $console } from "../src/log/Console";

const a = process.argv[2];
const b = process.argv[3];
const length = Math.max(a.length, b.length);

const chromosomeL = Layer.pure(HasChromosome)({ _: () => $flc<Bin>() as any });
const geneL = Layer.pure(HasGene)({ _: () => $bin as any });
const loggerL = Layer.pure(HasLogger)($console);

const strdiff = (a: string, b: string) => (chromosome: Chromosome<Bin>) =>
  pipe(
    chromosome.reduce(
      (diff: number, gene, i) =>
        (a[i] === b[i] && $bin.isZero(gene)) ||
        (a[i] !== b[i] && $bin.isOne(gene))
          ? diff
          : diff + 1,
      0
    ),
    (diff) => Effect.succeed(chromosome.length / diff - 1)
  );

pipe(
  solve(length / 2, length)(
    strdiff(a.toUpperCase(), b.toUpperCase()),
    undefined,
    Show.makeShow((chromosome) =>
      chromosome
        .map((gene, i) => (0 === gene ? (a[i] === b[i] ? a[i] : "*") : "."))
        .join("")
        .toUpperCase()
    )
  ),
  Effect.provideSomeLayer(Layer.all(chromosomeL, geneL, loggerL)),
  Effect.runPromise
).catch((e) => console.error(e));
