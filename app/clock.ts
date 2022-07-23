#!/usr/bin/env node

import { Clock, Effect, Show } from "@effect-ts/core";
import { pipe } from "@effect-ts/system/Function";
import * as Layer from "@effect-ts/system/Layer";
import { solve } from "../src";
import { Chromosome } from "../src/Chromosome";
import { $vlc } from "../src/chromosome/Vlc";
import { HasChromosome, HasGene, HasLogger } from "../src/Environment";
import { $dec, Dec } from "../src/gene/Dec";
import { $console } from "../src/log/Console";

const chromosomeL = Layer.pure(HasChromosome)({ _: () => $vlc<Dec>() as any });
const geneL = Layer.pure(HasGene)({ _: () => $dec as any });
const loggerL = Layer.pure(HasLogger)($console);

const clock = (chromosome: Chromosome<Dec>) =>
  pipe(
    Clock.currentTime,
    Effect.map(
      (time) =>
        Math.round(time / 1000) -
        +chromosome.join("") +
        (0 === chromosome[0] ? -(10 ** chromosome.length) : 0)
    ),
    Effect.map((diff) => 10 ** chromosome.length / Math.abs(diff))
  );

pipe(
  solve(1000, 1)(
    clock,
    undefined,
    Show.makeShow((chromosome) =>
      new Date(+chromosome.join("") * 1000).toISOString()
    )
  ),
  Effect.provideSomeLayer(Layer.all(chromosomeL, geneL, loggerL)),
  Effect.runPromise
).catch((e) => console.error(e));
