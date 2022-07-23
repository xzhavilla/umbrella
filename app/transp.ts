#!/usr/bin/env node

import { Array, Effect, Ord, Show, String } from "@effect-ts/core";
import { flow, pipe } from "@effect-ts/system/Function";
import * as Layer from "@effect-ts/system/Layer";
import { solve } from "../src";
import { Chromosome } from "../src/Chromosome";
import { $vlc } from "../src/chromosome/Vlc";
import { HasChromosome, HasGene, HasLogger } from "../src/Environment";
import { $lat, Lat } from "../src/gene/Lat";
import { $console } from "../src/log/Console";

const cipher = process.argv[2];
const [, , , ...matches] = process.argv;

const chromosomeL = Layer.pure(HasChromosome)({ _: () => $vlc<Lat>() as any });
const geneL = Layer.pure(HasGene)({ _: () => $lat as any });
const loggerL = Layer.pure(HasLogger)($console);

const decode = (cipher: string) => (key: Chromosome<Lat>) => {
  const _cipher = cipher.toUpperCase().replace(/ +/g, "");
  const length = _cipher.length / key.length;
  const mod = _cipher.length % Math.floor(length);

  return pipe(
    key.reduce(
      (as, char, i) => [
        ...as,
        {
          char,
          i,
          length: Math.floor(length) + (i < mod ? 1 : 0),
          pad: i < mod ? "" : " ",
        },
      ],
      [] as Array.Array<{
        readonly char: Lat;
        readonly i: number;
        readonly length: number;
        readonly pad: string;
      }>
    ),
    Array.sortBy([
      pipe(
        Ord.string,
        Ord.contramap(({ char }) => char)
      ),
      pipe(
        Ord.number,
        Ord.contramap(({ i }) => i)
      ),
    ]),
    (as) =>
      as.reduce(
        ([as, rest], a) =>
          [
            [...as, [a.i, rest.slice(0, a.length) + a.pad]],
            rest.slice(a.length),
          ] as const,
        [[] as Array.Array<Readonly<[number, string]>>, _cipher] as const
      ),
    ([as]) => as,
    Array.sort(
      pipe(
        Ord.number,
        Ord.contramap(([i]) => i)
      )
    ),
    Array.map(([, s]) => s),
    Array.map((s) => s.split("")),
    (s) =>
      pipe(
        Array.range(1, Math.ceil(length)),
        Array.map((i) =>
          pipe(s, Array.map(Array.filterWithIndex((j) => i - 1 === j)))
        )
      ),
    Array.map((s) => s.join("")),
    (s) => s.join("").replace(/ +/g, "")
  );
};

const transp =
  (cipher: string, matches: Array.Array<string>) => (key: Chromosome<Lat>) => {
    const _matches = matches.map((s) => s.toUpperCase());

    return pipe(
      key,
      decode(cipher),
      (s) =>
        _matches.reduce(
          (found, match) => (new RegExp(match).test(s) ? found + 1 : found),
          0
        ),
      (found) =>
        1000 * (_matches.length / (_matches.length - found) - 1) - key.length,
      Effect.succeed
    );
  };

pipe(
  solve(100, 1)(
    transp(cipher, matches),
    (species, individual) => ({
      ...species,
      [individual.chromosome.length]: [
        ...(species[individual.chromosome.length] ?? []),
        individual,
      ],
    }),
    Show.makeShow(
      (key) => `[${key.join("").toUpperCase()}] ${decode(cipher)(key)}`
    )
  ),
  Effect.provideSomeLayer(Layer.all(chromosomeL, geneL, loggerL)),
  Effect.runPromise
).catch((e) => console.error(e));
