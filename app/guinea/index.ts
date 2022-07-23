#!/usr/bin/env node

import { Array, Effect, Option, Record, Show } from "@effect-ts/core";
import { pipe } from "@effect-ts/system/Function";
import * as Layer from "@effect-ts/system/Layer";
import { solve } from "../../src";
import { Chromosome } from "../../src/Chromosome";
import { $vlc } from "../../src/chromosome/Vlc";
import { HasChromosome, HasGene, HasLogger } from "../../src/Environment";
import { $qua, Qua } from "../../src/gene/Qua";
import { $console } from "../../src/log/Console";
import { Population } from "../../src/Population";
import { $history, History } from "./History";
import { Maze } from "./Maze";
import { $position, Position } from "./Position";
import { $shift } from "./Shift";

const MAZES: Array.Array<Maze> = [
  [
    "████████",
    ".......█",
    ".......█",
    ".......█",
    ".......█",
    ".......█",
    ".......█",
    ".......█",
  ],
  [
    "██......",
    ".██.....",
    "..██....",
    "...██...",
    "....██..",
    ".....██.",
    "......██",
    ".......█",
  ],
  [
    "███.....",
    "..█.....",
    "..█..███",
    "..█.██.█",
    "..███.██",
    "......█.",
    "......█.",
    "......██",
  ],
  [
    "█.███...",
    "█.█.█...",
    "█.█.█...",
    "█.█.████",
    "█.█....█",
    "█.████.█",
    "█....█.█",
    "██████.█",
  ],
  [
    "███.███.........█████......",
    "..███.█.███████.█...█......",
    "......█.█.....█.█..██......",
    "...████.█.....███..█.......",
    "...█....█..........█.......",
    "...██████..█████████.......",
    "...........█...............",
    ".███████████..██████████...",
    ".█............█........█...",
    ".██████████████......███...",
    ".....................█.....",
    ".....................██████",
  ],
  [
    "█████████", //
    "......█.█",
    "███████.█",
    "█.......█",
    "███████.█",
  ],
  [
    "█████████", //
    "......█.█",
    "█████████",
    "█.█.....█",
    "███████.█",
  ],
  [
    "███.████........█████..███.",
    "..███.█.███████.█...█..█.█.",
    "......█.█.....█.█.█████████",
    ".██████.█..██████..█......█",
    "...█....█.........█████...█",
    ".█████████.█████████..███.█",
    "...........█............███",
    ".████████████.██████████..█",
    ".█............█........█..█",
    ".██████████████.████████..█",
    "...█...........█.....█.....",
    "...█████████████...████████",
  ],
];

const maze = +process.argv[2];

type Distance = number;

const chromosomeL = Layer.pure(HasChromosome)({ _: () => $vlc<Qua>() as any });
const geneL = Layer.pure(HasGene)({ _: () => $qua as any });
const loggerL = Layer.pure(HasLogger)($console);

const guinea = (maze: Maze) => (chromosome: Chromosome<Qua>) =>
  pipe(
    chromosome,
    // $shift.fromBins,
    (shifts) =>
      shifts.reduce((state, shift) => {
        if (Option.isNone(state)) {
          return Option.none;
        }

        const position = state.value[0];
        const distance = state.value[1];
        const history = state.value[2];
        const nextPosition = $position.next(shift)(position);

        return !$position.isInMaze(maze)(nextPosition) ||
          $history.hasVisited(maze, nextPosition)(history)
          ? Option.none
          : Option.some([
              nextPosition,
              distance + 1,
              $history.visit(maze, nextPosition)(history),
            ] as const);
      }, Option.some([$position.start, 0, $history.visit(maze, $position.start)($history.empty)] as Readonly<[Position, Distance, History]>)),
    Option.fold(
      () => -Infinity,
      ([position, distance]) =>
        // Math.sqrt(
        //   ($position.end(maze)[0] - $position.start[0]) ** 2 +
        //     ($position.end(maze)[1] - $position.start[1]) ** 2
        // ) /
        //   Math.sqrt(
        //     ($position.end(maze)[0] - position[0]) ** 2 +
        //       ($position.end(maze)[1] - position[1]) ** 2
        //   ) -
        // 1
        $position.isEnd(maze)(position) ? Infinity : distance
    ),
    Effect.succeed
  );

pipe(
  solve(1000, 1)(
    guinea(MAZES[maze]),
    (species, individual) => {
      if (-Infinity === individual.fitness) {
        return {
          ...species,
          _: [...(species._ ?? []), individual],
        };
      }

      const specie = individual.chromosome.join("");
      const [done, _species] = Object.entries(species).reduce(
        ([done, _species], [_specie, population]) =>
          done
            ? ([true, { ..._species, [_specie]: population }] as const)
            : new RegExp(`^${specie}`).test(_specie)
            ? ([
                true,
                { ..._species, [_specie]: [...population, individual] },
              ] as const)
            : new RegExp(`^${_specie}`).test(specie)
            ? ([
                true,
                { ..._species, [specie]: [...population, individual] },
              ] as const)
            : ([false, { ..._species, [_specie]: population }] as const),
        [false, {}] as Readonly<[boolean, Record.Dictionary<Population<Qua>>]>
      );

      return done ? _species : { ..._species, [specie]: [individual] };
    },
    Show.makeShow((chromosome) =>
      pipe(
        chromosome,
        // $shift.fromBins,
        Array.map($shift.Show.show),
        Array.join(" ")
      )
    )
  ),
  Effect.provideSomeLayer(Layer.all(chromosomeL, geneL, loggerL)),
  Effect.runPromise
).catch((e) => console.error(e));
