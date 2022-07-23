import { Array, Effect, Random, Show } from "@effect-ts/core";
import { pipe } from "@effect-ts/system/Function";
import { Chromosome, Chromosome$ } from "../Chromosome";
import { HasGene } from "../Environment";
import { Gene } from "../Gene";
import { $probability } from "../Probability";

const getShow = <A extends Gene>(S: Show.Show<A>): Show.Show<Chromosome<A>> =>
  Show.makeShow(
    (chromosome) =>
      `< ${pipe(chromosome, Array.map(S.show), Array.join(" "))} >`
  );

const { _: gene$ } = Effect.deriveAccessM(HasGene)(["_"]);

const getOffspring =
  (crossoverPoint: number) =>
  <A extends Gene>(a: Chromosome<A>, b: Chromosome<A>): Chromosome<A> =>
    pipe(
      a,
      Array.takeLeft(crossoverPoint),
      Array.concatS(pipe(b, Array.dropLeft(crossoverPoint)))
    );

const crossOver =
  <A extends Gene>(a: Chromosome<A>, b: Chromosome<A>) =>
  (crossoverPoint: number): Readonly<[Chromosome<A>, Chromosome<A>]> =>
    [getOffspring(crossoverPoint)(a, b), getOffspring(crossoverPoint)(b, a)];

export const $vlc = <A extends Gene>(): Chromosome$<A> => ({
  getShow,
  ofSize: (size) =>
    pipe(
      gene$(($) => $<A>().random),
      Effect.replicate(size - 1),
      Array.sequence(Effect.Applicative)
    ),
  crossOver: (a, b) =>
    pipe(Random.nextIntBetween(0, a.length - 1), Effect.map(crossOver(a, b))),
  mutate: (probability) => (chromosome) =>
    pipe(
      Effect.do,
      Effect.bind("n1", () => $probability.next),
      Effect.bind("mutant", ({ n1 }) =>
        n1 < probability
          ? pipe(
              Effect.do,
              Effect.bind("i", () =>
                Random.nextIntBetween(0, chromosome.length - 1)
              ),
              Effect.bind("gene", ({ i }) =>
                gene$(($) => $<A>().mutate(1)(chromosome[i]))
              ),
              Effect.map(({ i, gene }) =>
                pipe(chromosome, Array.unsafeUpdateAt(i, gene))
              )
            )
          : Effect.succeed(chromosome)
      ),
      Effect.bind("n2", () => $probability.next),
      Effect.chain(({ mutant, n2 }) =>
        n2 < probability
          ? pipe(
              $probability.next,
              Effect.chain((n) =>
                n < 0.25
                  ? pipe(
                      gene$(($) => $<A>().random),
                      Effect.map((gene) => pipe(mutant, Array.snoc(gene)))
                    )
                  : n < 0.5
                  ? pipe(
                      gene$(($) => $<A>().random),
                      Effect.map((gene) => pipe(mutant, Array.cons(gene)))
                    )
                  : n < 0.75
                  ? pipe(
                      mutant,
                      Array.dropRight(Math.min(1, mutant.length - 1)),
                      Effect.succeed
                    )
                  : pipe(
                      mutant,
                      Array.dropLeft(Math.min(1, mutant.length - 1)),
                      Effect.succeed
                    )
              )
            )
          : Effect.succeed(mutant)
      )
    ),
});
