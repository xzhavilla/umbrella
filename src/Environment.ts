import { Has } from "@effect-ts/core";
import { Chromosome$ } from "./Chromosome";
import { Gene, Gene$ } from "./Gene";
import { Logger } from "./Logger";

export const HasChromosome =
  Has.tag<{ _: <A extends Gene>() => Chromosome$<A> }>();
export const HasGene = Has.tag<{ _: <A extends Gene>() => Gene$<A> }>();
export const HasLogger = Has.tag<Logger>();
