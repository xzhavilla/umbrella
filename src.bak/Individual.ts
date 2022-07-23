import {Index, Lens} from 'monocle-ts';
import {indexArray} from 'monocle-ts/lib/Index/Array';
import {Gene} from './Gene';
import {Probability} from './Probability';
import {Clonable} from './Type/Clonable';
import {Comparable} from './Type/Comparable';
import {Countable} from './Type/Countable';
import {Groupable} from './Type/Groupable';
import {Mutable} from './Type/Mutable';
import {Randomizable} from './Type/Randomizable';
import {Showable} from './Type/Showable';

export interface Individual<Gene_ extends Gene = Gene, Size_ extends number = number> {
  readonly $: Groupable<Individual> & Mutable<Individual>
  readonly genes: Array<Gene_>
}

const lens = <Individual_ extends Individual>() => ({
  $: Lens.fromProp<Individual_>()('$'),
  genes: Lens.fromProp<Individual_>()('genes')
});
const index = <Individual_ extends Individual>() => new Index(
  (i: number) => lens<Individual_>().genes
    .composeOptional(indexArray<Individual_['genes'][number]>().index(i))
);

const _Individual: Clonable<Individual> & Countable<Individual> & Groupable<Individual> & Mutable<Individual> & Showable<Individual> = {
  clone: <Individual_ extends Individual>(individual: Individual_) =>
    lens<Individual_>().genes.modify(genes => [...genes])(individual),
  size: individual => lens().genes.get(individual).length,
  group: individuals => [individuals],
  mutate: <Individual_ extends Individual>(from: Individual_, probability: Probability = Probability.Max) =>
    index<Individual_>()
      .index(Math.floor(Math.random() * Individual.size(from)))
      .modify(
        gene => gene.$.mutate(gene, probability)
      )(Individual.clone(from)),
  show: individual => `[ ${lens().genes.get(individual).map(gene => Gene.optic.lens().$.get(gene).show(gene)).join(' ')} ]`
};

export const Individual = {
  ..._Individual,
  optic: {lens, index},
  fromGene: <Gene_ extends Gene>(gene: Comparable<Gene_> & Mutable<Gene_> & Randomizable<Gene_> & Showable<Gene_>) => ({
    andSize: <Size_ extends number>(size: Size_): Randomizable<Individual<Gene_, Size_>> => ({
      random: () => ({$: Individual, genes: Array(size).fill(undefined).map(gene.random)})
    })
  })
};
