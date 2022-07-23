import {Endomorphism} from 'fp-ts/lib/function';
import {pipe} from 'fp-ts/lib/pipeable';
import {Gene} from '../Gene';
import {Individual} from '../Individual';
import {Probability} from '../Probability';
import {Randomizable} from '../Type/Randomizable';

const elastic = (decrease: number, growth: number) =>
  <Individual_ extends Individual>(individual: Randomizable<Individual_>) =>
    (from: Individual_): Individual_ => {
      const fromSize = Individual.size(from);
      const min = Math.min(fromSize - 1, fromSize * (1 - decrease));
      const max = Math.max(fromSize + 1, fromSize * (1 + growth));
      const toSize = Math.max(1, Math.round(min + Math.random() * (max - min)));
      const genes = individual.random().genes;

      return {
        ...from,
        genes: Array(toSize)
          .fill(undefined)
          .map((_, i): Gene => i < fromSize ? from.genes[i] : genes[i % fromSize]),
        $: {
          ...from.$,
          group: (individuals: Array<Individual_>) => pipe(
            individuals.reduce(
              (acc: Record<string, Array<Individual_>>, individual) => {
                const key = `specie-${Individual.size(individual)}`;
                const specie = key in acc ? acc[key] : [];

                return {
                  ...acc,
                  [key]: specie.concat(individual)
                };
              },
              {}
            ),
            species => Object.values(species)
          ),
          mutate: (from: Individual_, probability = Probability.Max) => Probability.fold(
            () => from,
            () => Individual.mutate(elastic(decrease, growth)(individual)(from), probability)
          )(probability)
        }
      };
    };

export function Elastic<Individual_ extends Individual>(): Endomorphism<Randomizable<Individual_>>
export function Elastic<Individual_ extends Individual>(rate: number): Endomorphism<Randomizable<Individual_>>
export function Elastic<Individual_ extends Individual>(decrease: number, growth: number): Endomorphism<Randomizable<Individual_>>
export function Elastic(decrease: number = 0, growth: number = decrease) {
  return <Individual_ extends Individual>(individual: Randomizable<Individual_>): Randomizable<Individual_> => ({
    random: () => elastic(decrease, growth)(individual)(individual.random())
  });
}
