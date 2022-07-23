import {Individual} from './Individual';
import {Tuple} from './Tuple';

export interface Crossover<Individual_ extends Individual = Individual> {
  readonly from: Tuple<Individual_, 2>
  readonly to: Tuple<Individual_, 2>
}

export const Crossover = {
  fromIndividuals: <Individual_ extends Individual>([a, b]: Tuple<Individual_, 2>): Crossover<Individual_> => {
    const n = Math.floor(Math.random() * Math.min(Individual.size(a), Individual.size(b)));
    let [c, d] = [Individual.clone(a), Individual.clone(b)];

    // const tmp=c
    for (let i = 0; i < n; i++) {
      // c=pipe(
      //   d,
      //   Individual.optic.index<Individual_>().index(i).getOption,
      //   fold(
      //     ()=>c,
      //     gene=>Individual.optic.index<Individual_>().index(i).set(gene)(c)
      //   )
      // )
      // d=pipe(
      //   tmp,
      //   Individual.optic.index<Individual_>().index(i).getOption,
      //   fold(
      //     ()=>d,
      //     gene=>Individual.optic.index<Individual_>().index(i).set(gene)(d)
      //   )
      // )
      const tmp = c.genes[i];
      c.genes[i] = d.genes[i];
      d.genes[i] = tmp;
    }

    return {
      from: [a, b],
      to: [c, d]
    };
  }
};
