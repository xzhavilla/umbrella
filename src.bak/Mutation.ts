import {Individual} from './Individual';
import {Probability} from './Probability';

export interface Mutation<Individual_ extends Individual> {
  readonly from: Individual_
  readonly to: Individual_
}

export const Mutation = {
  ofSize: (size: number) => ({
    withProbability: (probability: Probability) => ({
      fromIndividual: <Individual_ extends Individual>(from: Individual_): Mutation<Individual_> => {
        let to = from;
        for (let i = 0; i < size; i++) {
          to = from.$.mutate(from, probability) as Individual_;
        }

        return ({
          from: from,
          to: to
        });
      }
    })
  })
};
