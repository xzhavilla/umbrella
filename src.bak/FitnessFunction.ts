import {Fitness} from './Fitness';
import {Individual} from './Individual';

export interface FitnessFunction<Individual_ extends Individual = Individual> {
  (individual: Individual_): Fitness
}
