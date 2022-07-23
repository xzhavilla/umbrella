import {Lens} from 'monocle-ts';
import {Comparable} from './Type/Comparable';
import {Mutable} from './Type/Mutable';
import {Showable} from './Type/Showable';

export interface Gene<A = unknown> {
  readonly $: Comparable<Gene<A>> & Mutable<Gene<A>> & Showable<Gene<A>>
  readonly value: A
}

const lens = <Gene_ extends Gene>() => ({
  $: Lens.fromProp<Gene_>()('$'),
  value: Lens.fromProp<Gene_>()('value')
});

export const Gene = {
  optic: {lens}
};
