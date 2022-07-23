import {Bit} from '../Gene/Bit';
import {Individual} from '../Individual';
import {Randomizable} from '../Type/Randomizable';

export interface BitString<Size_ extends number = number> extends Individual<Bit, Size_> {
}

const ofSize: <Size_ extends number>(size: Size_) => Randomizable<BitString<Size_>> = Individual.fromGene(Bit).andSize;

export const BitString = {ofSize: ofSize};
