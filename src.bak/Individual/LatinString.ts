import {LatinChar} from '../Gene/LatinChar';
import {Individual} from '../Individual';
import {Randomizable} from '../Type/Randomizable';

export interface LatinString<Size_ extends number = number> extends Individual<LatinChar, Size_> {
}

const ofSize: <Size_ extends number>(size: Size_) => Randomizable<LatinString<Size_>> = Individual.fromGene(LatinChar).andSize;

export const LatinString = {ofSize: ofSize};
