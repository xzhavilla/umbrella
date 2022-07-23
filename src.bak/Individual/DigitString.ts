import {Digit} from '../Gene/Digit';
import {Individual} from '../Individual';
import {Randomizable} from '../Type/Randomizable';

export interface DigitString<Size_ extends number = number> extends Individual<Digit, Size_> {
}

const ofSize: <Size_ extends number>(size: Size_) => Randomizable<DigitString<Size_>> = Individual.fromGene(Digit).andSize;

export const DigitString = {ofSize: ofSize};
