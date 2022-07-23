import { Effect } from "@effect-ts/core";
import { pipe } from "@effect-ts/system/Function";
import { Logger } from "../Logger";

export const $console: Logger = {
  debug: (message, ...args) =>
    pipe(
      Effect.unit,
      Effect.map(() => console.debug(message, ...args))
    ),
  info: (message, ...args) =>
    pipe(
      Effect.unit,
      Effect.map(() => console.log(message, ...args))
    ),
  notice: (message, ...args) =>
    pipe(
      Effect.unit,
      Effect.map(() => console.info(message, ...args))
    ),
  warning: (message, ...args) =>
    pipe(
      Effect.unit,
      Effect.map(() => console.warn(message, ...args))
    ),
  error: (message, ...args) =>
    pipe(
      Effect.unit,
      Effect.map(() => console.error(message, ...args))
    ),
  critical: (message, ...args) =>
    pipe(
      Effect.unit,
      Effect.map(() => console.error(message, ...args))
    ),
  alert: (message, ...args) =>
    pipe(
      Effect.unit,
      Effect.map(() => console.error(message, ...args))
    ),
  emergency: (message, ...args) =>
    pipe(
      Effect.unit,
      Effect.map(() => console.error(message, ...args))
    ),
};
