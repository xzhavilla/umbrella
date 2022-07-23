import { Array, Effect, Record } from "@effect-ts/core";

interface LogFunction {
  //   <T extends Record.Dictionary<unknown>>(
  //     context: T,
  //     message?: string,
  //     ...args: Array.Array<any>
  //   ): Effect.UIO<void>;
  (message: string, ...args: Array.Array<any>): Effect.UIO<void>;
}

export interface Logger {
  readonly debug: LogFunction;
  readonly info: LogFunction;
  readonly notice: LogFunction;
  readonly warning: LogFunction;
  readonly error: LogFunction;
  readonly critical: LogFunction;
  readonly alert: LogFunction;
  readonly emergency: LogFunction;
}
