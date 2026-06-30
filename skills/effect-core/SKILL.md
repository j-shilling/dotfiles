---
name: "effect-core"
description: >
  Teach agents how to write idiomatic Effect TypeScript code using Effect.gen generators,
  Effect.fn traced functions, pipe-based instrumentation, retry/timeout patterns, and
  Schema-tagged error handling. Use when writing effectful TypeScript with the Effect
  framework — Effect.gen, Effect.fn, Effect.pipe, Schedule, Effect.retry, Effect.timeout,
  Effect.catchTag, Effect.catch, TaggedErrorClass, Schema.Defect, or "defects" — even if
  the user only says "write an Effect program" or "handle errors in Effect".
metadata:
  author: effect-solutions
  version: "0.1.0"
allowed-tools: ["bash", "read", "edit"]
---

# Effect Core — Effect.gen, Effect.fn, Pipe, Retry, Error Handling

## Skill Hierarchy

- **`effect-schema`** (`skills/effect-schema/SKILL.md`) — data modeling with Schema.Class, branded types, TaggedClass variants
- **`effect-services`** (`skills/effect-services/SKILL.md`) — Context.Service, Layer dependency injection, Config
- **`effect-testing`** (`skills/effect-testing/SKILL.md`) — @effect/vitest, TestClock, test layers
- **`effect-ecosystem`** (`skills/effect-ecosystem/SKILL.md`) — HTTP clients, CLI, observability, library wrapping

## TypeScript Configuration

Reference the config from the docs:
- `target: ES2022`, `module: NodeNext`, `moduleDetection: force`
- `verbatimModuleSyntax: true`, `rewriteRelativeImportExtensions: true`
- `strict: true`, `exactOptionalPropertyTypes: true`, `noUnusedLocals: true`, `noImplicitOverride: true`
- `incremental: true`, `composite: true` (fast rebuilds)
- `declarationMap: true`, `sourceMap: true`, `skipLibCheck: true`
- Rule of thumb: "Build tool compiling? Use `preserve` + `bundler`. TypeScript compiling? Use `NodeNext`."

## Effect.gen

The primary way to write sequential Effect code, analogous to async/await. Use generators with `yield*`:

```typescript
import { Effect } from "effect"

const program = Effect.gen(function* () {
  const data = yield* fetchData
  yield* Effect.logInfo(`Processing data: ${data}`)
  return yield* processData(data)
})
```

- `yield*` is the Effect equivalent of `await`
- Always use `Effect.gen(function* () { ... })` — never use bare generators
- No "nested .pipe() hell" — generators keep code readable

## Effect.fn

Use `Effect.fn` for named, traced effect functions. Provides call-site tracing, stack traces with location details, and clean signatures:

```typescript
import { Effect, flow, Schedule } from "effect"

const processUser = Effect.fn("processUser")(function* (userId: string) {
  yield* Effect.logInfo(`Processing user ${userId}`)
  const user = yield* getUser(userId)
  return yield* processData(user)
})
```

**Second argument pattern** — apply cross-cutting concerns to the whole function:

```typescript
const fetchWithTimeout = Effect.fn("fetchWithTimeout")(
  function* (url: string) {
    const data = yield* fetchData(url)
    return yield* processData(data)
  },
  flow(
    Effect.retry(Schedule.recurs(3)),
    Effect.timeout("5 seconds")
  )
)
```

**Benefits:**
- Call-site tracking (knows where the function was called from, not just defined)
- Automatic OpenTelemetry span creation
- Clean signatures — cross-cutting concerns stay outside the body

**Best practice:** Prefer `Effect.fn` over `Effect.gen` for any named operation. Reserve bare `Effect.gen` for one-off anonymous code (tests, simple scripts).

## Pipe for Instrumentation

Use `.pipe()` to add cross-cutting concerns. Common chain:

```typescript
import { Effect, Schedule } from "effect"

const program = fetchData.pipe(
  Effect.timeout("5 seconds"),
  Effect.retry(Schedule.exponential("100 millis").pipe(Schedule.both(Schedule.recurs(3)))),
  Effect.tap((data) => Effect.logInfo(`Fetched: ${data}`)),
  Effect.withSpan("fetchData")
)
```

**Common combinators:**
- `Effect.timeout` — fail if effect takes too long
- `Effect.retry` — retry on failure with a schedule
- `Effect.tap` — run a side effect without changing the value
- `Effect.withSpan` — add an OpenTelemetry tracing span

## Retry and Timeout

For production code, always combine retry and timeout:

```typescript
import { Effect, Schedule } from "effect"

const retryPolicy = Schedule.exponential("100 millis").pipe(
  Schedule.both(Schedule.recurs(3))
)

const resilientCall = callExternalApi.pipe(
  Effect.timeout("2 seconds"),      // timeout each attempt
  Effect.retry(retryPolicy),         // retry failed attempts
  Effect.timeout("10 seconds")       // overall timeout for all attempts
)
```

**Schedule combinators:**
- `Schedule.exponential` — exponential backoff (preferred for APIs)
- `Schedule.recurs` — limit number of retries
- `Schedule.spaced` — fixed delay between retries
- `Schedule.both` — combine two schedules (both must continue)

## Schema.TaggedErrorClass

Define structured, serializable errors with automatic `_tag` for pattern matching:

```typescript
import { Effect, Schema } from "effect"

class ValidationError extends Schema.TaggedErrorClass<ValidationError>()(
  "ValidationError",
  { field: Schema.String, message: Schema.String }
) {}

class NotFoundError extends Schema.TaggedErrorClass<NotFoundError>()(
  "NotFoundError",
  { resource: Schema.String, id: Schema.String }
) {}

const AppError = Schema.Union([ValidationError, NotFoundError])
type AppError = typeof AppError.Type
```

**Tagged errors are yieldable** — you can `yield*` them directly in a generator:

```typescript
class BadLuck extends Schema.TaggedErrorClass<BadLuck>()(
  "BadLuck",
  { roll: Schema.Number }
) {}

const rollDie = Effect.gen(function* () {
  const roll = yield* Random.nextIntBetween(1, 6)
  if (roll === 1) {
    yield* new BadLuck({ roll })  // No Effect.fail needed
  }
  return { roll }
})
```

**Benefits:**
- Serializable (can send over network, save to DB)
- Type-safe
- Built-in `_tag` for pattern matching
- Sensible default `message` when you don't declare one

## Error Recovery

### catchTag — Handle specific errors by _tag:

```typescript
const recovered: Effect.Effect<string, ValidationError> = program.pipe(
  Effect.catchTag("HttpError", (error) =>
    Effect.gen(function* () {
      yield* Effect.logWarning(`HTTP ${error.statusCode}: ${error.message}`)
      return "Recovered from HttpError"
    })
  )
)
```

### catchTags — Handle multiple errors at once:

```typescript
const recovered = program.pipe(
  Effect.catchTags({
    HttpError: () => Effect.succeed("Recovered from HttpError"),
    ValidationError: () => Effect.succeed("Recovered from ValidationError")
  })
)
```

### catch — Handle all remaining errors:

```typescript
const recovered = program.pipe(
  Effect.catch((error) => Effect.succeed(`Recovered from ${error._tag}`))
)
```

## Expected Errors vs Defects

A major architectural distinction:

| Type | Use for | Example |
|------|---------|---------|
| Typed errors (`E` type param) | Domain failures callers can handle | validation, not-found, permission denied, rate limits |
| Defects | Unrecoverable situations | bugs, invariant violations, config failures |

```typescript
// At app entry: if config fails, nothing can proceed
const main = Effect.gen(function* () {
  const config = yield* loadConfig.pipe(Effect.orDie)
  yield* Effect.log(`Starting on port ${config.port}`)
})
```

**When to catch defects:** Almost never. Only at system boundaries for logging/diagnostics.

## Schema.Defect — Wrapping Unknown Errors

Wrap errors from external libraries (fetch, axios, etc.):

```typescript
class ApiError extends Schema.TaggedErrorClass<ApiError>()("ApiError", {
  endpoint: Schema.String,
  statusCode: Schema.Number,
  error: Schema.Defect,
}) {}

const fetchUser = (id: string) =>
  Effect.tryPromise({
    try: () => fetch(`/api/users/${id}`).then((r) => r.json()),
    catch: (error) => new ApiError({
      endpoint: `/api/users/${id}`,
      statusCode: 500,
      error
    })
  })
```

**Schema.Defect handles:**
- JavaScript `Error` instances -> `{ name, message }` objects
- Any unknown value -> string representation
- Serializable for network/storage

## Common Anti-Patterns

1. **Not using Effect.fn**: Bare `Effect.gen` misses call-site tracing and OTel spans
2. **Catching defects routinely**: Don't catch defects — they represent bugs. Use `orDie` to convert unrecoverable errors to defects at boundaries
3. **Nested Effect.gen without Effect.fn**: Wrapping sequential operations in anonymous generators loses traceability
4. **Missing retry+timeout**: Production code should always combine retry and per-attempt timeout

## Import Conventions

```typescript
import { Effect, Schedule, flow } from "effect"
import * as Context from "effect/Context"
import { Match, Schema } from "effect"
```

- Prefer named imports from `"effect"` for the most common types
- Use namespace imports (`import * as`) for submodules like `Context`
- Use scoped subpath imports for ecosystem modules: `"effect/unstable/http"`, `"effect/unstable/cli"`