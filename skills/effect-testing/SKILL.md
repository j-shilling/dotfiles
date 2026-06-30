---
name: "effect-testing"
description: >
  Teach agents how to test Effect TypeScript code using @effect/vitest — it.effect
  for native Effect tests, TestClock for deterministic time, Layer provisioning for
  dependency injection in tests, and the service-spy pattern for assertions. Use when
  writing tests for Effect code, setting up vitest for an Effect project, or mocking
  Effect services — even if the user only says "write tests" or "test this Effect code".
metadata:
  author: effect-solutions
  version: "0.1.0"
allowed-tools: ["bash", "read", "edit"]
---

# Effect Testing — @effect/vitest, TestClock, Test Layers, Service Spies

## Skill Hierarchy

- **`effect-core`** (`skills/effect-core/SKILL.md`) — Effect.gen, Effect.fn, error handling fundamentals
- **`effect-schema`** (`skills/effect-schema/SKILL.md`) — Schema.Class, branded types, TaggedClass variants
- **`effect-services`** (`skills/effect-services/SKILL.md`) — Context.Service, Layer, Config (testing draws heavily on these)
- **`effect-ecosystem`** (`skills/effect-ecosystem/SKILL.md`) — HTTP clients, CLI, observability

## Why @effect/vitest?

- Native Effect support — `it.effect()` runs Effect programs directly
- Automatic scoped resource cleanup
- Test services: TestClock, TestRandom for deterministic tests
- Detailed fiber failure reporting with causes, spans, and logs
- Layer support via `Effect.provide()`

## Setup

```bash
bun add -D vitest @effect/vitest@beta
```

package.json:
```json
{
  "scripts": {
    "test": "vitest run",
    "test:watch": "vitest"
  }
}
```

vitest.config.ts:
```typescript
import { defineConfig } from "vitest/config"

export default defineConfig({
  test: {
    include: ["tests/**/*.test.ts"],
  },
})
```

Run tests with `bun run test` (not `bun test`).

## Basic Testing

```typescript
import { Effect } from "effect"
import { describe, expect, it } from "@effect/vitest"

describe("Calculator", () => {
  // Sync test (optional — you can use regular vitest too)
  it("creates instances", () => {
    const result = 1 + 1
    expect(result).toBe(2)
  })

  // Effect test
  it.effect("adds numbers", () =>
    Effect.gen(function* () {
      const result = yield* Effect.succeed(1 + 1)
      expect(result).toBe(2)
    })
  )
})
```

## Test Function Variants

### it.effect() — most common, auto-provides TestContext

```typescript
it.effect("processes data", () =>
  Effect.gen(function* () {
    const result = yield* processData("input")
    expect(result).toBe("expected")
  })
)
```

Scoped resources are cleaned up automatically when the test ends.

### it.live() — real system clock (no TestClock)

```typescript
it.live("real clock", () =>
  Effect.gen(function* () {
    const now = yield* Clock.currentTimeMillis
    expect(now).toBeGreaterThan(0) // Actual system time
  })
)
```

### Modifiers

```typescript
it.effect.skip("disabled test", () => /* won't run */)
it.effect.only("focused test", () => /* only this runs */)
it.effect.fails("expected failure", () => Effect.succeed("but expect fail"))
```

### TestClock — deterministic time control

`it.effect` automatically provides TestContext with TestClock. Use `TestClock.adjust`:

```typescript
import { Effect, Fiber } from "effect"
import { TestClock } from "effect/testing"

it.effect("time-based test", () =>
  Effect.gen(function* () {
    const fiber = yield* Effect.delay(Effect.succeed("done"), "10 seconds").pipe(
      Effect.forkChild
    )
    yield* TestClock.adjust("10 seconds")
    const result = yield* Fiber.join(fiber)
    expect(result).toBe("done")
  })
)
```

## Providing Layers in Tests

### Inline layer provisioning (default — preferred)

```typescript
const testDatabase = Layer.succeed(Database, {
  query: (_sql) => Effect.succeed(["mock", "data"])
})

it.effect("queries database", () =>
  Effect.gen(function* () {
    const db = yield* Database
    const results = yield* db.query("SELECT * FROM users")
    expect(results.length).toBe(2)
  }).pipe(Effect.provide(testDatabase))
)
```

### Layer.provideMerge — expose leaf services for test setup/assertions

```typescript
const testLayer = Events.layer.pipe(
  Layer.provideMerge(Users.testLayer),
  Layer.provideMerge(Tickets.testLayer),
  Layer.provideMerge(Emails.testLayer)
)

it.effect("creates registration", () =>
  Effect.gen(function* () {
    // Arrange — use leaf services exposed by provideMerge
    const users = yield* Users
    yield* users.create(new User({ ... }))

    // Act — use the service under test
    const events = yield* Events
    const registration = yield* events.register(eventId, user.id)

    // Assert
    expect(registration.userId).toBe(user.id)
  }).pipe(Effect.provide(testLayer))
)
```

**The Email-Spy Pattern**: A convenient pattern for test assertions. Expose both the mutating method AND a read-only `sent` method on the test layer:

```typescript
class Emails extends Context.Service<
  Emails,
  {
    readonly send: (email: Email) => Effect.Effect<void>
    readonly sent: Effect.Effect<ReadonlyArray<Email>>
  }
>()("@app/Emails") {
  static readonly testLayer = Layer.sync(Emails, () => {
    const emails: Array<Email> = []
    return {
      send: (email: Email) => Effect.sync(() => void emails.push(email)),
      sent: Effect.sync(() => emails),
    }
  })
}
```

Then in tests:

```typescript
it.effect("sends confirmation email", () =>
  Effect.gen(function* () {
    const emails = yield* Emails
    yield* events.register(eventId, userId)
    const sentEmails = yield* emails.sent
    expect(sentEmails).toHaveLength(1)
    expect(sentEmails[0]!.subject).toBe("Event Registration Confirmed")
  }).pipe(Effect.provide(testLayer))
)
```

## Suite-Shared Layers (it.layer)

Default to per-test layering. Use `it.layer` only for expensive resources:

```typescript
it.layer(Counter.layer)("counter", (it) => {
  it.effect("starts at zero", () =>
    Effect.gen(function* () {
      const counter = yield* Counter
      expect(yield* counter.get()).toBe(0)
    })
  )

  it.effect("increments", () =>
    Effect.gen(function* () {
      const counter = yield* Counter
      yield* counter.increment()
      expect(yield* counter.get()).toBe(1)
    })
  )
})
```

Warning: state leaks between tests with shared layers. If you're not sure, skip `it.layer`.

## Logging in Tests

Logging is suppressed by default in `it.effect`. To enable:

```typescript
// Option 1: Provide a logger layer
it.effect("with logging", () =>
  Effect.gen(function* () {
    yield* Effect.log("This will be shown")
  }).pipe(Effect.provide(Logger.layer([Logger.consolePretty()])))
)

// Option 2: Use it.live (logging enabled by default)
it.live("live with logging", () =>
  Effect.gen(function* () {
    yield* Effect.log("This will be shown")
  })
)
```

## Test Layer Guidelines

| Pattern | When to use |
|---------|-------------|
| `Layer.sync(Service, () => { ... })` | Test layers with mutable state (stores, counters, spies) |
| `Layer.succeed(Service, { ... })` | Test layers with fixed/stateless values (config, constants) |
| Inline `Effect.provide(layer)` per test | Default — state isolation between tests |
| `it.layer(Service.layer)` | Only for expensive shared resources |

Test layers can use mutable state safely (JS is single-threaded). Each test gets a fresh instance when using inline provisioning.

## Running Tests

```bash
bun run test              # Run all tests
bun run test:watch        # Watch mode
bunx vitest run tests/user.test.ts  # Specific file
bunx vitest run -t "UserService"    # Pattern match
```

## Common Anti-Patterns

1. **Suite-shared layers by default**: Fresh layers per test prefer state isolation
2. **No testLayer on services**: Every service should have a test companion
3. **Mocking ConfigProvider directly**: Use `Layer.succeed(Service, { values })` instead
4. **Testing with real time**: Use `TestClock.adjust` for determinism
5. **Complex setup outside tests**: Use `it.beforeEach` or inline generators

## Import Conventions

```typescript
import { describe, expect, it } from "@effect/vitest"
import { Effect, Layer } from "effect"
import * as Context from "effect/Context"
```