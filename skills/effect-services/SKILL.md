---
name: "effect-services"
description: >
  Teach agents how to structure Effect applications using Context.Service for dependency
  injection, Layer for implementations and resource lifecycle, service-driven development,
  and Config for type-safe configuration loading. Use when defining services, composing
  layers, providing dependencies, managing config, or structuring an Effect application's
  entry point — even if the user only says "set up services" or "wire up dependencies".
metadata:
  author: effect-solutions
  version: "0.1.0"
allowed-tools: ["bash", "read", "edit"]
---

# Effect Services — Context.Service, Layer, Config, Dependency Injection

## Skill Hierarchy

- **`effect-core`** (`skills/effect-core/SKILL.md`) — Effect.gen, Effect.fn, error handling fundamentals
- **`effect-schema`** (`skills/effect-schema/SKILL.md`) — Schema.Class, branded types, TaggedClass variants (used extensively in service data)
- **`effect-testing`** (`skills/effect-testing/SKILL.md`) — @effect/vitest testing patterns
- **`effect-ecosystem`** (`skills/effect-ecosystem/SKILL.md`) — HTTP clients, CLI, observability

## What is a Service?

A service is a `Context.Service` class that declares:
1. **A unique identifier** (e.g., `@app/Database`)
2. **An interface** with readonly method signatures

```typescript
import { Effect } from "effect"
import * as Context from "effect/Context"

class Database extends Context.Service<
  Database,
  {
    readonly query: (sql: string) => Effect.Effect<unknown[]>
    readonly execute: (sql: string) => Effect.Effect<void>
  }
>()("@app/Database") {}
```

Rules:
- Tag identifiers must be unique. Use `@path/to/ServiceName` prefix pattern
- Service methods should have no dependencies (`R = never`). Dependencies are handled via Layer composition
- Use readonly properties. Services should not expose mutable state directly

## What is a Layer?

A Layer is an implementation of a service. Layers handle setup/initialization, dependency resolution, and resource lifecycle (cleanup happens automatically).

```typescript
import { Effect, Layer } from "effect"
import * as Context from "effect/Context"

class Users extends Context.Service<
  Users,
  { readonly findById: (id: UserId) => Effect.Effect<User> }
>()("@app/Users") {
  static readonly layer = Layer.effect(
    Users,
    Effect.gen(function* () {
      const http = yield* HttpClient.HttpClient

      const findById = Effect.fn("Users.findById")(function* (id: UserId) {
        const response = yield* http.get(`https://api.example.com/users/${id}`)
        return yield* HttpClientResponse.schemaBodyJson(User)(response)
      })

      return { findById }
    })
  )
}
```

**Layer naming:** camelCase with `Layer` suffix: `layer`, `testLayer`, `postgresLayer`, `sqliteLayer`, etc.

**Layer constructors:**
- `Layer.effect` — async initialization with Effect
- `Layer.succeed` — sync value (no side effects)
- `Layer.sync` — lazy sync (side effects run once per layer construction)

## Service-Driven Development

Start by sketching leaf service tags (without implementations). This lets you write real TypeScript for higher-level orchestration that type-checks even with unimplemented leaf services:

```typescript
class Users extends Context.Service<
  Users, { readonly findById: (id: UserId) => Effect.Effect<User> }
>()("@app/Users") {}

class Tickets extends Context.Service<
  Tickets, { readonly issue: (eventId: EventId, userId: UserId) => Effect.Effect<Ticket> }
>()("@app/Tickets") {}

class Emails extends Context.Service<
  Emails, { readonly send: (to: string, subject: string, body: string) => Effect.Effect<void> }
>()("@app/Emails") {}

// Higher-level service: orchestrates leaf services
class Events extends Context.Service<
  Events, { readonly register: (eventId: EventId, userId: UserId) => Effect.Effect<Registration> }
>()("@app/Events") {
  static readonly layer = Layer.effect(
    Events,
    Effect.gen(function* () {
      const users = yield* Users
      const tickets = yield* Tickets
      const emails = yield* Emails

      const register = Effect.fn("Events.register")(function* (eventId: EventId, userId: UserId) {
        const user = yield* users.findById(userId)
        const ticket = yield* tickets.issue(eventId, userId)
        const now = yield* Clock.currentTimeMillis
        // ... orchestration logic
        return registration
      })

      return { register }
    })
  )
}
```

This pattern lets you model dependencies and write orchestration code before building production implementations.

## Providing Layers to Effects

**Provide once at the top of your application** — never scatter `provide` calls throughout the codebase.

```typescript
// Compose all layers into a single app layer
const appLayer = userServiceLayer.pipe(
  Layer.provideMerge(databaseLayer),
  Layer.provideMerge(loggerLayer),
  Layer.provideMerge(configLayer)
)

// Provide once at the entry point
const main = program.pipe(Effect.provide(appLayer))
Effect.runPromise(main)
```

Benefits of single provide point:
- Clear dependency graph: all wiring in one place
- Easier testing: swap `appLayer` for `testLayer`
- No hidden dependencies: effects declare what they need via types

## Layer Memoization

Effect automatically memoizes layers by reference identity. Same layer instance = constructed once.

**Anti-pattern — inline constructors create duplicates:**

```typescript
// WRONG: Creates TWO connection pools
const badAppLayer = Layer.merge(
  UserRepo.layer.pipe(Layer.provide(Postgres.layer({ url: "...", poolSize: 10 }))),
  OrderRepo.layer.pipe(Layer.provide(Postgres.layer({ url: "...", poolSize: 10 }))) // Different ref!
)
```

**Fix — store in a constant:**

```typescript
const postgresLayer = Postgres.layer({ url: "postgres://localhost/mydb", poolSize: 10 })

const goodAppLayer = Layer.merge(
  UserRepo.layer.pipe(Layer.provide(postgresLayer)),
  OrderRepo.layer.pipe(Layer.provide(postgresLayer)) // Same reference!
)
```

**Rule:** When using parameterized layer constructors, always store the result in a module-level constant before using it in multiple places.

## Per-Test vs Suite-Shared Layers

Default to fresh layers inside each `it.effect`:

```typescript
it.effect("starts at zero", () =>
  Effect.gen(function* () {
    const counter = yield* Counter
    expect(yield* counter.get()).toBe(0)
  }).pipe(Effect.provide(Counter.layer))
)
```

Use `it.layer` only for expensive shared resources (DB connections):

```typescript
it.layer(Counter.layer)("counter", (it) => {
  it.effect("starts at zero", () => /* ... */)
})
```

## Config

### Recommended Pattern: Config Service

```typescript
import { Config, Effect, Layer, Redacted } from "effect"
import * as Context from "effect/Context"

class ApiConfig extends Context.Service<
  ApiConfig,
  { readonly apiKey: Redacted.Redacted; readonly baseUrl: string; readonly timeout: number }
>()("@app/ApiConfig") {
  static readonly layer = Layer.effect(
    ApiConfig,
    Effect.gen(function* () {
      const apiKey = yield* Config.redacted("API_KEY")
      const baseUrl = yield* Config.string("API_BASE_URL").pipe(
        Config.orElse(() => Config.succeed("https://api.example.com"))
      )
      const timeout = yield* Config.int("API_TIMEOUT").pipe(
        Config.orElse(() => Config.succeed(30000))
      )
      return { apiKey, baseUrl, timeout }
    })
  )

  static readonly testLayer = Layer.succeed(ApiConfig, {
    apiKey: Redacted.make("test-key"),
    baseUrl: "https://test.example.com",
    timeout: 5000,
  })
}
```

**Why this pattern:**
- Separates config loading from business logic
- Easy to swap implementations (layer vs testLayer)
- Config errors caught early at layer composition

### Config Primitives

```typescript
Config.string("MY_VAR")
Config.number("PORT")
Config.int("MAX_RETRIES")
Config.boolean("DEBUG")
Config.redacted("API_KEY")      // Hidden in logs (<redacted>)
Config.url("API_URL")
Config.duration("TIMEOUT")
```

### Validation with Config.schema

```typescript
const Port = Schema.NumberFromString.pipe(
  Schema.check(Schema.isInt()),
  Schema.check(Schema.isBetween({minimum: 1, maximum: 65535}))
)
const port = yield* Config.schema(Port, "PORT")
```

Prefer `Config.schema` over `Config.mapOrFail` — automatic type inference, rich validation errors, reusable schemas.

### Config Providers

Override config sources:

```typescript
const testConfigLayer = ConfigProvider.layer(
  ConfigProvider.fromUnknown({ API_KEY: "test-key", PORT: "3000" })
)
const prefixedConfigLayer = ConfigProvider.layer(
  ConfigProvider.fromEnv().pipe(ConfigProvider.nested("APP")) // Reads APP_API_KEY, APP_PORT
)
```

### Usage in Tests

Provide values directly with `Layer.succeed` — no need for `ConfigProvider.fromMap`:

```typescript
Effect.runPromise(
  program.pipe(
    Effect.provide(Layer.succeed(ApiConfig, {
      apiKey: Redacted.make("test-key"),
      baseUrl: "https://test.example.com"
    }))
  )
)
```

### Redacted for Secrets

```typescript
const apiKey = yield* Config.redacted("API_KEY")
const headers = { Authorization: `Bearer ${Redacted.value(apiKey)}` }
// apiKey logs as <redacted>
```

## Common Anti-Patterns

1. **Scattering Effect.provide**: Provide all layers at the entry point, not inline throughout application code
2. **Inline layer constructors**: Always store parameterized layers in module-level constants
3. **Using Config.provider directly in tests**: Provide test layers with `Layer.succeed` instead
4. **Not including testLayer**: Every config service should expose a `static testLayer` for tests

## Import Conventions

```typescript
import { Config, Effect, Layer, Redacted } from "effect"
import * as Context from "effect/Context"
```