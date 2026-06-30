---
name: "effect-ecosystem"
description: >
  Teach agents how to use Effect's ecosystem modules — typed HTTP clients with
  HttpClient and Schema, CLI applications with Command/Argument/Flag, OpenTelemetry
  observability with @effect/opentelemetry, and the use pattern for wrapping third-party
  Promise libraries. Use when making HTTP requests, building command-line tools, adding
  OpenTelemetry tracing, or integrating Promise-based libraries in Effect projects —
  even if the user only says "make an API call" or "build a CLI".
metadata:
  author: effect-solutions
  version: "0.1.0"
allowed-tools: ["bash", "read", "edit"]
---

# Effect Ecosystem — HTTP Clients, CLI, Observability, Library Wrapping

## Skill Hierarchy

- **`effect-core`** (`skills/effect-core/SKILL.md`) — Effect.gen, Effect.fn, error handling fundamentals
- **`effect-schema`** (`skills/effect-schema/SKILL.md`) — Schema.Class, branded types, JSON serialization (used extensively in HTTP and CLI)
- **`effect-services`** (`skills/effect-services/SKILL.md`) — Context.Service, Layer, Config
- **`effect-testing`** (`skills/effect-testing/SKILL.md`) — @effect/vitest testing patterns

## HTTP Clients

### Minimal Example

```typescript
import { FetchHttpClient, HttpClient, HttpClientResponse } from "effect/unstable/http"
import { Effect, Schema } from "effect"

const Repo = Schema.Struct({
  id: Schema.Number,
  name: Schema.String,
  full_name: Schema.String,
  stargazers_count: Schema.Number,
})

const program = Effect.gen(function* () {
  const response = yield* HttpClient.get("https://api.github.com/repos/Effect-TS/effect")
  const repo = yield* HttpClientResponse.schemaBodyJson(Repo)(response)
  console.log(`${repo.full_name}: ${repo.stargazers_count} stars`)
})

program.pipe(Effect.provide(FetchHttpClient.layer), Effect.runPromise)
```

### Building Requests

**Headers:**
```typescript
HttpClientRequest.get(url).pipe(
  HttpClientRequest.setHeader("Accept", "application/vnd.github.v3+json"),
  HttpClientRequest.bearerToken("ghp_xxxx"),
  HttpClientRequest.basicAuth(user, pass)
)
```

**Query params:**
```typescript
HttpClientRequest.get("https://api.github.com/search/repositories").pipe(
  HttpClientRequest.setUrlParam("q", "effect language:typescript"),
  HttpClientRequest.setUrlParam("sort", "stars")
)
```

**JSON body (returns Effect — encoding can fail):**
```typescript
const request = yield* HttpClientRequest.post(url).pipe(
  HttpClientRequest.schemaBodyJson(CreateIssue)(data)
)
```

### Response Decoding

**Decode JSON body:**
```typescript
HttpClientResponse.schemaBodyJson(User)(response)
```

**Handle different status codes:**
```typescript
HttpClientResponse.matchStatus(response, {
  "2xx": HttpClientResponse.schemaBodyJson(User),
  404: () => Effect.fail(new UserNotFound(username)),
  orElse: (r) => Effect.fail(new Error(`Unexpected status: ${r.status}`)),
})
```

**Filter 2xx only:**
```typescript
yield* HttpClientResponse.filterStatusOk(response)
return yield* HttpClientResponse.schemaBodyJson(User)(response)
```

### Client Middleware

Apply transformations to all requests:

```typescript
const GitHubClient = Layer.effect(
  HttpClient.HttpClient,
  Effect.gen(function* () {
    const baseClient = yield* HttpClient.HttpClient
    return baseClient.pipe(
      HttpClient.mapRequest(
        flow(
          HttpClientRequest.prependUrl("https://api.github.com"),
          HttpClientRequest.bearerToken("ghp_xxxx"),
        )
      )
    )
  })
).pipe(Layer.provide(FetchHttpClient.layer))
```

### Error Handling

Error types: `RequestError` (network/DNS/timeout) and `ResponseError` (non-2xx or parsing failures).

```typescript
program.pipe(
  Effect.catchTag("RequestError", (e) => Effect.fail(`Network: ${e.reason}`)),
  Effect.catchTag("ResponseError", (e) => Effect.fail(`HTTP ${e.response.status}`))
)
```

### Retry

Standard retry or `HttpClient.retryTransient` for transient errors only:

```typescript
const ResilientClient = Layer.effect(
  HttpClient.HttpClient,
  Effect.gen(function* () {
    const client = yield* HttpClient.HttpClient
    return client.pipe(HttpClient.retryTransient({ times: 3 }))
  })
).pipe(Layer.provide(FetchHttpClient.layer))
```

### GitHub API Service Pattern

Define a typed API as a Context.Service:

```typescript
class GitHubApi extends Context.Service<
  GitHubApi,
  {
    readonly getUser: (username: string) => Effect.Effect<User, unknown>
    readonly getRepo: (owner: string, repo: string) => Effect.Effect<Repo, unknown>
  }
>()("GitHubApi") {
  static layer = Layer.effect(
    GitHubApi,
    Effect.gen(function* () {
      const baseClient = yield* HttpClient.HttpClient
      const client = baseClient.pipe(
        HttpClient.mapRequest(HttpClientRequest.prependUrl("https://api.github.com"))
      )

      const getUser = Effect.fn("GitHubApi.getUser")(function* (username: string) {
        const response = yield* client.get(`/users/${username}`)
        return yield* HttpClientResponse.schemaBodyJson(User)(response)
      })

      return { getUser, getRepo }
    })
  )

  static live = GitHubApi.layer.pipe(Layer.provide(FetchHttpClient.layer))
}
```

## CLI Applications

### Minimal Example

```typescript
import { Argument, Command, Flag } from "effect/unstable/cli"
import { BunServices, BunRuntime } from "@effect/platform-bun"
import { Console, Effect } from "effect"

const name = Argument.string("name").pipe(Argument.withDefault("World"))
const shout = Flag.boolean("shout").pipe(Flag.withAlias("s"))

const greet = Command.make("greet", { name, shout }, ({ name, shout }) => {
  const message = `Hello, ${name}!`
  return Console.log(shout ? message.toUpperCase() : message)
})

const program = Command.run(greet, { version: "1.0.0" })
program.pipe(Effect.provide(BunServices.layer), BunRuntime.runMain)
```

### Arguments and Flags

**Arguments** are positional. **Flags** are named options. Flags must come before arguments.

```typescript
Argument.string("file")
Argument.string("output").pipe(Argument.optional)
Argument.string("format").pipe(Argument.withDefault("json"))
Argument.string("files").pipe(Argument.variadic())
Argument.string("files").pipe(Argument.atLeast(1))
Argument.integer("id").pipe(Argument.withSchema(TaskId))

Flag.boolean("verbose").pipe(Flag.withAlias("v"))
Flag.string("output").pipe(Flag.withAlias("o"))
Flag.choice("format", ["json", "yaml", "toml"])
Flag.integer("count").pipe(Flag.withDefault(10))
```

### Subcommands

```typescript
const add = Command.make("add", { task }, ({ task }) => Console.log(`Adding: ${task}`))
const list = Command.make("list", {}, () => Console.log("Listing tasks..."))

const app = Command.make("tasks").pipe(
  Command.withDescription("A simple task manager"),
  Command.withSubcommands([add, list])
)

const program = Command.run(app, { version: "1.0.0" })
```

### CLI + Service Integration

Commands access services via the Effect context:

```typescript
const addCommand = Command.make("add", { text }, ({ text }) =>
  Effect.gen(function* () {
    const repo = yield* TaskRepo
    const task = yield* repo.add(text)
    yield* Console.log(`Added task #${task.id}: ${task.text}`)
  })
)

const app = Command.make("tasks").pipe(Command.withSubcommands([addCommand]))

const mainLayer = Layer.merge(TaskRepo.layer, BunServices.layer)
Command.run(app, { version: "1.0.0" }).pipe(
  Effect.provide(mainLayer),
  BunRuntime.runMain
)
```

Built-in `--help` and `--version` work automatically.

## Observability (OpenTelemetry)

### Setup

```typescript
import { Otlp } from "@effect/opentelemetry"
import { Effect, Layer } from "effect"

const program = Effect.gen(function* () {
  // ... application code
}).pipe(
  Effect.provide(Otlp.layer)
)
```

### Custom Spans

```typescript
import { Effect, Tracer } from "effect"

// Effect.withSpan for individual effects
const result = yield* fetchData.pipe(Effect.withSpan("fetchData"))

// Effect.fn creates spans automatically
const processUser = Effect.fn("processUser")(function* (id: string) {
  // This function body is automatically traced
})
```

## The `use` Pattern (Library Wrapping)

When wrapping a Promise-based third-party library (Prisma, Drizzle, AWS SDK, etc.), use the `use` callback pattern:

```typescript
class FileSystemError extends Schema.TaggedErrorClass<FileSystemError>()(
  "FileSystemError",
  { operation: Schema.String, error: Schema.Defect }
) {}

class FileSystem extends Context.Service<
  FileSystem,
  {
    readonly use: <A>(
      fn: (fs: typeof import("node:fs/promises"), signal: AbortSignal) => Promise<A>
    ) => Effect.Effect<A, FileSystemError>
  }
>()("@app/FileSystem") {
  static readonly layer = Layer.effect(
    FileSystem,
    Effect.gen(function* () {
      const use = <A>(
        fn: (fs: typeof fs_, signal: AbortSignal) => Promise<A>
      ) =>
        Effect.tryPromise({
          try: (signal) => fn(import("node:fs/promises"), signal),
          catch: (error) => new FileSystemError({ operation: "use", error }),
        })

      return { use }
    })
  )
}
```

**Benefits:**
- Automatic error wrapping
- Interruption support via AbortSignal
- Encapsulation — consumers can't use the client outside Effect

**Testing:**
```typescript
static readonly testLayer = Layer.sync(FileSystem, () => {
  const store = new Map<string, string>()
  return {
    use: (fn) => Effect.tryPromise(() => fn(mockFs, new AbortController().signal)),
  }
})
```

For few-method libraries, wrap individual methods with `Effect.tryPromise` instead.

## Import Conventions

```typescript
// HTTP
import { FetchHttpClient, HttpClient, HttpClientRequest, HttpClientResponse } from "effect/unstable/http"

// CLI
import { Argument, Command, Flag } from "effect/unstable/cli"
import { BunServices, BunRuntime } from "@effect/platform-bun"

// Observability
import { Otlp } from "@effect/opentelemetry"
import { Tracer } from "effect"

// Platform
import { FileSystem } from "effect"
import { NodeFileSystem } from "@effect/platform-node"
```

## Summary Tables

### HTTP Client API

| Concept | API |
|---------|------|
| Simple GET | `HttpClient.get(url)` |
| Execute request | `HttpClient.execute(request)` |
| Build request | `HttpClientRequest.get/post/put/del` |
| Set headers | `.setHeader`, `.bearerToken`, `.basicAuth` |
| Query params | `.setUrlParam` |
| JSON body | `.schemaBodyJson(Schema)(data)` |
| Decode response | `HttpClientResponse.schemaBodyJson(Schema)` |
| Status matching | `HttpClientResponse.matchStatus(response, { ... })` |
| Filter 2xx | `HttpClientResponse.filterStatusOk` |
| Base URL | `HttpClient.mapRequest(HttpClientRequest.prependUrl(url))` |
| Retry transient | `HttpClient.retryTransient({ times: 3 })` |
| Provide client | `FetchHttpClient.layer` |

### CLI API

| Concept | API |
|---------|------|
| Define command | `Command.make(name, config, handler)` |
| Positional args | `Argument.string`, `.integer`, `.optional`, `.variadic()` |
| Named flags | `Flag.boolean`, `.string`, `.choice` |
| Flag alias | `Flag.withAlias("v")` |
| Subcommands | `Command.withSubcommands([...])` |
| Run CLI | `Command.run(cmd, { version })` |
| Platform | `BunServices.layer` or `NodeServices.layer` |

## Common Anti-Patterns

1. **No retry on HTTP calls**: Always retry transient failures in production
2. **Manual URL construction**: Use `HttpClientRequest.prependUrl` for base URLs
3. **Exposing raw clients**: Wrap third-party libraries behind Context.Service
4. **No span annotations**: Use `Effect.withSpan` or `Effect.fn` for trace visibility
5. **CLI handlers with direct side effects**: Use services from context for testable code