---
name: "effect-schema"
description: >
  Teach agents how to model domain data with Effect Schema — records via Schema.Class,
  variants via Schema.TaggedClass + Schema.Union, branded primitives, exhaustive pattern
  matching with Match.value, and JSON serialization with Schema.fromJsonString. Use when
  defining data models, domain types, discriminated unions, branded types, or doing JSON
  parsing/validation in Effect projects — even if the user only says "define a schema"
  or "model this domain".
metadata:
  author: effect-solutions
  version: "0.1.0"
allowed-tools: ["bash", "read", "edit"]
---

# Effect Schema — Data Modeling with Schema.Class, TaggedClass, Branded Types, Match

## Skill Hierarchy

- **`effect-core`** (`skills/effect-core/SKILL.md`) — Effect.gen, Effect.fn, error handling fundamentals
- **`effect-services`** (`skills/effect-services/SKILL.md`) — Context.Service, Layer DI, Config (uses Schema for config validation and service data)
- **`effect-testing`** (`skills/effect-testing/SKILL.md`) — @effect/vitest testing patterns
- **`effect-ecosystem`** (`skills/effect-ecosystem/SKILL.md`) — HTTP clients, CLI, observability

## Why Schema?

- **Single source of truth**: define once, get TypeScript types + runtime validation + JSON serialization
- **Parse safely**: validate API/config/CLI data with detailed errors
- **Rich domain types**: branded primitives prevent type confusion; classes add methods
- **Ecosystem integration**: same schema works across HTTP, CLI, frontend, backend

## Foundations

All data is composed of two primitives:
- **Records** (AND/Product types): a `User` has a name AND email AND createdAt
- **Variants** (OR/Sum types): a `Result` is a Success OR a Failure

## Records — Schema.Class

Use `Schema.Class` for composite data models. Add custom getters and methods:

```typescript
import { Schema } from "effect"

const UserId = Schema.String.pipe(Schema.brand("UserId"))
type UserId = typeof UserId.Type

export class User extends Schema.Class<User>("User")({
  id: UserId,
  name: Schema.String,
  email: Schema.String,
  createdAt: Schema.Date,
}) {
  get displayName() {
    return `${this.name} (${this.email})`
  }
}

// Usage
const user = new User({
  id: UserId.make("user-123"),
  name: "Alice",
  email: "alice@example.com",
  createdAt: new Date(),
})
```

## Variants — Schema.TaggedClass + Schema.Union

For simple alternatives, use `Schema.Literals`:

```typescript
const Status = Schema.Literals(["pending", "active", "completed"])
type Status = typeof Status.Type  // "pending" | "active" | "completed"
```

For structured variants with fields, use `Schema.TaggedClass` + `Schema.Union`:

```typescript
import { Match, Schema } from "effect"

export class Success extends Schema.TaggedClass<Success>("Success")("Success", {
  value: Schema.Number,
}) {}

export class Failure extends Schema.TaggedClass<Failure>("Failure")("Failure", {
  error: Schema.String,
}) {}

export const Result = Schema.Union([Success, Failure])
export type Result = typeof Result.Type

const renderResult = (result: Result) =>
  Match.value(result).pipe(
    Match.tag("Success", ({ value }) => `Got: ${value}`),
    Match.tag("Failure", ({ error }) => `Error: ${error}`),
    Match.exhaustive,
  )
```

**Always use `Match.exhaustive`** — the compiler ensures all cases are handled.

## Branded Types

**In a well-designed domain model, nearly all primitives should be branded.** Not just IDs, but emails, URLs, timestamps, slugs, counts, and any value with semantic meaning.

Simple branding:

```typescript
export const UserId = Schema.String.pipe(Schema.brand("UserId"))
export type UserId = typeof UserId.Type
```

Constrained branding (with runtime validation):

```typescript
export const Port = Schema.Int.pipe(
  Schema.check(Schema.isBetween({minimum: 1, maximum: 65535})),
  Schema.brand("Port")
)
export type Port = typeof Port.Type
```

**Benefits:**
- TypeScript prevents passing `PostId` where `UserId` is expected
- Runtime validation for constrained types (ports, emails, etc.)
- Self-documenting code

**Common branded types:**

```typescript
export const UserId = Schema.String.pipe(Schema.brand("UserId"))
export const PostId = Schema.String.pipe(Schema.brand("PostId"))
export const Email = Schema.String.pipe(Schema.brand("Email"))
export const Slug = Schema.String.pipe(Schema.brand("Slug"))
```

## JSON Encoding & Decoding

Use `Schema.fromJsonString` to combine `JSON.parse` + schema validation in one step:

```typescript
import { Effect, Schema } from "effect"

class Position extends Schema.Class<Position>("Position")({
  row: Schema.Literals(["A", "B", "C", "D", "E", "F", "G", "H"]),
  column: Schema.Literals(["1", "2", "3", "4", "5", "6", "7", "8"]),
}) {}

class Move extends Schema.Class<Move>("Move")({
  from: Position,
  to: Position,
}) {}

const MoveFromJson = Schema.fromJsonString(Move)

const program = Effect.gen(function* () {
  // Parse and validate JSON string in one step
  const jsonString = '{"from":{"row":"A","column":"1"},"to":{"row":"B","column":"2"}}'
  const move = yield* Schema.decodeUnknownEffect(MoveFromJson)(jsonString)

  // Encode back to JSON string
  const json = yield* Schema.encodeEffect(MoveFromJson)(move)
  return json
})
```

**Key distinction:** Use `MoveFromJson` (not `Move`) when decoding from or encoding to JSON strings. Use `Move` when working with parsed objects.

## Import Conventions

```typescript
import { Schema } from "effect"
import { Match } from "effect"
```

## Common Anti-Patterns

1. **Not branding primitives**: Using `string` instead of `UserId`, `Email`, etc. — causes type confusion bugs
2. **Manual JSON.parse**: Use `Schema.fromJsonString` instead of separate `JSON.parse` + decode
3. **Non-exhaustive matching**: Using `if/else` on variants instead of `Match.value` + `Match.exhaustive`
4. **Schema.Struct over Schema.Class**: Prefer `Schema.Class` so you get constructor, methods, and `instanceof`