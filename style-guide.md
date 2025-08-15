# Effect TypeScript Style Guide

A comprehensive guide for developing projects within the Effect ecosystem, focusing on functional programming, type safety, and effective concurrency patterns.

## Table of Contents

1. [Project Structure](#project-structure)
2. [TypeScript Configuration](#typescript-configuration)
3. [Code Style and Formatting](#code-style-and-formatting)
4. [Naming Conventions](#naming-conventions)
5. [Module Organization](#module-organization)
6. [Effect Data Types](#effect-data-types)
7. [Functional Programming Patterns](#functional-programming-patterns)
8. [Effect-Specific Guidelines](#effect-specific-guidelines)
9. [Type Safety Practices](#type-safety-practices)
10. [Testing and Build](#testing-and-build)
11. [Documentation Standards](#documentation-standards)
12. [Code Review Guidelines for Effect Projects](#code-review-guidelines-for-effect-projects)
13. [Further Documentation and Resources](#further-documentation-and-resources)

## Project Structure

### Monorepo Organization
- Use a monorepo structure with `packages/` directory
- Each package should have its own `package.json`, `tsconfig.json`, and documentation
- Organize packages by domain (e.g., `effect`, `platform`, `cli`, `experimental`)

### Package Structure
```
packages/
├── package-name/
│   ├── src/
│   │   ├── index.ts          # Main barrel export
│   │   ├── ModuleName.ts     # Individual modules
│   │   └── internal/         # Internal implementations
│   ├── test/                 # Test files
│   ├── examples/             # Usage examples
│   ├── package.json
│   ├── tsconfig.json
│   ├── README.md
│   └── CHANGELOG.md
```

### File Naming
- Use PascalCase for module files: `Array.ts`, `Option.ts`, `Effect.ts`
- Use camelCase for utility/internal files: `array.js`, `option.js`
- Use kebab-case for configuration files: `tsconfig.build.json`
- Test files should match their source: `Array.test.ts`

## TypeScript Configuration

### Base Configuration
```json
{
  "compilerOptions": {
    "strict": true,
    "exactOptionalPropertyTypes": true,
    "moduleDetection": "force",
    "composite": true,
    "downlevelIteration": true,
    "resolveJsonModule": true,
    "esModuleInterop": false,
    "declaration": true,
    "skipLibCheck": true,
    "moduleResolution": "NodeNext",
    "lib": ["ES2022", "DOM", "DOM.Iterable"],
    "target": "ES2022",
    "module": "NodeNext",
    "isolatedModules": true,
    "noUnusedLocals": true,
    "noFallthroughCasesInSwitch": true,
    "forceConsistentCasingInFileNames": true
  }
}
```

### Key TypeScript Rules
- Enable `strict` mode and `exactOptionalPropertyTypes`
- Use `NodeNext` for module resolution
- Target ES2022 for modern JavaScript features
- Always enable `isolatedModules` for build performance
- Use composite projects for monorepo builds

## Code Style and Formatting

### ESLint Configuration
- Use `@effect/eslint-plugin` for Effect-specific rules
- Configure dprint for consistent formatting:
  ```json
  {
    "indentWidth": 2,
    "lineWidth": 120,
    "semiColons": "asi",
    "quoteStyle": "alwaysDouble",
    "trailingCommas": "never",
    "operatorPosition": "maintain",
    "arrowFunction.useParentheses": "force"
  }
  ```

### Style Rules
- **Indentation**: 2 spaces
- **Line Width**: 120 characters maximum
- **Semicolons**: ASI (Automatic Semicolon Insertion)
- **Quotes**: Always use double quotes
- **Trailing Commas**: Never use trailing commas
- **Arrow Functions**: Always use parentheses around parameters

### Import Organization
- Group imports by type: external dependencies first, then internal modules
- Use `import * as ModuleName` for namespace imports
- Sort destructured keys alphabetically
- No duplicate imports

### Restrictions
- **No console.log**: Prohibited in `src/` and `test/` directories
- **No spread in Array.push**: Avoid `arr.push(...items)`
- **Prefer object shorthand**: Use `{ name }` instead of `{ name: name }`

## Naming Conventions

### Modules and Namespaces
- **Module names**: PascalCase (`Array`, `Option`, `Effect`)
- **Namespace exports**: `export * as ModuleName from "./ModuleName.js"`
- **File extensions**: Always use `.js` in imports (TypeScript compilation target)

### Functions and Variables
- **Function names**: camelCase (`map`, `flatMap`, `fromIterable`)
- **Variable names**: camelCase (`result`, `maybeValue`, `userInput`)
- **Constants**: UPPER_SNAKE_CASE for module-level constants
- **Private/Internal**: Prefix with underscore `_internal` or place in `internal/` directory

### Types and Interfaces
- **Type names**: PascalCase (`ReadonlyArray`, `NonEmptyArray`)
- **Interface names**: PascalCase, no `I` prefix
- **Generic parameters**: Single uppercase letters (`A`, `B`, `E`, `R`)
- **Brand types**: Use descriptive names (`UserId`, `PositiveNumber`)

### Error Types
- **Error classes**: End with `Error` (`ParseError`, `ValidationError`)
- **Error modules**: Use descriptive names (`ConfigError`, `HttpClientError`)

## Module Organization

### Barrel Exports
- Use `index.ts` as the main export point for each package
- Export modules as namespaces: `export * as Array from "./Array.js"`
- Include comprehensive JSDoc documentation for each export

### Internal Structure
- Place implementation details in `internal/` directory
- Separate public API from implementation
- Use dual APIs (data-first and data-last) for better ergonomics

### Export Patterns
```typescript
// Public API
export * as Array from "./Array.js"
export * as Option from "./Option.js"

// Individual exports for common utilities
export {
  pipe,
  flow,
  identity
} from "./Function.js"
```

## Effect Data Types

### Core Data Type Replacements

Effect provides functional, immutable alternatives to standard JavaScript data types. These should be preferred in Effect-based projects:

#### Collections

| Standard JavaScript | Effect Alternative | Use Case |
|---------------------|-------------------|----------|
| `Array<T>` | `Array<T>` (Effect) | General-purpose immutable arrays with functional operations |
| `Array<T>` (non-empty) | `NonEmptyArray<T>` | Arrays guaranteed to have at least one element |
| `Array<T>` (readonly) | `ReadonlyArray<T>` | Immutable array references |
| `Set<T>` | `HashSet<T>` | Immutable sets with fast lookup (O(1) average) |
| `Set<T>` (mutable) | `MutableHashSet<T>` | Mutable sets for performance-critical scenarios |
| `Map<K, V>` | `HashMap<K, V>` | Immutable key-value maps |
| `Map<K, V>` (mutable) | `MutableHashMap<K, V>` | Mutable maps for incremental building |
| `Object` (as map) | `Record<K, V>` (Effect) | String-keyed immutable records |
| Linked List | `List<T>` | Immutable linked lists optimal for LIFO operations |
| Queue | `Queue<T>` | Immutable queues for FIFO operations |

#### Specialized Collections

| Purpose | Effect Type | Description |
|---------|-------------|-------------|
| Ordered Sets | `SortedSet<T>` | Sets that maintain element ordering |
| Ordered Maps | `SortedMap<K, V>` | Maps that maintain key ordering |
| String-based Maps | `Trie<V>` | Optimized for string keys with prefix operations |
| Binary Trees | `RedBlackTree<K, V>` | Self-balancing binary search trees |
| Chunked Arrays | `Chunk<T>` | Append-optimized immutable sequences |

#### Optional and Error Handling

| Standard JavaScript | Effect Alternative | Use Case |
|---------------------|-------------------|----------|
| `T \| null \| undefined` | `Option<T>` | Explicit optional values with safe operations |
| `T \| undefined` (union) | `Option<T>` | Type-safe nullable operations |
| `try/catch` patterns | `Either<E, A>` | Explicit error handling without exceptions |
| Promise rejection | `Effect<A, E, R>` | Comprehensive error handling with context |

#### Concurrency and Effects

| Standard JavaScript | Effect Alternative | Use Case |
|---------------------|-------------------|----------|
| `Promise<T>` | `Effect<T, E, R>` | Composable effects with error and dependency tracking |
| `AsyncIterable<T>` | `Stream<T, E, R>` | Streaming data with backpressure and error handling |
| Manual resource cleanup | `Scope` | Automatic resource management |
| `setTimeout`/`setInterval` | `Schedule<A, B>` | Declarative scheduling and retrying |

#### Synchronization Primitives

| Standard JavaScript | Effect Alternative | Use Case |
|---------------------|-------------------|----------|
| Manual state management | `Ref<A>` | Thread-safe mutable references |
| Shared mutable state | `STM` (Software Transactional Memory) | Composable concurrent state |
| Event emitters | `PubSub<A>` | Type-safe publish-subscribe |
| Manual locking | `Semaphore`, `Mutex` | Controlled resource access |

### Data Type Selection Guidelines

#### When to Use Effect Types

**✅ Prefer Effect types when:**
- Building functional, immutable applications
- Need comprehensive error handling
- Working with concurrent or asynchronous operations
- Require type-safe optional value handling
- Building composable, testable systems

**⚠️ Consider standard types when:**
- Interfacing with external libraries that expect standard types
- Performance is critical and mutability is acceptable
- Working in a mixed codebase with non-Effect code

#### Migration Patterns

**From Arrays to Effect Collections:**
```typescript
// Before: Standard JavaScript
const items: string[] = []
items.push("item")
const filtered = items.filter(x => x.length > 3)

// After: Effect
import { Array } from "effect"
let items = Array.empty<string>()
items = Array.append(items, "item")  
const filtered = Array.filter(items, x => x.length > 3)
```

**From null/undefined to Option:**
```typescript
// Before: Nullable types
function findUser(id: string): User | null {
  // implementation
}

const user = findUser("123")
if (user !== null) {
  console.log(user.name)
}

// After: Option
import { Option } from "effect"

function findUser(id: string): Option<User> {
  // implementation  
}

const user = findUser("123")
Option.match(user, {
  onNone: () => console.log("User not found"),
  onSome: (u) => console.log(u.name)
})
```

**From Promises to Effects:**
```typescript
// Before: Promise-based
async function fetchData(url: string): Promise<Data> {
  const response = await fetch(url)
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}`)
  }
  return response.json()
}

// After: Effect-based
import { Effect } from "effect"

function fetchData(url: string): Effect<Data, FetchError, never> {
  return Effect.tryPromise({
    try: () => fetch(url).then(r => r.ok ? r.json() : Promise.reject(new Error(`HTTP ${r.status}`))),
    catch: (error) => new FetchError({ cause: error })
  })
}
```

### Type Interoperability

#### Converting Between Types
```typescript
// Array conversions
const jsArray: number[] = [1, 2, 3]
const effectArray = Array.fromIterable(jsArray)
const backToJs = Array.toArray(effectArray)

// Option conversions  
const maybeValue = Option.fromNullable(possiblyNull)
const backToNullable = Option.getOrNull(maybeValue)

// HashMap conversions
const jsMap = new Map([["a", 1], ["b", 2]])
const effectMap = HashMap.fromIterable(jsMap)
const backToJs = new Map(HashMap.toEntries(effectMap))
```

## Functional Programming Patterns

### Avoid Point-Free Style
**❌ Avoid:**
```typescript
Effect.map(fn) // Point-free style
```

**✅ Prefer:**
```typescript
Effect.map((x) => fn(x)) // Explicit lambda
```

**Reasons:**
- Better type inference
- Clearer stack traces
- Avoids generic erasure issues
- More explicit and readable

### Use Dual APIs
Provide both data-first and data-last versions:

```typescript
// Data-first (direct usage)
const result = Effect.map(effect, (x) => x * 2)

// Data-last (with pipe)
const result = pipe(
  effect,
  Effect.map((x) => x * 2)
)
```

### Prefer Do Notation for Complex Workflows
**For simple chains:**
```typescript
const result = pipe(
  getValue(),
  Effect.map((x) => x * 2),
  Effect.flatMap(processValue)
)
```

**For complex workflows:**
```typescript
const result = Effect.gen(function* () {
  const x = yield* getValue()
  const y = yield* processValue(x * 2)
  const z = yield* computeResult(y)
  return z
})
```

### Pattern Matching
Use structured pattern matching over if/else chains:

```typescript
const result = pipe(
  value,
  Match.type<Event>(),
  Match.tag("success", (event) => `Success: ${event.data}`),
  Match.tag("error", (event) => `Error: ${event.error}`),
  Match.exhaustive
)
```

## Effect-Specific Guidelines

### Runtime Management
- Use platform-specific runtimes:
  - Node.js: `NodeRuntime.runMain`
  - Bun: `BunRuntime.runMain`
  - Browser: `BrowserRuntime.runMain`

- Always handle graceful shutdown:
```typescript
import { NodeRuntime } from "@effect/platform-node"

const program = Effect.gen(function* () {
  // Your application logic
  return yield* Effect.succeed("Hello, World!")
})

NodeRuntime.runMain(program)
```

### Error Handling
- Use `Effect<A, E, R>` type signature consistently
- Prefer specific error types over generic `Error`
- Use `Cause` for comprehensive error information
- Handle both expected errors and defects appropriately

### Service Dependencies
- Define services using `Context.GenericTag`
- Use `Layer` for dependency injection
- Compose layers for complex service graphs
- Prefer constructor functions over classes

### Concurrency
- Use `Effect.fork` for background operations
- Employ `Fiber.interrupt` for graceful cancellation
- Use `Scope` for resource management
- Leverage `STM` for software transactional memory

## Type Safety Practices

### Branded Types
Use branded types for type safety:

```typescript
type UserId = number & Brand.Brand<"UserId">
const UserId = Brand.nominal<UserId>()

type PositiveInt = number & Brand.Brand<"PositiveInt">
const PositiveInt = Brand.refined<PositiveInt>(
  (n) => Number.isInteger(n) && n > 0,
  (n) => Brand.error(`Expected ${n} to be a positive integer`)
)
```

### Schema Validation
- Use `Schema.Data` for automatic `Equal` and `Hash` implementation
- Validate external data at boundaries
- Prefer schema-driven development for APIs

### Generic Constraints
- Use meaningful generic parameter names
- Apply appropriate constraints with `extends`
- Document generic parameters in JSDoc

### Type-Level Programming
- Use `HKT` (Higher Kinded Types) for advanced abstractions
- Employ conditional types judiciously
- Prefer readability over cleverness

## Testing and Build

### Test Structure
- Use `vitest` for testing framework
- Place tests in `test/` directories
- Follow naming convention: `ModuleName.test.ts`
- Use `@effect/vitest` for Effect-specific testing utilities

### Build Process
```json
{
  "scripts": {
    "build": "tsc -b tsconfig.build.json",
    "test": "vitest",
    "lint": "eslint \"**/{src,test,examples,scripts,dtslint}/**/*.{ts,mjs}\"",
    "check": "tsc -b tsconfig.json"
  }
}
```

### Package Management
- Use `pnpm` for package management
- Maintain workspace configuration
- Use `changeset` for version management
- Implement proper dependency resolution

## Documentation Standards

### JSDoc Requirements
- Document all public APIs with JSDoc
- Include `@since` tags for version tracking
- Provide `@example` blocks for complex functions
- Document generic parameters and return types
- Use Effect-specific JSDoc tags for organization

### Effect-Specific JSDoc Tags

Effect uses several non-standard JSDoc tags for better documentation organization:

#### Core Documentation Tags

| Tag | Purpose | Usage |
|-----|---------|--------|
| `@since` | Version tracking | `@since 2.0.0` - Required for all public APIs |
| `@category` | Functional grouping | `@category constructors`, `@category transformations` |
| `@example` | Code examples | `@example` - Multi-line code blocks with imports |
| `@internal` | Internal APIs | `@internal` - Marks implementation details |
| `@experimental` | Unstable APIs | `@experimental` - APIs subject to change |
| `@deprecated` | Deprecated APIs | `@deprecated` - APIs scheduled for removal |
| `@macro` | Compile-time expansion | `@macro` - Functions expanded at compile time |
| `@optimize` | Performance hints | `@optimize` - Optimization annotations |

#### Organizational Categories

**Common `@category` values used in Effect:**

- **Type-related:**
  - `symbol` - Type symbols and identifiers
  - `models` - Type definitions and interfaces  
  - `type lambdas` - Higher-kinded type definitions
  - `Type Lambdas` - Alternative capitalization
  - `type-level` - Type-level utilities and extractors
  - `Type-level Utils` - Type utilities for end users

- **API Organization:**
  - `constructors` - Functions that create instances
  - `refinements` - Type guard functions  
  - `guards` - Type checking predicates
  - `getters` - Property access functions
  - `elements` - Element-specific operations
  - `predicates` - Boolean-returning functions

- **Functional Operations:**
  - `mapping` - Transform operations (`map`, `flatMap`)
  - `filtering` - Selection operations (`filter`, `partition`)  
  - `sequencing` - Composition operations (`flatMap`, `andThen`)
  - `traversing` - Iteration operations (`forEach`, `reduce`)
  - `folding` - Aggregation operations (`reduce`, `fold`)
  - `transformations` - General data transformations
  - `mutations` - State-changing operations
  - `conversions` - Type conversion functions

- **Data Structure Operations:**
  - `concatenating` - Combining operations (`concat`, `append`)
  - `partitioning` - Splitting operations (`partition`, `separate`)
  - `sorting` - Ordering operations (`sort`, `sortBy`)
  - `zipping` - Pairing operations (`zip`, `zipWith`)
  - `grouping` - Collection grouping operations

- **Utilities:**
  - `utils` - General utility functions
  - `equivalence` - Equality comparison functions
  - `math` - Mathematical operations
  - `comparison` - Comparison utilities
  - `instances` - Type class instances
  - `errors` - Error-related functionality

### Module Documentation
```typescript
/**
 * This module provides utility functions for working with arrays in TypeScript.
 *
 * @since 2.0.0
 */
export * as Array from "./Array.js"
```

### Function Documentation
```typescript
/**
 * Creates a new array with the results of calling a provided function
 * on every element in the calling array.
 *
 * @param self - The array to map over
 * @param f - Function that produces an element of the new array
 * @returns A new array with each element being the result of the callback function
 * @since 2.0.0
 * @category mapping
 * @example
 * import { Array } from "effect"
 *
 * const numbers = [1, 2, 3]
 * const doubled = Array.map(numbers, (n) => n * 2)
 * assert.deepStrictEqual(doubled, [2, 4, 6])
 */
```

### Type Definition Documentation
```typescript
/**
 * Represents an immutable linked list of elements of type `A`.
 *
 * A `List` is optimal for last-in-first-out (LIFO), stack-like access patterns.
 *
 * @since 2.0.0
 * @category models
 */
export type List<A> = Cons<A> | Nil<A>

/**
 * @since 2.0.0
 * @category symbol
 */
export const TypeId: unique symbol = Symbol.for("effect/List")

/**
 * This type-level utility extracts the value type `V` from a `HashMap<K, V>` type.
 *
 * @example
 * ```ts
 * import { HashMap } from "effect"
 *
 * declare const hm: HashMap.HashMap<string, number>
 *
 * // $ExpectType number
 * type V = HashMap.HashMap.Value<typeof hm>
 * ```
 * @since 2.0.0
 * @category type-level
 */
export type Value<T extends HashMap<any, any>> = [T] extends [HashMap<infer _K, infer _V>] ? _V : never
```

### Internal API Documentation
```typescript
/**
 * @internal
 */
export const internalImplementation = () => {
  // Implementation details not exposed in public API
}
```

### Experimental API Documentation  
```typescript
/**
 * Advanced execution planning for Effects.
 * 
 * @since 3.16.0
 * @experimental
 * @category models
 */
export interface ExecutionPlan<A, E, R> {
  // Experimental API subject to change
}
```

### Documentation Best Practices

1. **Always include `@since`** for public APIs
2. **Use appropriate `@category`** for organization
3. **Provide realistic `@example`** blocks with imports
4. **Mark experimental features** with `@experimental`  
5. **Document internal APIs** with `@internal`
6. **Include type-level utilities** with proper examples
7. **Use consistent terminology** across similar functions
8. **Document performance characteristics** when relevant

### README Structure
- Clear description and purpose
- Installation instructions
- Basic usage examples
- Links to comprehensive documentation
- Contributing guidelines

### Changelog Maintenance
- Use semantic versioning
- Document breaking changes clearly
- Include migration guides for major versions
- Reference relevant issue/PR numbers

## Best Practices Summary

### Do's
- ✅ Use explicit lambda functions instead of point-free style
- ✅ Provide dual APIs for better ergonomics
- ✅ Use branded types for domain modeling
- ✅ Implement proper error handling with Effect types
- ✅ Use platform-specific runtimes
- ✅ Document all public APIs thoroughly
- ✅ Follow the established naming conventions
- ✅ Use Do notation for complex Effect workflows
- ✅ Leverage pattern matching for conditional logic
- ✅ Implement proper resource management with Scope

### Don'ts
- ❌ Don't use point-free style that compromises type inference
- ❌ Don't ignore error handling or use generic Error types
- ❌ Don't use console.log in source code
- ❌ Don't create barrel imports from individual packages
- ❌ Don't forget to handle resource cleanup
- ❌ Don't mix imperative and functional styles
- ❌ Don't skip JSDoc documentation for public APIs
- ❌ Don't use any or unknown types without justification
- ❌ Don't ignore ESLint rules without proper reasoning

## Further Documentation and Resources

### Official Documentation
- **Effect Website**: [https://effect.website](https://effect.website) - Comprehensive guides, tutorials, and API documentation
- **GitHub Repository**: [https://github.com/Effect-TS/effect](https://github.com/Effect-TS/effect) - Source code, issues, and contributions

### LLM-Optimized Documentation

Effect provides specialized documentation formats optimized for Large Language Models:

| Resource | URL | Purpose |
|----------|-----|---------|
| **Full Documentation** | `https://effect.website/llms-full.txt` | Complete Effect documentation in LLM-friendly format |
| **Compressed Documentation** | `https://effect.website/llms-small.txt` | Essential Effect information in condensed format |
| **Standard LLM Format** | `https://effect.website/llms.txt` | Standard llm.txt format documentation |

These resources are designed to help AI assistants provide accurate and up-to-date information about Effect APIs, patterns, and best practices.

#### Using LLM Documentation
```bash
# Download for local AI tools
curl -O https://effect.website/llms-full.txt
curl -O https://effect.website/llms-small.txt
curl -O https://effect.website/llms.txt
```

### MCP Server for Effect Documentation

The **Effect MCP Server** provides real-time access to Effect documentation through the Model Context Protocol:

- **Repository**: [https://github.com/tim-smart/effect-mcp](https://github.com/tim-smart/effect-mcp)
- **Purpose**: Allows AI assistants to look up current Effect documentation, API references, and examples
- **Benefits**: 
  - Always up-to-date information
  - Real-time API lookups
  - Integration with Claude and other MCP-compatible tools

#### Installing Effect MCP Server
```bash
# Install via npm
npm install -g effect-mcp

# Or use with Claude Code directly
# Add to your MCP configuration
```

### Development Resources

#### Community and Support
- **Discord**: Join the Effect community for discussions and support
- **GitHub Discussions**: For questions, feature requests, and community input
- **Stack Overflow**: Tag questions with `effect-ts` for community help

#### Learning Materials
- **Effect Workshop**: Interactive tutorials and exercises
- **Blog Posts**: Regular updates on new features and patterns
- **Video Tutorials**: Community-created learning content
- **Example Projects**: Real-world Effect applications and patterns

#### Tools and Integrations
- **VS Code Extension**: Enhanced TypeScript support for Effect
- **ESLint Plugin**: `@effect/eslint-plugin` for code quality
- **Vitest Integration**: `@effect/vitest` for testing Effect code
- **OpenTelemetry**: `@effect/opentelemetry` for observability

### API Reference Integration

When working with Effect, consider integrating these documentation sources into your development workflow:

1. **IDE Integration**: Configure your IDE to access Effect documentation
2. **AI Assistant Setup**: Use MCP server for real-time documentation lookup
3. **Local Documentation**: Keep LLM documentation files for offline reference
4. **Team Documentation**: Share Effect patterns and conventions within your organization

## Code Review Guidelines for Effect Projects

### Error Handling Patterns

#### ✅ Preferred Error Patterns

**Use Schema.TaggedError or Data.TaggedError for domain errors:**
```typescript
// ✅ Good: Tagged errors with structured data
class ValidationError extends Schema.TaggedError<ValidationError>()(
  "ValidationError",
  { 
    field: Schema.String,
    message: Schema.String 
  }
) {}

class DatabaseError extends Data.TaggedError("DatabaseError")<{
  readonly query: string
  readonly cause: unknown
}> {}
```

**Use specific error types instead of generic Error:**
```typescript
// ❌ Avoid: Generic errors
Effect.fail(new Error("Something went wrong"))

// ✅ Prefer: Specific tagged errors  
Effect.fail(new ValidationError({ field: "email", message: "Invalid format" }))
```

**Handle errors explicitly with Effect.catchTag or Effect.catchAll:**
```typescript
// ✅ Good: Explicit error handling
const program = pipe(
  riskyOperation(),
  Effect.catchTag("DatabaseError", (error) => 
    Effect.logError(`DB Error: ${error.query}`)
  ),
  Effect.catchTag("ValidationError", (error) =>
    Effect.succeed({ error: error.message })
  )
)
```

#### ❌ Anti-patterns to Flag

- Using generic `Error` instead of tagged errors
- Throwing exceptions instead of using `Effect.fail`
- Not handling specific error types
- Using `Effect.die` for recoverable errors

### Service and Dependency Injection Patterns

#### ✅ Preferred Service Patterns

**Use Context.GenericTag for service definitions:**
```typescript
// ✅ Good: Proper service definition
class UserService extends Context.Tag("UserService")<
  UserService,
  {
    findById: (id: string) => Effect.Effect<User, UserNotFound, never>
    create: (user: CreateUser) => Effect.Effect<User, ValidationError, never>
  }
>() {}
```

**Use Layer constructors appropriately:**
```typescript
// ✅ Good: Layer patterns
const UserServiceLive = Layer.effect(
  UserService,
  Effect.gen(function* () {
    const db = yield* DatabaseService
    return UserService.of({
      findById: (id) => db.query(`SELECT * FROM users WHERE id = ?`, [id]),
      create: (user) => pipe(
        validateUser(user),
        Effect.flatMap(db.insert)
      )
    })
  })
)

// ✅ Good: Simple service layers
const ConfigLive = Layer.succeed(Config, { apiUrl: "https://api.example.com" })

// ✅ Good: Scoped resources
const DatabaseLive = Layer.scoped(
  Database,
  Effect.acquireRelease(
    openConnection(),
    (conn) => closeConnection(conn)
  )
)
```

**Compose layers properly:**
```typescript
// ✅ Good: Layer composition
const AppLive = Layer.mergeAll(
  DatabaseLive,
  UserServiceLive,
  EmailServiceLive
).pipe(
  Layer.provide(ConfigLive)
)
```

#### ❌ Service Anti-patterns to Flag

- Creating services without proper Context.Tag
- Not using Layer for dependency injection
- Manual service instantiation instead of Effect.service
- Missing resource cleanup in scoped layers

### Testing Patterns

#### ✅ Preferred Testing Approaches

**Use @effect/vitest for Effect-based tests:**
```typescript
import { describe, it, expect } from "@effect/vitest"

// ✅ Good: Effect test patterns
it.effect("should process user data", () =>
  Effect.gen(function* () {
    const result = yield* processUser({ name: "Alice" })
    expect(result.processed).toBe(true)
  })
)

// ✅ Good: Scoped tests with resources
it.scoped("should handle database operations", () =>
  Effect.gen(function* () {
    const db = yield* Database
    const user = yield* db.createUser({ name: "Bob" })
    expect(user.id).toBeDefined()
  })
)
```

**Create test layers for dependencies:**
```typescript
// ✅ Good: Test service implementations
const TestDatabaseLive = Layer.succeed(Database, {
  query: () => Effect.succeed([]),
  insert: () => Effect.succeed({ id: "test-id" })
})

const TestAppLive = Layer.provide(AppLive, TestDatabaseLive)
```

#### ❌ Testing Anti-patterns to Flag

- Using regular async/await instead of Effect in tests
- Not providing test implementations for services
- Testing implementation details instead of behavior
- Not testing error paths

### Performance and Optimization

#### ✅ Performance Best Practices

**Avoid point-free style for better performance:**
```typescript
// ❌ Avoid: Point-free can hurt performance
pipe(data, Array.map(processItem))

// ✅ Prefer: Explicit lambdas
pipe(data, Array.map((item) => processItem(item)))
```

**Use Effect.gen for complex workflows:**
```typescript
// ✅ Good: Generator syntax for readability and performance
const workflow = Effect.gen(function* () {
  const user = yield* fetchUser(id)
  const permissions = yield* fetchPermissions(user.id)  
  const result = yield* processWithPermissions(user, permissions)
  return result
})
```

**Use appropriate data structures:**
```typescript
// ✅ Good: Use HashMap for key-value operations
const cache = HashMap.fromIterable(entries)
const value = HashMap.get(cache, key)

// ❌ Avoid: Array.find for lookups
const value = array.find(item => item.key === key)
```

**Batch operations when possible:**
```typescript
// ✅ Good: Batch effects
const results = yield* Effect.all([
  fetchUser(id1),
  fetchUser(id2), 
  fetchUser(id3)
])

// ❌ Avoid: Sequential operations when parallel is possible
const user1 = yield* fetchUser(id1)
const user2 = yield* fetchUser(id2)
const user3 = yield* fetchUser(id3)
```

### Code Organization and Structure

#### ✅ Organizational Best Practices

**Separate concerns properly:**
```typescript
// ✅ Good: Clear separation
// services/UserService.ts
export class UserService extends Context.Tag("UserService")<...> {}

// layers/UserServiceLive.ts  
export const UserServiceLive = Layer.effect(UserService, ...)

// errors/UserErrors.ts
export class UserNotFound extends Schema.TaggedError<...> {}
```

**Use proper module boundaries:**
```typescript
// ✅ Good: Clean exports
export * as UserService from "./UserService.js"
export * as UserErrors from "./UserErrors.js"

// ❌ Avoid: Barrel exports of implementation details
export { internalUserLogic } from "./internal/user.js"
```

### Code Review Checklist

#### Error Handling ✓
- [ ] Are domain errors modeled as tagged errors?
- [ ] Is error handling explicit and comprehensive?
- [ ] Are generic Error types replaced with specific ones?
- [ ] Are error messages informative and actionable?

#### Services & Dependencies ✓
- [ ] Are services properly tagged with Context.Tag?
- [ ] Are layers used for dependency injection?
- [ ] Is resource cleanup handled in scoped layers?
- [ ] Are service dependencies clearly declared?

#### Testing ✓
- [ ] Are tests using @effect/vitest patterns?
- [ ] Are test services properly mocked/stubbed?
- [ ] Are both success and error paths tested?
- [ ] Are tests independent and deterministic?

#### Performance ✓
- [ ] Is point-free style avoided where it hurts performance?
- [ ] Are appropriate data structures used for operations?
- [ ] Are effects batched when possible?
- [ ] Is Effect.gen used for complex workflows?

#### Type Safety ✓
- [ ] Are branded types used for domain modeling?
- [ ] Are Option types used instead of null/undefined?
- [ ] Are Effect types properly annotated with error types?
- [ ] Are generic constraints appropriate and meaningful?

#### Documentation ✓
- [ ] Are all public APIs documented with JSDoc?
- [ ] Are @since tags present for version tracking?
- [ ] Are @category tags used for organization?
- [ ] Are examples provided for complex functions?

#### Code Style ✓
- [ ] Does code follow ESLint configuration?
- [ ] Are imports organized and deduplicated?
- [ ] Are naming conventions followed consistently?
- [ ] Is indentation and formatting consistent?

### Common Code Smells to Flag

1. **Effect Leakage**: Using Effect types in places where plain values suffice
2. **Over-abstraction**: Creating unnecessary layers or services for simple operations
3. **Missing Error Handling**: Operations that can fail but don't specify error types
4. **Resource Leaks**: Acquired resources not properly released
5. **Test Pollution**: Tests that don't clean up after themselves
6. **Type Any Usage**: Using `any` types without proper justification
7. **Console Logging**: Using `console.log` in production code
8. **Manual Promise Handling**: Using Promise APIs instead of Effect equivalents

---

This style guide reflects the patterns and practices used throughout the Effect ecosystem. Following these guidelines will help maintain consistency, improve code quality, and enhance the developer experience across all Effect-related projects.