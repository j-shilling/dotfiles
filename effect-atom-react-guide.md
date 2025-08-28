# @effect-atom/atom and @effect-atom/atom-react Guide

A comprehensive guide for creating React applications with Effect-TS state management using atoms.

## Overview

The `@effect-atom/atom` and `@effect-atom/atom-react` libraries provide a reactive state management solution for React applications built on Effect-TS. Atoms represent reactive computation nodes that can change over time, similar to reactive queries but more powerful since they work with both synchronous and asynchronous state.

**Note**: This library was previously named `@effect-rx/rx` but was renamed to `@effect-atom/atom` between versions, so older documentation may reference "RX" concepts which are now "atoms".

## Core Concepts

### Atoms

An **atom** is a reactive computation node that represents a value that can change over time. When an atom changes, any subscribers (React components) are automatically updated.

Key characteristics:
- **Lazy evaluation**: Computation doesn't start until first access
- **Automatic caching**: Results are cached and shared across consumers
- **Service integration**: Can access Effect-TS services through runtime
- **Read-only by default**: Atoms are immutable unless explicitly made writable

### Atom Runtime

An **atom runtime** provides the services and configuration needed by atoms. Each runtime:
- Provides necessary Effect-TS services (API clients, loggers, etc.)
- Has its own lifecycle tied to consumers
- Uses memoization to share service instances efficiently
- Cleans up resources when no longer needed

## Installation

```bash
npm install @effect-atom/atom @effect-atom/atom-react
```

## Setup

### 1. Registry Provider

Wrap your application with the `RegistryProvider` to provide global atom registry access:

```tsx
import { RegistryProvider } from '@effect-atom/atom-react'

function App() {
  return (
    <RegistryProvider>
      {/* Your app components */}
    </RegistryProvider>
  )
}
```

### 2. Runtime Creation

Create a runtime factory for your atoms:

```ts
// atoms/runtime.ts
import { Atom } from '@effect-atom/atom'
import { Layer } from 'effect'

export const makeAtomRuntime = Atom.context(Atom.defaultMemoMap).runtime

// Create runtime with services
export const runtime = makeAtomRuntime(
  Layer.mergeAll(
    APIClient.Default,
    Logger.pretty,
    Logger.minimumLogLevel(
      process.env.NODE_ENV === 'dev' ? LogLevel.Debug : LogLevel.Info
    )
  )
)
```

### 3. Service Definition

Define your services using Effect-TS patterns:

```ts
// services/APIClient.ts
import { Effect, Service } from 'effect'
import { HttpClient } from '@effect/platform'

export class APIClient extends Service.make(Service.Tag<APIClient>()('APIClient'), {
  dependencies: Service.Dependencies.none,
  factory: () => ({
    http: yield* HttpClient.make({
      baseUrl: process.env.API_URL,
      transformClient: HttpClient.retryTransient({
        times: 3,
        schedule: Schedule.exponential(100)
      })
    })
  })
}) {}
```

## Creating Atoms

### Read-only Atoms

#### Async Atoms (with Effects)

```ts
// atoms/styles.ts
import { runtime } from './runtime'

// Remote data source
const stylesRemoteAtom = runtime.atom(Effect.gen(function*() {
  const api = yield* APIClient
  return yield* api.http.styles.list()
}))

export { stylesRemoteAtom }
```

#### Sync Atoms

```ts
import { Atom } from '@effect-atom/atom'

// Simple value atom
const counterAtom = Atom.make(0)

// Computed atom
const doubledCounterAtom = Atom.make((get) => get(counterAtom) * 2)
```

### Writable Atoms

Writable atoms allow state mutations and provide encapsulated actions:

```ts
// atoms/styles.ts
import { Data } from 'effect'

// Define action types
const StyleAction = Data.TaggedEnum<{
  Upsert: { payload: UpsertStylePayload }
  Delete: { id: StyleId }
}>()

// Create writable atom
export const stylesAtom = Atom.writable(
  (get) => get(stylesRemoteAtom),
  (context, action: StyleAction.Type) => {
    const result = context.get(stylesAtom)
    if (!Result.isSuccess(result)) return
    
    const updated = StyleAction.match(action, {
      Delete: ({ id }) => 
        result.value.filter(style => style.id !== id),
      Upsert: ({ payload }) => {
        const existing = result.value.find(s => s.id === payload.id)
        return existing 
          ? result.value.map(s => s.id === payload.id ? { ...s, ...payload } : s)
          : [payload as Style, ...result.value]
      }
    })
    
    context.setSelf(Result.success(updated))
  }
)

// Expose remote atom for refreshing
Object.assign(stylesAtom, { remote: stylesRemoteAtom })
```

### Atoms with Toast Integration

Create a higher-order function for adding toast notifications:

```ts
// atoms/withToast.ts
import { toast } from 'sonner' // or your toast library

type ToastOptions<A, E, Args extends readonly unknown[]> = {
  onWaiting: string | ((...args: Args) => string)
  onSuccess: string | ((result: A, ...args: Args) => string)  
  onFailure: string | ((error: Option.Option<E>, ...args: Args) => string)
}

export const withToast = <A, E, Args extends readonly unknown[]>(
  options: ToastOptions<A, E, Args>
) => (self: Effect.Effect<A, E, never>) => 
  Effect.gen(function*() {
    const toastId = toast.loading(
      typeof options.onWaiting === 'string' 
        ? options.onWaiting 
        : options.onWaiting(...args)
    )
    
    const result = yield* self.pipe(
      Effect.tapError(cause => Effect.sync(() => 
        toast.error(
          typeof options.onFailure === 'string'
            ? options.onFailure
            : options.onFailure(Cause.firstFailure(cause), ...args),
          { id: toastId }
        )
      ))
    )
    
    toast.success(
      typeof options.onSuccess === 'string'
        ? options.onSuccess  
        : options.onSuccess(result, ...args),
      { id: toastId }
    )
    
    return result
  })

// Usage
const upsertStyleAtom = runtime.fn((payload: UpsertStylePayload) =>
  Effect.gen(function*() {
    const api = yield* APIClient
    const result = yield* api.http.styles.upsert({ payload }).pipe(
      withToast({
        onWaiting: (p) => p.id ? 'Updating style...' : 'Creating style...',
        onSuccess: 'Style saved!',
        onFailure: 'Failed to save style'
      })
    )
    
    // Update cache optimistically
    const registry = yield* Atom.Registry
    registry.set(stylesAtom, StyleAction.Upsert({ payload: result }))
    
    return result
  })
)
```

## Using Atoms in React Components

### Reading Atom Values

```tsx
import { useAtomValue } from '@effect-atom/atom-react'
import { Result } from 'effect'

function StylesList() {
  const stylesResult = useAtomValue(stylesAtom)
  
  return Result.match(stylesResult, {
    onInitial: ({ waiting }) => waiting && <div>Loading...</div>,
    onSuccess: ({ value: styles, waiting }) => (
      <div>
        {waiting && <div>Refreshing...</div>}
        {styles.map(style => (
          <StyleItem key={style.id} style={style} />
        ))}
      </div>
    ),
    onFailure: ({ cause }) => <div>Error: {Cause.prettyPrint(cause)}</div>
  })
}
```

### Using Result Builder Pattern

```tsx
import { ResultBuilder } from '@effect-atom/atom'

function StylesList() {
  const stylesResult = useAtomValue(stylesAtom)
  
  return ResultBuilder(stylesResult)
    .onWaiting(result => 
      result.isInitial && result.waiting ? <div>Loading...</div> : null
    )
    .onSuccess(styles => 
      styles.map(style => <StyleItem key={style.id} style={style} />)
    )
    .onFailure(() => <div>Something went wrong</div>)
    .orNull()
}
```

### Setting Atom Values

```tsx
import { useAtomSet } from '@effect-atom/atom-react'

function StyleItem({ style }: { style: Style }) {
  const deleteStyle = useAtomSet(deleteStyleAtom, { mode: 'promise' })
  const [deleteFn, deleteResult] = useAtomPromise(deleteStyleAtom)
  
  const handleDelete = () => deleteStyle(style.id)
  
  return (
    <div>
      <span>{style.name}</span>
      <button 
        onClick={handleDelete}
        disabled={deleteResult.waiting}
      >
        {deleteResult.waiting ? 'Deleting...' : 'Delete'}
      </button>
    </div>
  )
}
```

### Custom Hooks

Create reusable hooks for common patterns:

```tsx
// hooks/useAtomPromise.ts
export function useAtomPromise<A, E, Args extends readonly unknown[]>(
  atomFn: (...args: Args) => Atom.Atom<Result.Result<A, E>>
) {
  const [execute, result] = useAtomSet(atomFn, { mode: 'promise' })
  const value = useAtomValue(result)
  
  return [execute, value] as const
}

// hooks/useAtomPromiseUnwrapped.ts  
export function useAtomPromiseUnwrapped<A, E, Args extends readonly unknown[]>(
  atomFn: (...args: Args) => Atom.Atom<Result.Result<A, E>>
) {
  const [execute] = useAtomSet(atomFn, { mode: 'promise' })
  
  const wrappedExecute = useCallback(
    (...args: Args) =>
      execute(...args).then(exit =>
        Exit.match(exit, {
          onSuccess: identity,
          onFailure: cause => { throw Cause.squash(cause) }
        })
      ),
    [execute]
  )
  
  return wrappedExecute
}
```

## Advanced Patterns

### Runtime Per Feature

Isolate services by creating feature-specific runtimes:

```ts
// atoms/styles.ts
const runtime = makeAtomRuntime(
  Layer.mergeAll(
    APIClient.Default,
    Logger.pretty
  )
)

export const stylesAtom = runtime.atom(/* ... */)
export const upsertStyleAtom = runtime.fn(/* ... */)
export const deleteStyleAtom = runtime.fn(/* ... */)
```

### Keep-Alive Services

Prevent service destruction for global services:

```tsx
// components/KeepAliveServices.tsx
import { useAtomMount } from '@effect-atom/atom-react'

const keepAliveRuntime = makeAtomRuntime(WorkerClient.Default)

export function KeepAliveServices() {
  useAtomMount(keepAliveRuntime)
  return null
}

// In your root component
function App() {
  return (
    <RegistryProvider>
      <KeepAliveServices />
      {/* Rest of app */}
    </RegistryProvider>
  )
}
```

### Cache Management

```tsx
function StylesList() {
  const stylesResult = useAtomValue(stylesAtom)
  const refreshStyles = useAtomRefresh(stylesAtom.remote) // Refresh source
  
  return (
    <div>
      {/* Results display */}
      <button onClick={refreshStyles}>Refresh</button>
    </div>
  )
}
```

## Migration from @effect-rx/rx

When migrating from the old RX package:

1. **Package names**: `@effect-rx/rx` → `@effect-atom/atom`
2. **Imports**: `import { RX } from '@effect-rx/rx'` → `import { Atom } from '@effect-atom/atom'`
3. **Concepts**: "RX" → "Atom", "RX runtime" → "Atom runtime"
4. **API**: Most APIs remain the same, just replace `RX` with `Atom`

## Best Practices

### 1. Service Isolation
- Create feature-specific runtimes when services shouldn't be shared
- Use keep-alive services for truly global services

### 2. Error Handling  
- Model expected errors in your Effect types
- Use defects for unexpected errors (network failures, etc.)
- Provide user-friendly error messages through Result builders

### 3. Cache Management
- Use writable atoms for optimistic updates
- Separate remote atoms from local cache atoms
- Refresh remote atoms, not derived cache atoms

### 4. Performance
- Leverage memo maps to share service instances
- Use specific atom subscriptions instead of large state objects
- Consider atom composition for complex derived state

### 5. Testing
- Feature-specific runtimes make testing easier
- Provide test implementations of services in test runtime
- Atoms are pure functions that can be unit tested

## Common Patterns

### CRUD Operations

```ts
// Complete CRUD atom setup
export const itemsRemoteAtom = runtime.atom(Effect.gen(function*() {
  const api = yield* APIClient
  return yield* api.items.list()
}))

export const itemsAtom = Atom.writable(
  (get) => get(itemsRemoteAtom),
  (context, action: ItemAction.Type) => {
    // Handle CRUD operations
  }
)

export const createItemAtom = runtime.fn((data: CreateItemData) =>
  Effect.gen(function*() {
    const api = yield* APIClient
    const item = yield* api.items.create({ data }).pipe(
      withToast({ /* toast options */ })
    )
    
    const registry = yield* Atom.Registry
    registry.set(itemsAtom, ItemAction.Create({ item }))
    
    return item
  })
)
```

### Form Integration

```tsx
import { useForm } from '@tanstack/react-form'

function ItemForm() {
  const [createItem] = useAtomPromiseUnwrapped(createItemAtom)
  
  const form = useForm({
    defaultValues: { name: '', description: '' },
    onSubmit: async ({ value }) => {
      await createItem(value)
      form.reset()
    }
  })
  
  return (
    <form onSubmit={form.handleSubmit}>
      {/* Form fields */}
      <form.Subscribe
        selector={state => state.isSubmitting}
        children={isSubmitting => 
          <button disabled={isSubmitting}>
            {isSubmitting ? 'Creating...' : 'Create'}
          </button>
        }
      />
    </form>
  )
}
```

This guide covers the essential patterns for building React applications with Effect-TS and atoms. The key is understanding atoms as reactive computation nodes that integrate seamlessly with Effect's service system while providing optimized React integration.