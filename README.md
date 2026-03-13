# Honcurrency - Mini Concurrent Execution Model in Haskell

This project builds a small, interpretable model of concurrency. It defines:
1) A tiny instruction set with branching, memory ops, and concurrency primitives.
2) A scheduler that simulates multi-threaded execution with yield/block/post.
3) Synchronization primitives (spinlock, mutex, semaphore, condvar) built on top.
4) A small DSL to assemble programs and a set of tests/examples to validate behavior.

The goal is not performance, but clarity: make concurrency semantics explicit and testable.

## Quick Map
- `Core/Instr.hs`: instruction set + execution semantics
- `Core/Machine.hs`: machine state (mem, regs, blocked/posted flags)
- `Core/Scheduler.hs`: multi-threaded scheduling and tracing
- `Core/Program.hs`: DSL for building instruction streams
- `Conc/*.hs`: sync primitives built from core instructions
- `Utils/Queue.hs`: queue implementation for wait lists
- `tests/*.hs`: behavior-focused tests
- `examples/*.hs`: runnable demos

## Core Semantics (What Is Modeled)
- **Thread state**: each thread has a frame (`pc`, registers).
- **Memory**: shared array, addressable by instructions.
- **Scheduling**: round-robin style stepping with a fixed timestep and `YLD`.
- **Blocking**: `BLK` blocks a thread; `PST` posts to a thread (unblocks or marks pending).
- **Atomicity**: `CAS` is modeled as a read + conditional write used for lock acquisition.

## Instruction Highlights
- `CAS`: reads memory and writes `1` if it was `0` (test-and-set style).
- `YLD`: yields control early in the time slice.
- `BLK` / `PST`: explicit block and post primitives for synchronization.

These are not hardware-accurate; they are pedagogical and sufficient to model core
concurrency behavior at the algorithm level.

## Concurrency Primitives Implemented
- Spinlock: `Conc/Spinlock.hs`
- Mutex: `Conc/Mutex.hs` (spinlock + wait queue)
- Semaphore: `Conc/Semaphore.hs` (counting + wait queue)
- Condition Variable: `Conc/Cond.hs` (wait queue + mutex discipline)

## Examples
- `examples/spinlock.hs`: basic spinlock usage
- `examples/mutex.hs`: mutex lock/unlock with two threads
- `examples/semaphore.hs`: resource pool modeled with a semaphore

## Tests
Behavioral tests live in `tests/`:
- `tests/blkpst.hs`: block/post semantics
- `tests/labeling.hs`: label scoping and branching
- `tests/yield.hs`: yield and interleaving
- `tests/semaphore.hs`: semaphore correctness cases
- `tests/cond.hs`: condition variable semantics

## How to Run
This is plain Haskell. If you use GHC:

```bash
.\build.ps1 -Build
.\examples\semaphore.exe False
```

Run tests similarly, e.g.:

```bash
.\build.ps1 -Test
```

You can also use the built executables of all examples from the release.

## What This Demonstrates (For Interviews)
- **Concurrency understanding**: lock/semaphore/condvar semantics and composition.
- **Systems modeling**: explicit instruction-level semantics and a scheduler.
- **Programming language skills**: DSL design with `StateT + Writer`.
- **Testing discipline**: behavior-driven tests around race-sensitive code.
- **Engineering structure**: clean module boundaries and examples.

## Known Simplifications / Limitations
- `CAS` is simplified (test-and-set semantics).
- No memory consistency model; all operations are effectively sequentially consistent.
- Scheduler is deterministic and not preemptive beyond time slice and yield.
- No fairness guarantees; starvation is possible in some cases.

## Possible Next Steps
- Expand CAS to full compare-and-swap semantics.
- Add fairness metrics or randomized scheduling.
- Add properties/invariants as tests (QuickCheck style).
- Extend the instruction set with fences or atomic increments.
