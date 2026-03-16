# Honcurrency: A Mini Concurrent Execution Model in Haskell

Honcurrency is a small, interpretable model of concurrency built from the ground up in Haskell. It provides a clear and testable environment for understanding the core concepts of multi-threaded programming.

At its heart, Honcurrency defines a tiny instruction set, a simple scheduler, and a set of concurrency primitives. This is not a high-performance library, but a pedagogical tool designed to make the semantics of concurrency explicit and easy to explore.

## Vision

The goal of Honcurrency is to demystify concurrency. By providing a clear, simple, and inspectable model, it helps developers and students build a strong mental model of how threads, memory, and synchronization primitives interact.

## Features

Honcurrency implements a range of fundamental concurrency primitives, built upon a small set of core instructions:

*   **Spinlock**: A simple, busy-waiting lock.
*   **Mutex**: A lock that puts waiting threads to sleep instead of busy-waiting.
*   **Semaphore**: A classic counting semaphore for managing access to a pool of resources.
*   **Condition Variable**: A tool for threads to wait for a certain condition to become true.
*   **Channels**: A way for threads to communicate with each other.

## How it Works

Honcurrency is built in layers:

1.  **Core**: The `Core` modules define the fundamental components of the execution model:
    *   `Instr.hs`: A tiny instruction set with branching, memory operations, and concurrency primitives.
    *   `Machine.hs`: The state of the machine, including shared memory, registers, and thread states.
    *   `Scheduler.hs`: A simple, round-robin scheduler that simulates the execution of multiple threads.
    *   `Program.hs`: A DSL for writing programs in the instruction set.

2.  **Concurrency Primitives**: The `Conc` modules implement the concurrency primitives on top of the core instruction set.

3.  **Utilities**: The `Utils` modules provide helpful data structures, like a queue for managing waiting threads.

## Getting Started

### Prerequisites

*   [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)

### Building and Running

A PowerShell build script (`build.ps1`) is provided for convenience.

1.  **Build the project:**

    ```bash
    .\build.ps1 -Build
    ```

2.  **Run an example:**

    ```bash
    .\examples\semaphore.exe False
    ```

    The boolean argument to the example executables typically controls whether to show verbose tracing.

3.  **Run the tests:**

    ```bash
    .\build.ps1 -Test
    ```

## What This Demonstrates

Studying and experimenting with Honcurrency can help you:

*   **Deepen your understanding of concurrency**: See how locks, semaphores, and condition variables are implemented.
*   **Learn about systems modeling**: Understand how to model a simple computer architecture and scheduler.
*   **Improve your Haskell skills**: Explore the use of `StateT` and `Writer` for building DSLs.
*   **Develop testing discipline**: See how to write behavior-driven tests for concurrent code.

## Known Simplifications and Limitations

Honcurrency is a model, and as such, it makes some simplifications:

*   The `CAS` (Compare-And-Swap) instruction is a simplified test-and-set.
*   There is no memory consistency model; all operations are sequentially consistent.
*   The scheduler is deterministic and not preemptive (beyond time slices and explicit yields).
*   There are no fairness guarantees, so starvation is possible.
