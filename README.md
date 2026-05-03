# Honcurrency

![Static Badge](https://img.shields.io/badge/language-Haskell-blue?logo=Haskell&link=www.haskell.org)
![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/kINo204/honcurrency/build-release.yml)

A tiny concurrency machine, written in Haskell.

Honcurrency is an assembly-like DSL and interpreter for building concurrent programs from small, inspectable execution rules.

It is not a production concurrency library. It is a compact model for seeing how concurrency primitives can be constructed rather than assumed.

## The DSL

Honcurrency exposes an assembly-style DSL for writing programs that run on its own tiny concurrent machine.

The DSL is used in two places: first to write user programs, then to implement the concurrency primitives those programs call. The best way to read it is as a small language that grows from instructions into reusable routines.

### Start with machine instructions

At the bottom, a DSL block is just a sequence of instruction helpers. They append instructions to a `Program`, which is internally a `Writer`-based instruction builder. You usually do not need to think about that implementation detail; the important part is that each helper emits one machine-level operation.

| Operation      | Meaning                                             |
| -------------- | --------------------------------------------------- |
| `lab name`     | Define a label.                                     |
| `br target`    | Branch to a target label or program counter.        |
| `imm rd value` | Load an immediate value into register `rd`.         |
| `adi rd value` | Add an immediate value to register `rd`.            |
| `add rd rs`    | Add register `rs` into register `rd`.               |
| `lod rd addr`  | Load from memory address `addr` into register `rd`. |
| `sto rs addr`  | Store register `rs` into memory address `addr`.     |
| `blk`          | Block the current thread.                           |
| `pst tid`      | Wake a blocked thread.                              |
| `prt rs`       | Print the value of register `rs`.                   |

For example, this instruction block describes an infinite counter:

```haskell
lab "begin"
adi 0 1
prt 0
br $ Msg "begin"
```

It labels the beginning of the loop, increments register `0`, prints it, and jumps back.

The core `Program` type, `program`, `procedure`, and instruction helpers are defined in [`Core/Program.hs`](https://github.com/kINo204/honcurrency/blob/002f8c1c7992d67b4105a496139fd3f37f7e927c/Core/Program.hs#L36).

### Compile a block into a thread

An instruction block becomes an executable Honcurrency thread when it is wrapped with `program`:

```haskell
import Core.Program

counter = program $ do
  lab "begin"
  adi 0 1
  prt 0
  br $ Msg "begin"
```

`program` is the outer compiler for a complete thread. In normal code, it appears at the root of a user program, not around every helper function.

### Replace local effects with communication

The counter above only prints locally. To make it interact with other threads, keep the same loop shape but replace `prt` with a channel operation.

The channel example imports the concurrency library and defines a producer program like this:

```haskell
import Core.Program
import Conc.Channel

ch1 = channel 0 100 2

producer = program $ do
  lab "begin"
  adi 0 1
  send 0 ch1 1 2
  br $ Msg "begin"
```

This is the producer from [`examples/channel.hs`](https://github.com/kINo204/honcurrency/blob/002f8c1c7992d67b4105a496139fd3f37f7e927c/examples/channel.hs#L9-L13). It still looks like the counter: label, increment, loop. The difference is that the value in register `0` is sent through `ch1` instead of being printed.

A receiver can then consume values from a channel and print them:

```haskell
printer = program $ do
  lab "begin"
  recv 0 ch2 1 2
  prt 0
  br $ Msg "begin"
```

This printer is also from [`examples/channel.hs`](https://github.com/kINo204/honcurrency/blob/002f8c1c7992d67b4105a496139fd3f37f7e927c/examples/channel.hs#L25-L29).

At the call site, `send` and `recv` look like ordinary DSL operations. But they are not primitive instructions. They are library functions implemented in the DSL itself.

### Define DSL functions with `procedure`

Reusable DSL routines are packaged with `procedure`.

This is the main distinction:

* use `program` to compile a complete executable thread;
* use `procedure` to define a reusable instruction fragment that can be expanded inside a thread.

Channel receive is a good example. User code calls `recv` as if it were a single operation, but its implementation is a procedure composed from lower-level synchronization and machine instructions.

```haskell
recv rd chan t0 t1 = procedure $ do
  mutexLock (lock chan) t0 t1
  qlength (senders chan) t0 t1 -- no senders waiting, slow
  bfs t0 $ Msg "channel_recv_slow"

  dequeue (senders chan) t0 t1 -- fetch the first sender
  pst t0
  condWait (ready chan) (lock chan) t0 t1
  lod rd (buf chan)
  br $ Msg "channel_recv_end"

  lab "channel_recv_slow"
  tid t0
  enqueue (recvers chan) t0 t1
  mutexUnlock (lock chan) t0 t1
  blk
  mutexLock (lock chan) t0 t1
  lod rd (buf chan)

  lab "channel_recv_end"
  mutexUnlock (lock chan) t0 t1
```

The full definition is in [`Conc/Channel.hs`](https://github.com/kINo204/honcurrency/blob/002f8c1c7992d67b4105a496139fd3f37f7e927c/Conc/Channel.hs#L65-L86).

This is the core idea of Honcurrency's DSL: the same language writes both the user program and the primitive it calls. A user program can say `recv`; the library can define `recv` by locking, inspecting queues, blocking, posting, waiting on a condition variable, and loading from shared memory.

Concurrency primitives are not assumed by the host runtime. They are assembled inside Honcurrency.

## Run

Honcurrency currently uses a PowerShell build script.

```powershell
.\build.ps1 -Build
```

Run an example:

```powershell
.\examples\channel.exe False
```

Run the tests:

```powershell
.\build.ps1 -Test
```

The boolean argument controls whether the example prints verbose execution traces.

## Examples

The `examples` directory contains small executable experiments. Each one focuses on a particular concurrency primitive or interaction pattern.

Typical examples include:

* lock-based mutual exclusion
* semaphore coordination
* condition-variable waiting and signaling
* channel communication between concurrent programs

They are meant to be read as small machines, not as application code.

## The machine

`Core` is the machine layer.

It defines the low-level execution model used by every example and every higher-level primitive in the project:

* `Instr.hs` defines the instruction set.
* `Machine.hs` defines machine state: memory, registers, threads, and execution state.
* `Scheduler.hs` defines how runnable threads are selected and stepped.
* `Program.hs` provides the embedded DSL for writing instruction-level programs.

At this level, a program is a sequence of small operations: arithmetic, memory access, branching, printing, yielding, blocking, posting, and synchronization-related instructions.

The machine is intentionally small. That is what makes the behavior inspectable.

## The library above the machine

`Conc` is the concurrency layer.

It rebuilds familiar synchronization mechanisms on top of the core instruction set:

* spinlocks
* mutexes
* semaphores
* condition variables
* channels

These are not wrappers around Haskell's concurrency library. They are Honcurrency-level programs and protocols implemented against the project's own machine model.

The important idea is the layering: the primitives are not assumed; they are derived.

## Deliberate simplifications

Honcurrency chooses clarity over realism.

Some parts of the model are deliberately simplified:

* The scheduler is simple and deterministic.
* Memory is sequentially consistent.
* `CAS` is modeled as a simplified test-and-set operation.
* There is no claim of fairness.
* Starvation is possible.
* The project does not aim for performance.

These choices are part of the design. A more realistic runtime would also be harder to inspect. Honcurrency keeps the model small enough that the mechanisms remain visible.
