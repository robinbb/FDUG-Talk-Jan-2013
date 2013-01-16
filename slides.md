% OO Design Patterns in Functional Programming Languages
% Robin Bate Boerop
% FDUG, 16 January 2013

# Agenda

* About this talk
* About the speaker
* Moore&apos;s Law
* Motivation: why parallelism?
* Problems with traditional concurrent/parallel programming
* Characteristics of functional programming
* Approaches to concurrency and parallelism in functional programming

# About this talk

> This highly technical talk will explain the problems encountered by imperative
programming languages (C/C++, Java), and will explain how functional
programming languages and frameworks (Erlang, Node, Haskell) have created
elegant solutions. The task of writing a high performance IRC server will serve
as an example.

# About this talk

Goal 1: To explain why understanding parallel and concurrent programming is
more important than ever.

Goal 2: To point you to &quot;ahead-of-the-curve&quot; software development
tools and techniques for including parallelism and concurrency in the
software that you make.


# About The Speaker

* Name: Robin Bate Boerop
* Class: Software Developer
* Alignment: Chaotic Good


# Moore&apos;s Law

* Is really about the number of components on ICs.
* Double every year.
* Performance doubles every 18 months, history re-written by Intel.
* How?
* This is the motivation for learning to program parallel and concurrent programs.


# Vocabulary 1

Concurrency: something to do with your problem.

Parallelism: seeking performance with repeated (or simply more) hardware.


# Vocabulary 2

Data parallelism:

* Operate simultaneously, often identically, on bulk data
* Single flow of control
* Synchronization is implicit
* Provides massive parallelism
* Easy to obtain correctness
* SIMD


# Vocabulary 3

Task parallelism:

* Explicit threads
* Synchronization by programmer via locks, messages, or STM
* Provides modest parallelism
* Very difficult to program correctly
* MIMD


# Vocabulary 4

Semi-implicit parallelism:

* Evaluation of pure functions in parallel
* Synchronization is implicit
* Provides modest parallelism
* Easy to program (FP langs.)


# Example of Semi-Implicit Parallelism

~~~~ {.haskell}
f list =
   sum (map blah list)

main = if x then y else z
       do a b
          c
~~~~

becomes

~~~~ {.haskell}
f list =
   sum (parMap blah list)
~~~~


# Parallelism in Traditional PLs 1

* Semi-implicit parallelism not available
* For data parallelism, use libraries
    * From Intel and other vendors
    * Orc
    * Use OpenGL
    * OpenCL (for GPUs)


# Parallelism in Traditional PLs 2

* Most parallelism comes from task (explicit) parallelism (concurrent programming running on multi-core)
* Many threads, most performing I/O
* Shared address space
* Eg. GUIs, web servers, BitTorrent clients
* Non-deterministic by design
* Need (lightweight) threads, way to coordinate
    * pthreads, Java locks


# Problems with Locks

* Race-condition: forgot to obtain correct lock
* Deadlock: obtained locks in wrong order
* ... or did not fix an order
* Lost wakeups: forgot to notify cond var
* Error recovery mindfuck: need to restore correct lock state in error condition
* Future-proofing: must ensure subsequent maintainers of your code know what to do
* Just plain hard to get right


# High Intellectual Overhead of Traditional Concurrent Programming

* Must document re-entrancy characteristics of API
* Must understand threading to use API, if only that you do not understand
* Thread pools
* Thread local storage
* Interaction between GC and multithreading
* Message pumps, event handlers, etc.


# Functional Parallel Programming

* Note that large-scale applications of parallel programming are declarative and value-oriented:
    * SQL
    * LINQ
    * Map/Reduce frameworks
* Which is better?
    * Plan A: start with a language whose computational model is sequential, and then add parallelism.
    * Plan B: start with a language whose computational model is parallel by default.


# Node

* Core insight: disallow multithreading!
* It is a single event loop.
* Not as stupid as it might seem.
* Makes for specialist language.
* Great for creating servers by JavaScript developers.


# Erlang

* Processes (really more like threads) share nothing
* Message passing by copy (serialization)
* Processes have independent GC, independent failure!
* Communicate over channels
* Highly reliable systems, phone switches


# Haskell - What Is It?

* Strongly typed purely functional programming language.
* Extremely popular among PL enthusiasts.
* GHC is production-grade.
* Is the target for PL researchers.
* Great for writing EDSLs.


# Haskell

Advantages of Functional Programming
for writing correct programs which include concurrency or parallelism.

* Side-effects are more tightly controlled than imperative programs.
* DSLs for GPUs or data parallel instructions
* Software transactional memory
* Semi-implicit parallelism


# Task Parallelism in Haskell

* This is the explicit threads, sychronization by programmer story
* Haskell has extremely lightweight threads
    * Millions of threads per program not a problem
    * Overhead per thread: 100 bytes approx.
* I/O via Unix epoll: okay to have millions of outstanding I/O requests
* Threads shared address space
* Synchronization via STM, or more explicitly via MVars


# Example of Concurrency Program in Haskell Using STM

~~~{.haskell}
module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

atomRead = atomically . readTVar
dispVar x = atomRead x >>= print
appV fn x = atomically $ readTVar x >>= writeTVar x . fn
milliSleep = threadDelay . (*) 1000

main = do shared <- atomically $ newTVar 0
          before <- atomRead shared
          putStrLn $ "Before: " ++ show before
          forkIO $ 25 `timesDo` (dispVar shared >> milliSleep 20)
          forkIO $ 10 `timesDo` (appV ((+) 2) shared >> milliSleep 50)
          forkIO $ 20 `timesDo` (appV pred shared >> milliSleep 25)
          milliSleep 800
          after <- atomRead shared
          putStrLn $ "After: " ++ show after
 where timesDo = replicateM_
~~~~

# Output

    Before: 0
    0
    1
    0
    1
    0
    1
    1
    0
    1
    0
    1
    1
    0
    1
    0
    1
    1
    0
    1
    0
    1
    1
    0
    1
    0
    After: 0

# Functional Programming for Parallelism and Concurrency: Summary

* No silver bullet. Concurrency and parallelism will soon be mandatory. World is complicated. Get used to it!
* Functional programming (Haskell in particular) already way better than traditional programming languages for concurrency (explicit parallelism).
* For large scale data parallel programming, we are already using declarative programming (map/reduce, SQL, etc.). More of that is coming.
* Haskell hosts multiple paradigms. Your traditional language does not.

