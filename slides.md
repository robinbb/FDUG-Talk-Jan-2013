% OO Design Patterns in Functional Programming Languages
% Robin Bate Boerop
% FDUG, 16 January 2013


# Agenda

* About the speaker
* About this talk
* About the audience
* About FP
* Haskell basics
* Functional programming idioms and OO design patterns
* Conclusion

# About The Speaker

> Robin Bate Boerop [http://robinbb.com] is the Chief Technology Officer at UserEvents
> [http://userevents.com], an enterprise software technology startup company
> based in Fredericton, NB. He has been a professional software developer for
> fifteen years. Robin has worked in the most challenging of software
> domains: concurrency control technologies, parallel programming,
> just-in-time compilation techniques, compiler construction, operating
> systems, memory management algorithms, performance programming, and large
> scale software development methods. His software forms integral parts of
> IBM's DB2 for Linux and Windows, Mozilla's Firefox, and other widely used
> open source software projects.
> Robin's preferred programming language is Haskell, which he has been using
for 10 years. He uses functional programming techniques in his every day
work. At his workplace, UserEvents, Clojure is the programming language of
choice. He has years of experience using C/C++ in large scale projects, and
so is well suited to compare the experience of functional programming with
the use of more conventional programming languages.

# About This Talk

GoF book.

> Effective software development in conventional programming languages (like C++, Java, or C#) requires knowledge of programming idioms. These idioms, the most popular of which were well documented in the Gang of Four (GoF) book "Design Patterns", are well known to professional programmers. How do conventional programming language expressions of these idioms map to modern functional programming languages like Clojure and Haskell? This talk answers the question with concrete examples, and aims to show that each of the GoF design patterns are better expressed, or not even necessary, in the modern functional programming languages. No prior knowledge of Clojure or Haskell is necessary.

A bigger goal: learn about FP ideas.


# About You, The Audience

* Professional software dev using conventional languages (C++, C#, Java)
* Already understand design patterns
* No knowledge of functional programming (FP)
* Interested in learning about advantages of FP


# About FP

* Algebraic data types
* Functions as first class values
* Immutability
* Laziness
* Lisp data-as-code features

# Haskell Basics

~~~~ {.haskell}
pi = 3.1415927

theta = pi / 2

f x = 1 + pi * x + x^2

main =
   do c <- getChar
      putStrLn ("You typed " ++ show c)
~~~~

# Haskell Basics

~~~~ {.haskell}
pi :: Double
pi = 3.1415927

f :: Double -> Double
f x = 1 + pi * x + x^2
~~~~

# Haskell Basics

~~~~ {.haskell}
getChar :: IO Char
putStrLn :: String -> IO ()
main :: IO ()
main =
   do c <- getChar
      putStrLn ("You typed " ++ show c)
~~~~

# Haskell Basics

~~~~ {.haskell}
map :: (a -> b) -> List a -> List b
~~~~

# Haskell Basics

~~~~ {.haskell}
class Functor f where
   fmap :: (a -> b) -> f a -> f b

class Monad m where
   return :: a -> m a
   (>>=)  :: m a -> (a -> m b) -> m b
~~~~


# Type Classes

* Bridge design pattern
* Type classes are very much like interfaces or abstract classes
* Haskell type classes can't have state, can have default impls

~~~~ {.haskell}
class Abstraction a where
   hash :: a -> Integer
   ser  :: a -> ByteString
   wrap :: a -> Wrapper a
   hash _ = 0
~~~~


# Algebraic Data Types

* First-class citizen in languages: OCaml, Standard ML, Haskell, Scala, F#
* Used to express DPs: Command, Composite, Interpreter
* The only way that data values are expressed in Haskell
* May be recursive
* Related to pattern matching (nothing to do with regexes)

# Algebraic Data Type Example

~~~~ {.haskell}
data Cmd =
   LogMsg String | StopLogger

data Expr =
   Prod Expr Expr | Sum Expr Expr | Value Int
~~~~

# ADT Examples Cont'd

~~~~ {.haskell}
data Cmd =
   LogMsg String | StopLogger

log :: Cmd -> IO ()
log (LogMsg m) = hPutStrLn stderr m
log StopLogger = hClose stderr

logCmds :: [Cmd] -> IO ()
logCmds = mapM_ log
~~~~

# ADT Examples Cont'd

~~~~ {.haskell}
data Expr =
   Prod Expr Expr | Sum Expr Expr | Value Int
...
eval :: Expr -> Int
eval (Prod e1 e2) = eval e1 * eval e2
eval (Sum e1 e2)  = eval e1 + eval e2
eval (Value i)    = i
~~~~


# Automatic Serialization

~~~~ {.haskell}
data Expr =
   Prod Expr Expr | Sum Expr Expr | Value Int
   deriving (Read, Show)
...
show :: Show a => a -> String
read :: Read a => String -> a
~~~~

* Memento DP (you always know what type you have)
* Lisp languages have a better story, here.


# Functions As First Class Values

* Much can be done by passing functions
* Strategy DP can be replaced by functions representing the various pieces
* Similarly for Template Method; just use HOFs
* Example on board (GoF p. 327)

# Function Composition

* Functions easily composed
* DPs: Adapter, Decorator, Chain of Responsibility
* These DPs don't "disappear" - you still have to adapt/decorate/chain in FPs

~~~~ {.haskell}
want :: x -> y
have :: z -> y
adapt :: z -> x
want = have . adapt
~~~~

# Immutability

* What is the deal with *that*?
* Prototype DP (shallow vs deep copy)
* Flyweight DP

# The Iterator and Visitor Design Patterns

* Implemented in code directly, not as a DP
* Methods of iteration, traversal are various, with flavours depending on IO,
  etc.

~~~~ {.haskell}
toList :: Foldable t => t a -> [a]
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
~~~~

# Laziness

* What is the deal with *this*?
* Nutshell: values are not computed at run-time unless needed
* Motivation for Proxy disappears
* Can be used in implementing Flyweight (maximum memo data structure)

# Design Patterns With No Applicability

* State

# DPs Not Discussed

* Facade
* Observer
* Singleton
* Mediator
* Factory Method
* Abstract Factory
* Builder


# Conclusion

Programming well still requires artistic/creative skills. Recognizing when to
use a well-known idiom is key in both OO and FP. FP makes some idioms more
readily expressible.

