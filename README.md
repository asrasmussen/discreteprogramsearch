# Introduction

This repository contains variations of "Discrete Program Search" and the following is - hopefully - pedagogical introduction to the concept.

The idea is inspired by Victor Taelin's work on discrete program search using Higher Order Co's HVM to efficiently perform program search. See [for example](https://gist.github.com/VictorTaelin/7fe49a99ebca42e5721aa1a3bb32e278).

This repository uses Haskell since we will be searching infinitely large spaces, and Haskell's lazy evaluation model makes that much more easier to do than in other languages.

Below you will find the pedagogical walkthroughs of DPS and the code is available in the subfolders.

# How to run this code

Install Haskell and cabal. Run `cabal run` to run the project or `cabal repl` for a interactive repl.

# Searching Integer Arithmetic Programs

To introduce the concept of program search we will start by finding programs that can perform a certain arithmatic operation.

For example we might want a program that given the inputs 1, 2, 4, produces the outputs 2, 4, 8. These are the constraints we will use to guide the search and determine when to stop searching.

In general we will have some list of input-output pairs:

```
constraints :: [(Int, Int)]
constraints = [(1, 2), (2, 4), (4, 8)]
```

The most obvious function that satisfies these constraints might be `f(x) = x * 2`. 

To find a function that satisfies the constraints we need a way to search through all possible programs/functions and evaluate them on the constraints.

For a fully general program search you would probably use a low-level representation of programs like a lambda calculus variant, or Taelin's interaction nets.

To keep things relatively simple here, we will define a small "programming language" that can perform the operations we might be interested in.

In this section, our language will model functions that take a single input variable and can perform integer arithmetic operations, such as `+, -, *, /`.
The language also supports constant positive integer values.

We can represent such a function in the following way:

```haskell
-- The Op datatype enumerates the allowed arithmetic operations
data Op = Mult | Plus | Sub | Div deriving (Show)

-- Possible function bodies, i.e. f(x) = some term
data Term =
  Var                   -- This represents the input variable, 'x' above.
  | ConstNum Int        -- A constant with some integer value
  | Binary Op Term Term -- A binary operation on two terms. Note that we have a recursive type here, so operations can be arbitrarily nested.
  deriving (Show)
```

To further understand how this data type can be used to describe a function/program we'll define a evaluator function, which takes an input value, a `Term`, and runs the function represented by the `Term`:

```haskell
eval :: Int -> Term -> Int
eval x Var = x                                                     -- `Var` stands for whatever the input variable is
eval _ (ConstNum x) = x                                            -- Constants do not depend on the input
eval input (Binary Mult t1 t2) = (eval input t1) * (eval input t2) -- A binary operation first evaluates its 'legs' then applies the binary operation to the results.
eval input (Binary Div t1 t2) = (eval input t1) `div` (eval input t2)
eval input (Binary Add t1 t2) = (eval input t1) + (eval input t2)
eval input (Binary Sub t1 t2) = (eval input t1) - (eval input t2)
```


If the target function is `f(x) = x * 2` we can write that as the `Term` value

```haskell
targetTerm = Binary Mult Var (ConstNum 2)
```

Exercise: Write terms for `f(x) = x * x * 2` and `f(x) = 1 + x * 2`.



## Searching infinite spaces
Even though our language is so simple, it still has an infinite number of possible terms (we can have `x`, `x * x`, `x * x * x`, ..., not to mention what happens when we include constant values):is so simple, it still has an infinite number of possible terms (we can have `x`, `x * x`, `x * x * x`, ..., not to mention what happens when we include constant values).

Due to Haskell's lazy evaluation model, it is actually quite easy to work with infinite lists. For example, all possible constant values can be written as

```haskell
constants = map ConstNum [0..] -- Remember we only accept positive integers in our toy language
```

All possible variable terms is easy: there's only one:

```haskell
vars = [Var]
```

Things get slightly more tricky when we consider binary operations.

Let's start with multiplications:

```haskell
multiplications = liftM2 (Binary Mult) terms terms
```

We've had to define an as-of-yet undefined variable `terms` since `multiplications` can have all possible terms as its 'legs'.

Note that also use the function `liftM2` which can be thought of as applying its first argument (`(Binary Mult)`) to every combination of its second and third arguments (it's a function for use with monads but we basically only working with lists here).

We can define the rest of the terms similarly:

```haskell
constants = map ConstNum [0..]
vars = [Var]
mult = liftM2 (Binary Mult) terms terms
div = liftM2 (Binary Div terms terms
add = liftM2 (Binary Add) terms terms
sub = liftM2 (Binary Sub) terms terms

terms :: [Term]
terms = vars ++ constants ++ mult ++ div ++ add ++ sub
```

If you try to compile this program it will compile but it's actually completely useless.

The reason is that if we try to search through all the possible terms to find a function that satisfies our constraints, we will spend an infinite amount of time looking through terms like `ConstNum 0`, `ConstNum 1`, `ConstNum 2`, ... . 

We never reach any multiplications terms because they only occur after an infinite number of constant terms (and one `Var` term).

What we need is a better way to search through combinations of infinite lists. To do this we use the `Omega` monad.

How exactly the Omega monad works is beyond the scope of this document, but the code is actually simple and short enough and is available at (https://hackage.haskell.org/package/control-monad-omega-0.3.2/docs/src/Control.Monad.Omega.html).

The basic idea is simple, however. Suppose we want to search through two infinite lists: [0, 1, 2, ...] and [-1, -2, -3, ...]. As above, it doesn't work to search through the first list and only then start on the second since it will take an infinite amount of time to reach -1.

Instead we use a zig-zag pattern of searching: 0 -> -1 -> 1 -> -2 -> 2 -> -3 -> ... . This is what the Omega monad implements. This example could be written as

```haskell
each [0..] <|> each [-1, -2 ..]
```

The `each` function turns a possibly infinite list into an "Omega list", and the `<|>` operator takes care of correctly combining the two infinite lists.

This combination can be applied arbitrarily many times and you are sure to reach every element of each infinite list in finite time (although it may take a long time).

Our enumeration above then becomes

```haskell
constants = map ConstNum [0..]
vars = [Var]
mult = liftM2 (Binary Mult) terms terms
div = liftM2 (Binary Div terms terms
add = liftM2 (Binary Add) terms terms
sub = liftM2 (Binary Sub) terms terms

terms :: Omega Term
terms = 
    return Var                           -- Equivalent to `each [Var]`
    <|> each constants
    <|> liftM2 (Binary Mult) terms terms -- Since terms now is of type `Omega Term`, `liftM2` will produce `Omega Term` as well.
    <|> liftM2 (Binary Div terms terms
    <|> liftM2 (Binary Add) terms terms
    <|> liftM2 (Binary Sub) terms terms
```

To see how the Omega monad enumerates things you can run the following in the repl:
```haskell
take 10 $ runOmega terms -- Technically you might need to do `take 10 $ runOmega IntFun.terms` because of the way the project is set up.
```

## Finding our target function
We are almost ready to search through the infinite possible arithmetic programs.

The last thing we need is a function that can check whether a `Term` value satisfies the constraints.

Here is one implementation:
```haskell
checkConstraints :: Term -> Bool
checkConstraints term = all id $ map (\t -> eval (fst t) term == (snd t)) constraints
```

We can then find the first program that satisfies our constraints:

```haskell
firstOk :: Term
firstOk = head $ filter (\pair -> checkConstraints (snd pair)) $ zip [1..] (runOmega terms) 
-- We zip with the list [1..] to see how many terms had to be searched before we found the correct one.
```

The result of this is 

```bash
(4,Binary Plus Var Var)
```

So we found the function `f(x) = x + x` which is equivalent to `f(x) = x * 2`. 

Exercise: Try setting up other constraints and running the program search.


# Searching Functions on Bit Strings
This section is heavily inspired/adapted from Taelin's public examples of DPS, see [this](https://gist.github.com/VictorTaelin/7fe49a99ebca42e5721aa1a3bb32e278).


In this section we want to model a function/program that takes a bit string as input and produces a new bit string.

Examples of interesting functions in this space are:

- Binary increment
- Bitwise not
- Bitwise XOR


Our data type will be the following:


```haskell
data BitString =
  Empty
  | Zero BitString
  | One BitString
  deriving (Show)
```

You will note that this is basically a linked list.

A lot of operations on bit strings start with the least significant bit, so to facilitate this we will interpret the head of the linked list as the least significant bit, and the last element as the most significant bit. That is:

```haskell
6 (base 10) = 110 (base 2) = Zero (One (One Empty)) (BitString)
```

In other words, we read the `BitString` values in the opposite of the usual direction.


As before, we want to define the operations that can be performed on `BitString` values. 

We are going to think of our functions as taking a bitstring and gradually producing the output bitstring.

To facility that, we need operations to "emit" elements to the output string:

```haskell
data Term =
    EmitEmpty       -- Emit the empty element. There is nothing more to do here, because 'Empty' marks the end of the output BitString.
    | EmitZero Term -- After emitting Zero or One, we still have stuff left to do (perhaps just emitting Empty), so we add an extra operation to perform after the emission.
    | EmitOne Term
```

A simple program that constantly outputs 6 would correspond to

```haskell
-- Remember we output the least significant bit first
alwaysSix = EmitZero (EmitOne (EmitOne EmitEmpty))
```

So far the operations we have defined can only specify constant programs. To do interesting things we need conditionals and looping. We will also add an early return that we will see can be useful.

```haskell
data Term =
 EmitEmpty
 | EmitZero Term
 | EmitOne Term
 | If Term Term -- Looks at the head of the bitstring and chooses a branch depending on whether the head is Zero or One
 | Recurse      -- Restart the original program at the current position in the bitstring
 | Return       -- Return the remainder of the bitstring
 deriving (Show)
```

As before we now need an evaluator function to run our programs, we need to enumerate - using the Omega monad - all possible programs, and then finally to specify constraints and do the search.

The evaluator plus a couple of utility functions will look like this:

```haskell
-- Take input value, convert to binary string, evaluate function, convert result to integer
eval :: Integer -> Term -> Maybe Integer
eval input body = binToInt . (eval_ 10 body body) . intToBin $ input

-- I chose to make this output a `Maybe` because a program that just outputs an `Empty` is not valid.
binToInt :: BitString -> Maybe Integer
binToInt = binToInt_ 0 0 False
  where
    binToInt_ :: Integer -> Integer -> Bool -> BitString -> Maybe Integer
    binToInt_ _ _ False Empty = Nothing
    binToInt_ level acc True Empty = Just acc
    binToInt_ level acc b (Zero term) = binToInt_ (level + 1) (acc) True term
    binToInt_ level acc b (One term) = binToInt_ (level + 1) (acc + 2^level) True term

intToBin :: Integer -> BitString 
intToBin 0 = Zero Empty
intToBin x = intToBin_ nbits x Empty
  where
    nbits = ( (floor $ (log $ fromIntegral x :: Double) / (log 2.0)) :: Int)
    intToBin_ :: Int -> Integer -> BitString -> BitString
    intToBin_ (-1) x acc = acc
    intToBin_ bitindex x acc 
      | (x `shiftR` bitindex) `mod` 2 == 1 = intToBin_ (bitindex - 1) x (One acc)
      | otherwise = intToBin_ (bitindex - 1) x (Zero acc)
```


The inner evaluator, `eval_`, is made of the following parts

```haskell
-- Args:
-- Recursion depth
-- Original program (to enable recursion)
-- Current instruction
-- Current input
eval_ :: Integer -> Term -> Term -> BitString -> BitString
```

Because we have the `Recurse` instruction it's very easy to define a program that loops forever: it is just `Recurse`. 

To avoid infinite looping, we will specify a maximal recursion depth. If the program exceeds this recursion depth, it fails.

Recursion also requires a reference to the original program, or some mechanism to start the execution over from the start. On (current) real architectures, this is done by moving the instruction pointer to the start of the program code block. 
Here I chose to simply provide the evaluator with the original program, since we don't have a notion of instruction pointer in Haskell.


Next for the implementation

```haskell
-- After `Empty` has been emitted the program stops
eval_ recDepth orig (EmitEmpty) _ = Empty
```

```haskell
-- Emit `Zero` or `One` and then run the rest of the program
eval_ recDepth orig (EmitZero next) x = Zero $ eval_ recDepth orig next x
eval_ recDepth orig (EmitOne next)  x = One  $ eval_ recDepth orig next x
```

```haskell
-- Output the input
eval_ recDepth orig Return x = x
```

So far the instructions have not modified the input argument. The `If` instruction is different; it consumes the head of the input:

```haskell
eval_ recDepth orig (If true false) (Zero rest) = eval_ recDepth orig false rest
eval_ recDepth orig (If true false) (One rest)  = eval_ recDepth orig true rest
eval_ recDepth orig (If true false) Empty       = eval_ recDepth orig false Empty
```

It's worth noting here that I chose to make `Empty` act like a false value. I think it makes sense: any bit string can be thought of as preceded by an infinite amount of zeros:

```haskell
6 (base 10) = ...00000110 (base 2) = Zero (One (One (Zero (Zero ... ))))
```
(Remember we go from least to most significant bit in the `BitString` type.) With this in mind `Empty` can be thought of as standing in for an infinite amount of `Zero`s.


The final piece of the puzzle is recursion:

```haskell
eval_ 0 orig Recurse x = Empty                                   -- If we reach the maximum recursion the program exits
eval_ recDepth orig Recurse x = eval_ (recDepth - 1) orig orig x -- Restart from the original program, decrement recusion depth
```

We can then define the search filter and the constraints:

```haskell
checkAllConstraints term = all id $ map (evalConstraint term) constraints
  where
    evalConstraint term (input, output) = case eval input term of 
                                        Just x -> x == output
                                        Nothing -> False

```

Let's try to find the binary increment function
```haskell
constraints = [(0, 1), (1, 2), (2, 3), (3, 4)]

firstInc = head $ filter (\pair -> checkAllConstraints (snd pair)) $ zip [1..] (runOmega terms)
```


This outputs

```bash
(374,If (EmitZero Recurse) (EmitOne Return))
```

Exercise: Convince yourself this program is correct.


# What's next
For more and more general programs we want a more abstract and powerful representation.

Possible choices are lambda calculus or interaction nets, for example.

However, as you attempt to find more complicated programs you can easily run into problems due to the search space being infinitely large and not optimally structured.

The last point is worth thinking about: our program searches only succeeded because the correct programs occurred early enough in our enumeration of all possible programs. This was largely because the target programs were fairly short and because we enumerated programs from shorter to longer (more or less).

A language can be made more powerful in two ways: 1) make it more abstract and "low-level", i.e. move in the direction of, say, lambda calculus, or 2) increase the number of basic instructions that are available to the language.

In either case, I think, the target programs tend to occur later in the search, either because it requires more terms, or because there are more basic terms to search through before you reach the right combination.

And you don't have to make the test case much more complicated than the above examples before you run into trouble.

Case in point: I also attempted to define a language for performing list operations, with the goal of finding a sorting algorithm automatically.

I know the language was powerful enough to express sorting, because I manually wrote a sorting program.

The trouble was that even after searching 500 million programs, a correct sorting algorithm had not been found.


This is where the kind of technology that Taelin and Higher Order Co are working on becomes necessary.

I don't fully understand how it's implemented but they use a special runtime that can efficiently collapse multiple different calculation pathways in a way that eliminates redundant calculations. Read more [here](https://gist.github.com/VictorTaelin/d5c318348aaee7033eb3d18b0b0ace34).



















