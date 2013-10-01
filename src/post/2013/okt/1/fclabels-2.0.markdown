
<article>

<div class=meta>
<span class=author>[Sebastiaan Visser](/about.html)</span>
<span class=date>Oktober 1st, 2013</span>
</div>

# fclabels 2.0

I'm excited to announce a brand new major version of
[`fclabels`](hackage.haskell.org/package/fclabels). This Haskell library
provides first class labels that can act as bidirectional record fields. The
labels can be derived automatically using Template Haskell which means you
don't have to write any boilerplate yourself. The labels are implemented as
_lenses_ and are fully composable. Lenses can be used to _get_, _set_ and
_modify_ parts of a data type in a consistent way.

<div style="width: 360px; margin: 0 auto">

  In this post:

  - [What's new](#whats-new)
  - [Basic usage](#basic-usage)
  - [Different lens types](#different-lens-types)
  - [Base lens](#base-lens)
  - [Monomorphic and polymorphic lenses](#polymorphic-and-monomorphic-lens-types)
  - [Specializing contexts](#specializing-contexts)
  - [Isomorphisms](#isomorphisms)
  - [Views using Applicative](#views-using-applicative)
  - [Deriving lenses in scope](#deriving-lenses-in-scope)
  - [Lenses as expressions](#lenses-as-expressions)
  - [Direct derivation](#direct-derivation)
  - [Lenses for GADTs](#lenses-for-gadts)

</div>

## What's new

This new version provides a major cleanup of the API and provides a lot more
power while not compromising on the simplicity of the interface. We now support
lenses for both monomorphic and polymorphic updates, we have total lenses,
partial lenses and lenses that can fail with some error.

There are now several new ways to derive the labels from existing Haskell
types. We can derive labels in scope (the existing method), but we can now also
generate them as expressions that can be named and typed manually, and we can
now also transform a record into a variant with first class labels directly.
Examples of this are shown below.

The new version now has full support for deriving lenses for the fields in a
GATD. It contains a sufficiently intelligent totality checker that looks at the
indices of a GADT to decide whether to derive the labels as total or partial
lenses.

A small set of predefined lenses is included for some of the types from base,
like Maybe, Either, lists and tuples.

For an introduction into this package read the rest of this blog post or see the
[API documentation on Hackage](http://hackage.haskell.org/package/fclabels) or
[the source code on github](https://github.com/sebastiaanvisser/fclabels).

__Note that the library is heavily optimised for qualified imports and some
names may occurs multiple times across the package. The full module name should
provide enough context.__

## Basic usage

Let's say we have a simple data type that represents an address. We can
automatically derive labels for this type by prefixing the constructors with an
underscore and invoking the Template Haskell function `mkLabel`:

```Haskell
import Data.Label

data Address = Address
  { _city    :: String
  , _street  :: (String, Int)
  } deriving Show

mkLabel ''Address
```

Now we have two labels in scope named after the record fields:

```Haskell
city    :: Address :-> String
street  :: Address :-> (String, Int)
```

The labels are implemented as lenses and are a _getter_ and _modifier_ function
packed in one data type. We can use the `get`, `modify`, and `set` function to
read or write values.

```
get    :: (f :-> a) -> f -> a
set    :: (f :-> a) -> a -> f -> f
modify :: (f :-> a) -> (a -> a) -> f -> f
```

Although not enforced by the type system, for predictable usage of the library
we expect the following laws to hold:

```
get l (modify l m f) == m (get l f)
```

Because the `Address` data type contains only one constructor and doesn't have
any type variables, the derived labels are __total__ and __monomorphic__. More
lens types are available as will be explained in the next section.

The labels can be used to get and update values from an `Address`.

```Haskell
rijksmuseum = Address
  "Amsterdam"
  ("Museumstraat", 1)

> get city rijksmuseum
"Amsterdam"

> modify city (map toUpper)
Address "AMSTERDAM" ("Museumstraat", 1)
```

Labels can be composed with other labels to dig deeper into a data type. We can
for example access the individual components of the street tuple:

```Haskell
import Control.Category
import Prelude hiding ((.))
import qualified Data.Label.Base as L

> modify (L.snd . street) (+ 99)
Address "Amsterdam" ("Museumstraat", 100)
```
Composition in `fclabels` is done using the `(.)` function from
`Control.Category`, a generalization of function composition.

This example explains the main goal of the library, derivation of labels for
our data types that are implemented as fully composable lenses. The lenses are
first class and can be passed around and extended as we wish. The lenses allow
both reading value from and writing value to a data type.

## Different lens types

The package provides several different lens types. The main distinction is
between lenses that only allow monomorphic updates and lenses that allow
polymorphic updates, and between total lenses and partial lenses. And the
combinations of both.

As described the lens in the `Address` example above as a total monomorphic
lens. But, for example, a lens that points to the value in the `Left`
constructor of an `Either` is a different type. It is partial because the `get`
or `modify` might fail in the case of a `Right` constructor. Also it is
polymorphic, because it is allow to change the type of the value.

## Base lens

All the lens types are build by specialization of a _base lens_ type. The base
lens type is an abstract polymorphic lens. Abstract means that the getters and
setters are not just Haskell functions, but can run in a custom `Category`
allowing effects.

```Haskell
Lens cat (f -> g) (o -> i)
```

The code for this lens can be found in [`Data.Label.Poly`](https://github.com/sebastiaanvisser/fclabels/blob/master/src/Data/Label/Poly.hs).

Because all the lenses in the package are specializations of this base they can
all be fully composed with each other.

## Polymorphic and monomorphic lens types

The two function types in the signature above indicate that by modifying the
referenced record field from `o -> i` the data type changes from `f -> g`. This
makes the lens allow polymorphic updates.

Monomorphic lenses simply specialise polymorphic lenses by having the same
input and output types:

```Haskell
Lens cat (f -> f) (o -> o)
```

Using a type synonym in
[`Data.Label.Mono`](https://github.com/sebastiaanvisser/fclabels/blob/master/src/Data/Label/Mono.hs)
specializes the lens type go get rid of the redundant type variables.

```
Lens cat f o
```

## Specializing contexts

By specializing `cat` to normal function space `(->)` we get a total lens. By
specializing to `Kleisli Maybe` we end up with a partial lens that fail
silently. By specializing to `Kleisli (Either e)` we end up with a lens that
can fail with some error value. We provide convenient names for those three
lens contexts.

```Haskell
type Total     = (->)
type Partial   = Kleisli Maybe
type Failing e = Kleisli (Either e)

Lens Total       (f -> g) (o -> i)
Lens Partial     (f -> g) (o -> i)
Lens (Failing e) (f -> g) (o -> i)
```

The code for the specialized lenses can be found in
[`Data.Label.Total`](https://github.com/sebastiaanvisser/fclabels/blob/master/src/Data/Label/Total.hs),
[`Data.Label.Partial`](https://github.com/sebastiaanvisser/fclabels/blob/master/src/Data/Label/Partial.hs),
[`Data.Label.Failing`](https://github.com/sebastiaanvisser/fclabels/blob/master/src/Data/Label/Failing.hs).

We can imagine this approach nicely scaling up to different contexts. For
example we could use `Kleisli IO` to build lenses for `IORef`s, or `Kleisli
STM` to build lenses for `TVar`s.

## Isomorphisms

The library contains a type `Iso` which represents an isomorphism, a
bidirectional function. Like lenses isomorphisms don't use Haskell functions
directly, but work in some custom `Category`.

```Haskell
data Iso cat i o = Iso (cat i o) (cat o i)
```

Isomorphisms can be lifted into a lens using the `iso` function and can then be
composed with other lenses. For example, we can create a partial lens from a
`Read`/`Show` isomorphism.

```Haskell
import Control.Arrow (arr)
import Control.Category
import Data.Label.Mono (Lens, iso)
import Data.Label.Partial (modify)
import Data.Label.Point (Iso(..))
import Prelude hiding ((.))
import Safe (readMay)
import qualified Data.Label.Base as L

readShow
  :: (Show a, Read a)
  => Lens Partial String a
readShow = iso (Iso r s)
  where r = Kleisli readMay
        s = arr show

> modify (readShow . L.snd) (*4) (1, "2")
Just (1, "8")

> modify (readShow . L.snd) (*4) (1, "-")
Nothing
```

## Views using Applicative

We have seen how to compose lenses __horizontally__ using the `(.)` operator
from `Control.Category`. We can also compose lenses __vertically__ using an
instance of the `Applicative` type class. Vertical composition gives us a small
DSL for building isomorphic lenses. Isomorphic lenses can be seen as data types
as views on another data type. The following example might clarify this idea:

```Haskell
view :: Address :-> (String, String, Int)
view = Mono.point $
  (,,) <$> L.fst3 >- city
       <*> L.snd3 >- L.fst . street
       <*> L.trd3 >- L.snd . street
```

Here we create a view on the `Address` data type as a 3-tuple. 

We connect the lenses for 3-tuples to the lenses for the `Address` data type
using the `(>-)` operator, which can be read as _points-to_. Now we use the
`Applicative` instance on the connected lenses together with the output
constructor `(,,)` into a proper view. Because we cannot directly make `Lens`
an instance of `Applicative` (the types don't allow this) we use an more
generic type
[`Point`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Point.html#v:Point)
underneath. We can turn a `Point` back into a proper lens using the `point`
function.

Using the view we can now use to 3-tuples to manipulate addresses.

```Haskell
> let addr = Address "-" ("Neude", 1)
> set (L.fst3 . view) "Utrecht" addr
Address "Utrecht" ("Neude", 11)
```

For multi-constructor type we can use the `Point` instance for `Alternative` to
get views on all the constructors. For example, to swap the sides of an
`Either`:

```Haskell
swapE :: Either a b :~> Either b a
swapE = Poly.point $
      Left  <$> L.left  >- L.right
  <|> Right <$> L.right >- L.left
```

## Deriving lenses in scope

There are three basic ways to derive labels for Haskell data types. The first
method we have seen above with the `Address` example is to derive labels in
scope for a data type with all the record fields prefixed with an underscore.
We can use the function
[`mkLabel`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:mkLabel),
or
[`mkLabels`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:mkLabels)
for this.

A more general version [`mkLabelsWith`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:mkLabelsWith) can be used to configure the deriving
process. We can decide to generate type signature or not, we can create
concrete type signatures or leave the lens working in an abstract category. We
can decide whether to let partial lenses fail silently using `ArrowZero` or
preserve the error with `ArrowFail e`. We can provide a custom renaming
function so we can use an different naming strategy than just stripping
underscores.

## Lenses as expression

A second derivation method is to derive labels as expressions in a n-tuple
using the [`getLabel`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:getLabel) function. This allows us to pattern match on the tuple and
provide custom names and types for the lenses. This is especially useful for
deriving lenses for existing non-record data types. Most of the lenses in
[`Data.Label.Base`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Base.html)
are derived this way. For example, the lenses for the `Either` type:

```Haskell
left  :: (Either a b -> Either o b) :~> (a -> o)
right :: (Either a b -> Either a o) :~> (b -> o)

(left, right) = $(getLabel ''Either)
```

Because of the abstract nature of the generated lenses and the top level
pattern match, it might be required to use
[`NoMonomorphismRestriction`](http://www.haskell.org/haskellwiki/Monomorphism_restriction)
in some cases.

## Direct derivation

The last option is to derive labels for a record by directly wrapping the
definition with the
[`fclabels`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:getLabel)
function. The wrapped record definition will be brought into scope together
with the derived labels. The labels will be named exactly as in the record
definition, the original fields will be stripped from the data type.

```Haskell
fclabels [d|
  data Record = Record
    { fieldA :: Int
    , fieldB :: Bool
    } deriving Show
  |]

> modify fieldX (+2) $ Record 1 False
Record 1 False
```

There are several advantages to this approach: there is no need for all the
underscores prefixes, there is no need to explicitly hide the original labels
from being exported from the module, and the record fields will not show up in
the `Show` instance.

Multiple data types are allowed within one quotation passed to `fclabels` and
all non-type declaration will be brought in scope untouched.

## Lenses for GADTs

We can now also derive labels for GATDs, which wasn't possible in the previous
version. The interesting aspect of (some) GADTs is that even when the type
might have multiple constructors, the record fields might still be total. This
can happen when the type indices of the GADT are restrictive enough. For example:

```Haskell
data Gadt a where
  P :: { _fa :: a, _fb :: b } -> Gadt (a, b)
  Q :: { _bi :: (Bool, Int) }    Gadt (Bool, Int)
  R :: { _ls :: [Int]       } -> Gadt [Int]

mkLabel ''Gadt
```

This will bring the partial labels `fa`, `fb` and `bi` in scope together with
the total label for `ls`. A simple totaility checker will try to figure out if
label type overlap or not.

Not that this feature has not been proven correct for combination of indices
yet, but for now it seems to cover a great deal of cases.

<hr>

Install using Cabal from [Hackage](http://hackage.haskell.org/package/fclabels)

Source code on [github](https://github.com/sebastiaanvisser/fclabels)

<!--
Discussion on [Reddit](http://www.reddit.com/r)
-->

</article>
