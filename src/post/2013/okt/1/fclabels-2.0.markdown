
<article>

<div class=meta>
<span class=author>[Sebastiaan Visser](/about.html)</span>
<span class=date>Oktober 1st, 2013</span>
</div>

# fclabels 2.0

I'm excited to announce a brand new major version of
[`fclabels`](http://hackage.haskell.org/package/fclabels). This Haskell library
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

There are now several ways to derive the labels from Haskell data types, for
both record and non-record types. We can derive labels in scope (the existing
method), but we can now also generate them as expressions that can be named and
typed manually. Data type declarations can also directly be translated into a
variant with first class labels, without needing the underscore prefixes.
Examples of the possible derivation methods [can be found
below](#deriving-lenses-in-scope).

The new version has full support for deriving labels for the fields in
generalized algebraic data types (GADTs). It contains a sufficiently
intelligent totality checker that looks at the indices of a GADT to decide
whether to derive the labels as total or partial lenses.

A small set of predefined lenses is included for some of Haskell's base types,
like `Maybe`, `Either`, lists and tuples.

For an introduction into this package read the rest of this blog post or see the
[API documentation on Hackage](http://hackage.haskell.org/package/fclabels) or
[the source code on github](https://github.com/sebastiaanvisser/fclabels).

__Note that the library is heavily optimised for qualified imports and some
names may occurs multiple times across the package. The module name should
provide enough context.__

## Basic usage

Let's say we have a simple record data type that represents an address. We can
automatically derive first class labels for this type by prefixing the
field names with an underscore and invoking the Template Haskell function
[`mkLabel`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:mkLabel):

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

The labels are implemented as lenses, which means _getter_ and _modifier_
functions packed in one data type. The lens type above uses a function like
operator
[`(:->)`](http://hackage.haskell.org/package/fclabels/docs/Data-Label.html#t::-45--62-).
We can use the `get`, `modify`, and `set` function to read or write values.

```
get    :: (f :-> a) -> f -> a
set    :: (f :-> a) -> a -> f -> f
modify :: (f :-> a) -> (a -> a) -> f -> f
```

Although not enforced by the type system, for predictable usage of the library
we expect the following law to hold:

```
get lens (modify lens m f) == m (get lens f)
```

Because the `Address` data type contains only one constructor and doesn't have
any type variables, the derived labels are __total__ and __monomorphic__. More
lens types are available in this library as will be explained in the [next
section](#different-lens-types).

The labels can be used to get and update values from an `Address`:

```Haskell
rijksmuseum = Address
  "Amsterdam" ("Museumstraat", 1)

> get city rijksmuseum
"Amsterdam"

> modify city (map toUpper) rijksmuseum
Address "AMSTERDAM" ("Museumstraat", 1)
```

Labels can be composed with other labels to dig deeper into a data type. For
example, we can access the individual components of the street tuple:

```Haskell
import Control.Category
import Prelude hiding ((.))
import qualified Data.Label.Base as L

> modify (L.snd . street) (+ 99)
Address "Amsterdam" ("Museumstraat", 100)
```
Composition in `fclabels` is done using the `(.)` function from
[`Control.Category`](http://hackage.haskell.org/package/base/docs/Control-Category.html),
a generalization of function composition. This way lens composition reads
exactly like function composition.

This example explains the essence of the library, derivation of labels for our
data types that are implemented as fully composable lenses. The lenses are
first class and can be passed around and extended as we wish. Lenses allow both
reading values from and writing values to a data type.

## Different lens types

The package provides several different lens types. The two main distinctions we
make is between lenses that allow only monomorphic updates and lenses that
allow polymorphic updates, and between total lenses and partial lenses. All
four combinations are possible.

As explained above, the lens in the `Address` example is a total monomorphic
lens. But, for example, a lens that points to the value in the `Left`
constructor of an `Either` will be of a different type: __partial__ and
__polymorphic__. It is partial because the accessor functions might fail in the
case of a `Right` constructor, it is polymorphic because the type of the value
can change on update.

All the lens types are built by specialization of a _base lens_ type. The base
lens type is an __abstract polymorphic__ lens. Abstract means that the getters
and setters are not just Haskell functions, but can run in a custom `Category`,
allowing effects. For example, totality and partiality are considered effects.

The code block below lists the most important lenses from the library together
with some convenient type synonyms. It might look like a lot at first, but you
will notice a very simple and regular pattern:

```Haskell
-- Abstract polymorphic (base) lens:

  Lens cat (f -> g) (o -> i)

-- Abstract monomorphic lens:

  Lens cat f o

-- Total polymorphic lens and synonyms:

  Lens (->) (f -> g) (o -> i)
  Lens Total (f -> g) (o -> i)
  (f -> g) :-> (o -> i)

-- Total monomorphic lens and synonyms:

  Lens (->) f o
  Lens Total f o
  f :-> o

-- Partial polymorphic lens and synonyms:

  Lens (Kleisli Maybe) (f -> g) (o -> i)
  Lens Partial (f -> g) (o -> i)
  (f -> g) :~> (o -> i)

-- Partial monomorphic lens and synonyms:

  Lens (Kleisli Maybe) f o
  Lens Partial f o
  f :~> o
```

You get the pattern.

Because all the lenses in the package are specializations of the same base lens
they can all be fully composed with each other.

## Polymorphic and monomorphic lens types

The two function types in the signature for the polymorphic lenses indicate
that by modifying the value in the record field from `o -> i` the data type
changes from `f -> g`. This makes the lens allow polymorphic updates.

Lenses for the components of a tuple are nice examples of polymorphic lenses:

```Haskell
import Data.Label.Base (fst, snd)

fst :: Lens arr ((a, b) -> (o, b)) (a -> o)
snd :: Lens arr ((a, b) -> (a, o)) (b -> o)
```

In the case of
[`fst`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Base.html#v:fst)
modifying the value of type `a` to type `o` will update the entire tuple form
`(a, b)` to `(o, b)`. Something similar happens for
[`snd`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Base.html#v:snd)
and the second component of the tuple.

Monomorphic lenses simply specialise polymorphic lenses by having the same
input and output types. A synonym from
[`Data.Label.Mono`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Mono.html#t:Lens)
can be used to simplify the type:

```Haskell
import qualified Data.Lens.Poly as Poly
import qualified Data.Lens.Mono as Mono

Poly.Lens cat (f -> f) (o -> o)

-- or simply:

Mono.Lens cat f o
```

## Specializing contexts

The base lens is abstract which means that we can use it in different
computational contexts. We can specialize the context to allow different
effects.

By specializing the `cat` variable to normal function space `(->)` we get back
total lenses. By specializing to `Kleisli Maybe` we end up with a partial lens
that fails silently. By specializing to `Kleisli (Either e)` we end up with a
partial lens that can fail with some error value. The library provides
convenient names for these commonly used lens contexts:

```Haskell
type Total     = (->)
type Partial   = Kleisli Maybe
type Failing e = Kleisli (Either e)

Lens Total       (f -> g) (o -> i)
Lens Partial     (f -> g) (o -> i)
Lens (Failing e) (f -> g) (o -> i)
```

Specialized versions of the lens utilities are provided by the library in the modules
[`Data.Label.Total`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Total.html),
[`Data.Label.Partial`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Partial.html) and
[`Data.Label.Failing`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Failing.html).

For total lenses we provide the shortcut operator
[`(:->)`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Total.html#t::-45--62-)
and for partial lenses we provide
[`(:~>)`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Total.htmlt::-126--62-).

By default, labels derived using the Template Haskell functions that come with
the library are abstract. They are not yet specialized to a concrete context.
This is useful for composition. For example, we can freely lift total lenses
into partial lenses and compose them with other partial lenses. Of course,
conceptually we cannot lift partial lenses into total lenses. By using custom
`Arrow` type classes (like
[`ArrowZero`](http://hackage.haskell.org/package/base/docs/Control-Arrow.html#t:ArrowZero)
and
[`ArrowFail`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Point.html#t:ArrowFail))
we are actually able to enforce those constraints using the type system.

We can imagine this approach nicely scaling up to different effects.  We could
use `Kleisli IO` to build lenses for `IORef`s, `Kleisli STM` to build lenses
for `TVar`s, or some `Database` category to build lenses pointing to some
record in an external database.

## Isomorphisms

The library contains a type `Iso` which represents an isomorphism, which is a
bidirectional function. Like lenses isomorphisms don't use Haskell functions
directly, but work in some custom `Category`.

```Haskell
data Iso cat i o = Iso (cat i o) (cat o i)
```

To clarify, specialized to normal function space this isomorphism would just be
a pair of `a -> b` and `b -> a`.

Isomorphisms can be lifted into a lens using the `iso` function. After lifting
they can be freely composed with other lenses. The library provides both a
function for embedding monomorphic and polymorphic isomorphisms. Embedding an
isomorphism into a lens will preserve the original context.

For example, we can create a partial lens from a `Read`/`Show` isomorphism:

```Haskell
import Safe (readMay)

readShow
  :: (Show a, Read a)
  => Isomorphism Partial String a
readShow = Iso r s
  where r = Kleisli readMay
        s = Kleisli (Just . show)

asFloat :: Mono.Lens Partial (Int, String) Float
asFloat = iso readShow . L.snd

> Partial.modify asFloat (*4) (1, "2.1")
Just (1, "8.4")

> Partial.modify asFloat (*4) (1, "-")
Nothing
```

The `readShow` isomorphism is included in
[`Data.Label.Base`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Vase.htmlv:readShow).

## Views using Applicative

We have seen how to compose lenses __horizontally__ using the `(.)` operator
from `Control.Category`. We can also compose lenses __vertically__ using an
instance of the `Applicative` type class. Vertical composition gives us a small
DSL for building views. The following example might clarify this idea:

```Haskell
asTup3 :: Address :-> (String, String, Int)
asTup3 = Mono.point $
  (,,) <$> L.fst3 >- city
       <*> L.snd3 >- L.fst . street
       <*> L.trd3 >- L.snd . street
```

Here we create a view on the `Address` data type as a 3-tuple. 

We connect the lenses for 3-tuples to the lenses for the `Address` data type
using the `(>-)` operator, which can be read as _points-to_. Now we use the
`Applicative` instance on the connected lenses together with the output
constructor `(,,)` to create a view on the `Address` type. Because we cannot
directly make `Lens` an instance of `Applicative` (the types don't allow this)
we use an more generic type
[`Point`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Point.html#v:Point)
underneath. We can turn a `Point` back into a proper lens using the
[`point`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Mono.html#v:point)
function.

Using the view we can now use 3-tuples to manipulate addresses.

```Haskell
> let addr = Address "-" ("Neude", 1)
> set (L.fst3 . asTup3) "Utrecht" addr
Address "Utrecht" ("Neude", 11)
```

For multi-constructor types we can use the `Alternative` instance for `Point`
to get views on all the constructors. For example, to swap the sides of an
`Either`:

```Haskell
swapE :: Either a b :~> Either b a
swapE = Poly.point $
      Left  <$> L.left  >- L.right
  <|> Right <$> L.right >- L.left
```

## Deriving lenses in scope

There are three basic ways to derive labels for Haskell data types in
`fclabels`. The first method, which we've already seen with the `Address` example,
is to derive labels in scope for all the record fields prefixed with an
underscore. We can use the function
[`mkLabel`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:mkLabel),
or
[`mkLabels`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:mkLabels)
for this.

A more general version [`mkLabelsWith`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:mkLabelsWith) can be used to configure the deriving
process. We can decide to generate type signature or not, we can create
concrete type signatures or keep the lens abstract. We can decide whether to
let partial lenses fail silently using
[`ArrowZero`](http://hackage.haskell.org/package/base/docs/Control-Arrow.html#t:ArrowZero)
or preserve the error with
[`ArrowFail`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Point.html#t:ArrowFail).
We can provide a custom renaming function so we can use an different naming
strategy than just stripping underscores.

## Lenses as expressions

A second derivation method is to derive labels as expressions in a n-tuple,
using the
[`getLabel`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:getLabel)
function. This allows us to pattern match on the tuple and provide custom names
and types for the lenses. This is especially useful for deriving lenses for
existing non-record data types. Most of the lenses in
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

The third option is to derive labels for a record by directly wrapping the
declaration with the
[`fclabels`](http://hackage.haskell.org/package/fclabels/docs/Data-Label-Derive.html#v:fclabels)
function. The wrapped record definition will be brought into scope together
with the derived labels. The labels will be named exactly as in the record
definition, the original fields will be stripped from the data type:

```Haskell
fclabels [d|
  data Pt = Pt
    { ptX :: Double
    , ptY :: Double
    } deriving Show
  |]

> modify ptX (+ 2.0) (Pt 1.0 2.0)
Record 3.0 2.0
```

There are several advantages to this approach: there is no need for the
underscore prefixes, there is no need to explicitly hide the original labels
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
  Q :: { _bi :: (Bool, Int) } -> Gadt (Bool, Int)
  R :: { _ls :: [Int]       } -> Gadt [Int]

mkLabel ''Gadt
```

This will bring the partial labels `fa`, `fb` and `bi` in scope together with
the total label for `ls`. A simple totaility checker will try to figure out if
label types overlap or not.

Note that this feature has not been proven correct for all combination of
indices yet, but for now it seems to cover a great deal of cases.

<hr>

Install using Cabal from [Hackage](http://hackage.haskell.org/package/fclabels)

Source code on [github](https://github.com/sebastiaanvisser/fclabels)

<!--
Discussion on [Reddit](http://www.reddit.com/r)
-->

</article>
