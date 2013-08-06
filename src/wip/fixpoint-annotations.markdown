<article>

<div class=meta>
<span class=author>Sebastiaan Visser</span>
<span class=date>March 6th, 2013</span>
</div>

# Fixpoint annotations

## intro

3 years after graduation
thesis into blog post
thesis can be found here, presentation can be found here

## generic folds

well know trick for generic folds over recursive datatypes
lets say we have a tree with two functions
data Tree ...
lookup/fromList
now open up recursion
data TreeF
now Tree = Fix TreeF
generic fold unfold
slight change to lookup/fromList

## annotations

now we can add functionalities using annotation
type class for pointed (wrapping) and copointed (unwrapping) in some monad
specialize the fold and unfold using annotation
example: data Trace
print when wrapping unwrapping
now fromList and lookup print out trace, generically

## pointer annotation

lets say we have a two computations for read from block, allocate+write new block
in some IO based monad Heap, returning Pointers
now we can make this into an annotation


</article>
