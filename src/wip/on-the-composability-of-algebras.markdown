<article>

<div class=meta>
<span class=author>Sebastiaan Visser</span>
<span class=date>March 6th, 2013</span>
</div>

# On the composability of algebras

I'm a fraud! Almost three years after finishing my degree I discovered a major
flaw in chapter X.X of my MSc thesis. My thesis contains a chapter on the
composability of initial algebras using `Applicative` functors. The code I
published doesn't work. The definition is circular, and while circular logic is
the best kind of logic (because it's circular) circular defintions mostly just
make your programs loop. I started playing around with the code again in order
to find a working solution to the composability of algebras, and it proved to
be hard. (At least for me it was.)

Algebras are the driver functions for generic folds. Everyone knows the basic
`foldr` function from the haskell `Prelude`, it recursively destructs a list
into a single value.

</article>
