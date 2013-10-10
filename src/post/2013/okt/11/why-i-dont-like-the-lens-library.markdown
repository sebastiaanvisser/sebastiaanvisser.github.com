
<article>

<div class=meta>
<span class=author>[Sebastiaan Visser](/about.html)</span>
<span class=date>Oktober 11th, 2013</span>
</div>

# Why I don't like the lens library

As some of you know I'm the author of `fclabels`, that other lens library that
isn't `lens`. I recently released a new version and I'm very happy with result.
But I knew beforehand that usage would probably be extremely low. Mainly
because of the popularity of the `lens` library.

And I'm fine with that.

I never build `fclabels` because I wanted people to use my software (maybe just
a bit), but I wanted a nice solution for Haskell's non-composable record
labels. I think both `fclabels` and `lens` nicely solve this problem, both with
slightly different technology and philosophy.

Van Laarhoven lenses are a very clever trick that gives rise to great
extensibility, a great basis for a lens library. One of the reasons I actually
considered giving up on `fclabels` in favor of `lens`. It will save me time as
a maintainer and community consensus is mostly in favor of `lens`.

But after closer inspection I didn't. Partially because I still think the
design choices in `fclabels` add something to the ecosystem, but mostly because
I think that `lens` is a vastly over-engineered, intimidating, complex beast. I
tried really hard, but I can't get myself to like the package.

Maybe an extreme example, but opening the package and just randomly clicking
some modules could end up with things like this:

```Haskell
class (Choice p, Corepresentable p,
       Comonad (Corep p), Traversable (Corep p),
       Strong p, Representable p, Monad (Rep p),
       MonadFix (Rep p), Distributive (Rep p),
       ArrowLoop p, ArrowApply p, ArrowChoice p
       ) => Conjoined p where
  distrib :: Functor f => p a b -> p (f a) (f b)
  conjoined :: (p ~ (->) => q (a -> b) r) -> q (p a b) r -> q (p a b) r
```

I cannot be the only that thinks this might be **just a little bit too much**.

In my opinion `lens` doesn't have enough focus, has an extremely intimidating
API and exposes way to many utilities that are only useful in handful of
use-cases. It wants to solve every problem of data access at once, at the cost
of simplicity and elegance. Simply put, I think `lens` is very cool technology,
everything in there has some nice theoretical underpinning, but from an
end-user perspective it's very badly designed.

I know that not everyone agrees with me, but I also know a lot of people do
think the package is intimidating.

Haskell always attracted me because of small and simple libraries, that focus
on solving small problems. Bigger problems are mostly solved by composition of
smaller solutions instead of adding more primitives. Van Laarhoven lenses allow
for exactly that, `lens` as a library feels like the exact opposite.

If we all agree that lenses are an essential addition to the haskell ecosystem,
can't we work towards a small and simple solution? One that we can safely put
in the platform and show to first time Haskellers?

No, I do not propose `fclabels`. Can't we just get the essential core ideas out
of `lens` and put that in a library? A *small* library, with a very carefully
crafted interface? No corepresentable distributive indexed coreps, no category
theory, not tons of 'convenient' operators. Just lenses. Maybe traversables and
isomorphisms. Everything else can be build on top.

<hr>

<!--
Discussion on [Reddit](http://www.reddit.com/r)
-->

</article>
