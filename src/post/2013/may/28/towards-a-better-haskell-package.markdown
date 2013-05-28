<article>

<div class=meta> <span class=author>Sebastiaan Visser</span> <span
class=date>May 28th, 2013</span> </div>

# Towards a better Haskell package

Hackage, the package repository for the [Haskell](http://www.haskell.org)
programming language, has around 5000 package nowadays. This is a lot! More
libraries for a programming language can generally be considered a good thing.
But most developers know that package quality varies and not all libraries are
easy to work with. There are some general guidelines that can make a package
more user friendly: focus the public interface and documentation on *usage* not
implementation, keep code simple and don't over-engineer, be careful with
chances between versions, etc.

I would like to share set of guidelines I compiled over time and use in an
attempt to make my own Haskell package easier to work with.

## Have a clear module structure

From a usability perspective the module structure of a package really matters.
Modules are the top level building blocks of a library and tell a lot about
what is has to offer.

* *Keep the module hierarchy flat.* Deeply nested hierarchies might
  be correct from a theoretical point of view, but don't necessarily benefit
  the user. Shallow hierarchies are easier to grasp and bring focus to the
  modules that are important.

* *Use a short top-level namespace.* There is no need for long
  module prefixes when a creatively chosen package name suffices. A quick
  `import Parsec` is more convenient than `import
  Text.ParserCombinators.Parsec`. Top level namespaces like `Control`, `Data`,
  and `Text` rarely add anything.

* *One import, batteries included.* Have one top-level module that
  exposes the biggest sane default of the library as possible. Requiring users
  to import several modules to get even the most basic functionality is
  inconvenient. A common trick is directly export the most important functions
  through the top-level module and expose everything else using module
  re-exports.

## Document package usage

Good documentation is a no-brainer upon which everyone agrees. Writing proper
documentation takes a lot of work though. Maybe more than we can expect of
every open-source volunteer and hobbyist. There are some ways to make a small
amount of documentation matter. When a user finds out how to use the package on
a high-level, they'll figure out the details out themselves eventually.

* *Focus documentation on usage, not implementation.* Focus on the main use
  case, don't go into detail about the billion ways a generic functions can be
  configured to work under all circumstances. The actual implementation of a
  library is hardly useful to users that doesn't even know how to use the API.

* *Think about the generated documentation.* The
  [Haddock](http://www.haskell.org/haddock/) documentation of a
  package is probably the first thing users see. Make sure there is some
  structure in there, similar functions grouped together, types first, use
  section headers. Add some documentation that explains what the functions and
  datatypes have to offer.

* *Be honest about the shortcomings.* Users will figure them out anyways.
  Clearly documenting things that don't work as expected can save people a lot
  of time.

* *Create a website with a tutorial.* Documentation within the package is good
  for reference purposes, but is rarely a good starting point. Just a few steps
  to get up and running is good enough most of the time. Haskell code can be
  abstract and vague sometimes, concrete examples help.

## Keep the code simple

Haskell beginners often claim Haskell is a complicated language requiring a big
change in the way they think about programming. As if a paradigm shift
isn't hard enough already, some Haskell packages present their code and their
API in a more complicated way than necessary.

* *Keep type signatures simple.* Complicated type signatures are hard to grasp,
  not only for beginners. Haskell allows for some really generic code and
  additional type parameters sneak in easy. Keep the package internals general,
  but expose sane defaults for a first time user.

* *Avoid type classes.* Type classes are wonderful technology but are by far
  the easiest way to obscure a library. Avoid them whenever possible. Sometimes
  it's better to provide a few concrete helper functions to deal with different
  types in an API, than to introduce to a type class to abstract them away.
  Don't hide complexity behind magic.

* *Avoid useless identifier prefixes.* Code for qualified imports. Too much
  two and three letter prefixes makes code cryptic and less appealing. Use
  simple and short identifiers that make sense in the context of the package.
  Don't be afraid for names clashes with the Prelude or other packages,
  explicit imports and qualified import solve this problem.

## Keep the package stable

Chances are a package isn't perfect, especially the first few releases. Be
clear about what it does and what it doesn't. Assume people are actually using
the package and changes to it [will break their workflow](http://xkcd.com/1172)

* *Stick to the Package Versioning Policy.* The PVP allows developers to keep
  their stack stable and avoid broken builds. Sneaking breaking changes into a
  minor bump will most likely result in some red dots on a build server
  somewhere.  Although it can be useful to ignore version upper bounds when
  developing, removing this precious information from the version number would
  be a shame.  Instructing our build tools build to ignore upper bounds might
  be a better solution than skipping them altogether.

* *Prune the dependencies.* More dependencies make packages harder to install
  and easier to break. Don't depend on entire stacks of extra dependencies just
  for a few simple helper functions. Avoid reinventing the wheel, but be
  pragmatic.

* *Don't hide big impact chances behind minor bumps.* Some changes are major
  but don't change the API, be careful with those. Silently changing a network
  request timeout to a tenth of it's original value in a minor upgrade might
  break the user's network stack. Fixing an encoding bug that has been the
  package for years? Chances are someone assumed this was intended and now has
  a double encoding bug. Be explicit.

* *Have a changelog.* Seeing what has changed between version is invaluable
  information. Both in what the new version can add in value, and what it might
  break. Digging through git logs before an upgrade is dreadful.

## Encourage collaboration

Packages are never finished, the more people work on it the faster it will
improve. There are some tricks to simplify collaboration.

* *Expose internals.* Export them either in a separate package or
  in a namespace separate from the public API. Forking remains expensive.
  Avoiding a fork by allowing a `Binary` instance for the otherwise opaque data
  constructor through an internals module can save a lot time.

* *Think about licensing.* The reusability of a package depends greatly on the
  type of license used. Everyone has their own opinion about open source
  software and a variety of licenses are available. At least give it a thought
  and don't just pick one.

* *Host on Github.* Github makes forking and pushing changes
  upstream extremely easy. Forking a package, fixing a bug, requesting a merge
  back, and a new deploy to Hackage can all be done in a matter of hours. Don't
  like Github? Use one of their competitors. Self hosting rarely benefits
  collaboration.

The points on my list mostly arrive from my experience with existing Haskell
libraries, most of which very high quality. I would be lying if I claimed my
own packages conform to this list entirely. Luckily packages can be developed
and updated incrementally while knowledge and insight changes.

After seeing my share of real world Haskell I'm very much impressed by the
quality of libraries, but we should never stop thinking about how we can
improve.

<hr>

Disagree or do you have anything that should be added? Let me know!

<!--
Discuss on [Reddit](http://www.reddit.com/r/haskell/comments/XXX/) or
[Hacker News](http://news.ycombinator.com/item?id=XXX).
-->

</article>
