<article>

<div class=meta>
<span class=author>[Sebastiaan Visser](/about.html)</span>
<span class=date>March 4th, 2013</span>
</div>

# Responsive design with Clay

Responsive websites are totally hip and not without reason. Sites that
automatically adjust their layout to fit the screen size can improve usability
a lot. With the ever growing supply of smart phones and tablets no assumptions
can safely be made about the screen size your visitors. Because of its
combinatoric design, building responsive sites with [Clay](/clay) is really
easy. I'll quickly explain how I made this website work on smaller screen sizes
by making use of [CSS media queries](http://www.w3.org/TR/css3-mediaqueries).

The content section of this website is 600 pixels wide and keeps itself
horizontally centered in the browser window. Centering elements can easily be
done in CSS by setting both the left and right margin of a block element to
`auto`{.haskell}. In Clay this would look like this:

```haskell
import Clay

main :: IO
main = putCss content

content :: Css
content = ".content" ?
  do width       siteW
     marginLeft  auto
     marginRight auto

siteW :: Size Abs
siteW = px 600
```

Although 600 pixels is probably already pretty narrow for a website, there
might be people with even less screen real estate. Possibly because they are
using a non-retina mobile device or simply because they resized their browser
window. We can use media queries to gracefully resize our body column back for
lower screen widths. First we need to import the Clay media query module,
preferably qualified to avoid name clashes:

```haskell
import qualified Clay.Media as Mq
```

Now we can create two queries to represent the layout states in which our site
can be, either the screen is wider than the defined page width or narrower.

```haskell
wide, narrow :: Css -> Css

wide   = query Mq.all [Mq.minWidth siteW]
narrow = query Mq.all [Mq.maxWidth siteW]
```

We can use the two new combinators everywhere in our stylesheet where we want
to dispatch based on screen width. So, we create a new version of our
<code>content</code> function that has the 600 pixels centered layout on wide
screens and a 100% width version on smaller screens.

```haskell
content :: Css
content = ".content" ?
  do narrow $ do width       (pct 100)
     wide   $ do width       siteW
                 marginLeft  auto
                 marginRight auto
```

That's it! We just made our site a bit more usable on smaller screen sizes by
not always sticking to the predefined page width. You can see that it works by
resizing your browser window to below the page width and see that it scales
appropriately. This is an extremely simple example of course, but by defining
your own media query combinators this approach scales up to larger sites
nicely.

When interested, you can find the full source code of this site, including the
Clay stylesheet, [on my
github](https://github.com/sebastiaanvisser/sebastiaanvisser.github.com).

<hr>

Discuss on [Reddit](http://www.reddit.com/r/haskell/comments/19lpzv/responsive_design_with_clay/) or
[Hacker News](http://news.ycombinator.com/item?id=5315653).

</article>
