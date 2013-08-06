<article>

<div class=meta>
<span class=author>[Sebastiaan Visser](/about.html)</span>
<span class=date>March 8th, 2013</span>
</div>

# Upside down mouse pointer

Something I never realised before, but controlling your mouse pointer in the
web browser is really easy. Move over the map below for a demo.

<style>
body
{
  padding: 0;
  margin: 0;

}
.content
{
  min-height: 720px;
}
#australia
{
  margin-top: 48px;
  position: relative;
  background: url("../../../../image/australia.png");
  width: 552px;
  height: 552px;
  cursor: none;
}
#australia img
{
  display: none;
}
#australia:hover img
{
  display: block;
}
#australia img
{
  position: absolute;
  left: 40px;
  top: 40px;
}
</style>
<div id=australia><img src=/image/worra.png></div>
<script>
var w = 552;
$("#australia").mousemove
( function (ev)
  {
    $("#australia img").css("left", (w - (ev.offsetX || ev.originalEvent.layerX || 0) - 3) + "px");
    $("#australia img").css("top",  (w - (ev.offsetY || ev.originalEvent.layerY || 0) - 3) + "px");
  }
);
</script>

</article>
