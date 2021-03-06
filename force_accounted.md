---
title: ""
output:
  html_document:
    keep_md: true
    self_contained: false
  md_document:
    variant: markdown_github
---
<style>
html, body, div, h1, h2, h3 {
  background:black; 
  background-color:black;
  color:white
}
a {
  color: #78dcff;
}
div { padding-top:30px; padding-bottom:30px; }
</style>



<center><img style="max-width:100%" src="title.svg"/></center>
<center><h1>IN</h1></center>
<center><img style="padding-bottom:30px" src="Rlogo.png"/></center>

This is what happens when you carelessly tweet things like this:

<blockquote class="twitter-tweet" lang="en"><p lang="en" dir="ltr"><a href="https://t.co/R3qVHEMTLo">https://t.co/R3qVHEMTLo</a> <a href="https://twitter.com/hrbrmstr">@hrbrmstr</a> is there an R package? ;)</p>&mdash; Thorsten G. (@SaThaRiel74) <a href="https://twitter.com/SaThaRiel74/status/685571947032395776">January 8, 2016</a></blockquote>
<script async src="http://platform.twitter.com/widgets.js" charset="utf-8"></script>

It turns out this is a pretty decent data munging and ggplot2 wrangling exercise. If you web-inspect the Bloomberg page (if it's not obvious I've fair-used some of their styles & images and their data) you'll find they use a few CSVs. I grabbed them and put them into a data folder and wrapped the reading of them into functions. If this were a "real" package I'd've put them in R data files. 

I didn't cheat and use widgets or a separate HTML+SVG combo this time. Everything is ggplot2. That also puts limitations (yes, ggplot2 has limits) on what we can do. So I've ggplot2-ized the emulated visualizations, starting with _"Good vs. Evil"_. There are some neat tricks here to get space for the *"Light | Episode | Dark"* label line and a custom label function to convert the time. While I could have spent time allowing for a space in the middle, it was easier to just put the episode numbers at `0` (and it looks kinda cool IMO).

<div style="background:black">
<img src="force_accounted_files/figure-html/unnamed-chunk-2-1.png" width="960" />
</div>

I was not about to deal with raster images and just leaned on ggplot2's strengths at core visualizations here (and they kinda look like light saber bars if you squint). You can learn how to do colored bar label annotations by looking at this example.

<div style="background:black">
<img src="force_accounted_files/figure-html/unnamed-chunk-3-1.png" width="960" />
</div>

I'm kinda proud of this one. This is some pretty complex use of facets, color/fill aesthetics and arranging the grobs. Again, no images since they're kind of a pain to use with the precision necessary. I _almost_ created a font from the images but I really had real stuff to do this weekend.

<div style="background:black">
<h1 style="color:white; padding:5px">Applied Force</h1>
<img src="force_accounted_files/figure-html/unnamed-chunk-4-1.png" width="960" />
</div>

Many of the Bloomberg visualizations rely heavily on interactive effects and _"May the Force Be Mentioned"_ also had images. I went with a static plot, sizing, coloring & labeling circles accordingly. Some more "fancy" facet work here for those inclined to poke at the code.

<div style="background:black">
<img src="force_accounted_files/figure-html/unnamed-chunk-5-1.png" width="960" />
</div>

This ended up being an exercise more in data wrangling than anything else, but it came out pretty well, despite the lack of interactivity.

<div style="background:black">
<img src="force_accounted_files/figure-html/unnamed-chunk-6-1.png" width="960" />
</div>

You can find all this [on github](https://github.com/hrbrmstr/forceaccounted), and may the FoRce be with you!
