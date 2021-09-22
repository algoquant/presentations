---
title: Hello World!
date:   2016-06-25 12:00:00
layout: post
categories: websites
output: html_document
---

### I finally took the time to set up my home page on GitHub

Unfortunately it forced me to learn more about coding in *HTML*, *CSS*, *YAML*, and [*liquid*](https://shopify.github.io/liquid/){:target="_blank"} than I ever cared to know.  Fortunately I discovered a decent amount of blogs and tutorials online to be able to set up the website without too much pain.  


### Selecting *Jekyll* for generating the website

There are many different website technologies for creating static websites.  Since I chose to host my website on [GitHub](https://pages.github.com/){:target="_blank"} (where I already have a repository for other projects), I decided to explore website technologies used by other GitHub users.  
As it happens, GitHub has developed a website generator written in Ruby, called [*Jekyll*](https://jekyllrb.com/){:target="_blank"}, which has become very popular, so I chose to use *Jekyll* for building my website as well. 


### Selecting a *Jekyll theme*

A *Jekyll theme* is a set of *HTML* and *CSS* files that determine the appearance of a website, like fonts, colors, shading, and page layouts.  A *Jekyll theme* can be adapted to create websites with the same appearance as the theme.  
I wanted to choose a visually attractive theme, but with additional features, like a sidebar containing icons and text, with links to my website pages and to other websites.  I chose to adapt the 
<a href="https://github.com/pietromenna/jekyll-architect-theme" target="_blank"> Jekyll-Architect theme</a> 
by 
<a href="https://github.com/pietromenna" target="_blank"> Pietro Menna</a>, 
and the 
<a href="https://github.com/daattali/beautiful-jekyll" target="_blank"> Beautiful-Jekyll theme</a> 
by 
<a href="https://github.com/daattali" target="_blank"> Dean Attali</a>.
Here you can find more: [Jekyll themes](http://jekyll.tips/templates/){:target="_blank"}.  
I also added *Disqus* comments to my blog posts.


### You're welcome to adapt my website for your own needs

You can download the website files using the buttons on the right sidebar.  
You can read about how to set up this website in the [README.md]({{ site.name | prepend: site.githuburl }}/blob/master/README.md){:target="_blank"} file.  But be aware that this website isn't a static website theme, but rather it's a complete website, and its contents change over time.
Please follow the same copyright rules as those for the original website themes that it's based on.


### Building websites using *R Markdown* and *knitr*

I use *R* for all my work, so in the future I may also try building my websites using *R Markdown* and *knitr*.  Here are some references about building websites using *R Markdown* and *knitr*:

[Building websites using *R Markdown* and *knitr*](https://brendanrocks.com/blogging-with-rmarkdown-knitr-jekyll/){:target="_blank"}  
[Hacking *htmlwidgets* into website using *rmarkdown*](https://brendanrocks.com/htmlwidgets-knitr-jekyll/){:target="_blank"}  
[Hacking *htmlwidgets* into website using *Jekyll*](http://benjcunningham.org/2016/06/13/hacking-together-htmlwidgets-for-jekyll.html){:target="_blank"}  
