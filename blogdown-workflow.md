
### Recipe for writing a new post

  - Start up an R session in your website directory, and use
    `blogdown::new_post()` to generate an RMarkdown file for your post
    (you’re not recommended to do it yourself).

<!-- end list -->

``` r
blogdown::new_post(
              title = "A recipe for writing a new post",
              ext = ".rmd"
          )
```

  - To preview your website, run this:

<!-- end list -->

``` r
blogdown::serve_site()
```
