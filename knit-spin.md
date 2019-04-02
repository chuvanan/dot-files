

## What is spin?

`knitr::spin()` converts formatted R scripts into html output. That's it.

Main advantage: Keep your whole analysis in R script only. No need R Markdown.

## How to spin

* Any line beginning with `#'` is treated as a markdown directive

* Any line beginning with `#+` is parsed as code chunk options

* `knitr::spin()` on the file
