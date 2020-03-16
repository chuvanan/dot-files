

local({

    ## CRAN's repo
    r <- getOption("repos")
    r["CRAN"] <- "https://cloud.r-project.org"
    options(repos = r)

    ## blogging
    options(
        servr.daemon = TRUE,
        blogdown.author = "An Chu",
        blogdown.ext = ".rmd",
        blogdown.subdir = "post"
    )

    ## general options
    options(width = 100)

    ## make cairo as default graphic engine
    setHook(packageEvent("grDevices", "onLoad"),
            function(...) grDevices::X11.options(type = 'cairo'))
    options(device = 'x11')

    ## warn on partial matches
    options(
        warnPartialMatchAttr = TRUE,
        warnPartialMatchDollar = TRUE,
        warnPartialMatchArgs = TRUE
    )

    ## fancy quotes are annoying and lead to 'copy + paste' bugs / frustrations
    options(useFancyQuotes = FALSE)

    ## `brave` is my new favorite browser
    options(browser = "/usr/bin/brave-browser")

    ## managing search path conflicts (from R-3.6.0)
    options(conflicts.policy = "strict")

})
