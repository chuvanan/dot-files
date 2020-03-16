


local({

    ## enable autocompletions for package names in `require()`, `library()`
    utils::rc.settings(ipck = TRUE)

    ## enable detection of functions.
    utils::rc.settings(func = TRUE)

    utils::rc.options(funarg.suffix = " = ")

})
