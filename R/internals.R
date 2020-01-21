## Ensure population inputs to flow_matrix are
## numeric and positive
assert_pop <- function(pop) {

    if (is.list(pop)) {

        pop <- unlist(pop)
    }

    if (! is.numeric(pop)) stop("population should be numeric")
    if (! all(is.finite(pop))) stop("population should be finite")
    if (any(pop < 0)) stop("population cannot be negative.")
    if (any(pop == 0)) warning("One of the populations is 0.")

}

## Distances should be strictly positive
assert_dist <- function(x) {

    if (is.list(x)) {

        x <- unlist(x)
    }

    if (! is.numeric(x)) stop("distances should be numeric")
    if (! all(is.finite(x))) stop("distances should be finite")
    if (any(x <= 0)) stop("distances cannot be negative.")


}

## Ensure models are invoked with appropriate args.
assert_params <- function(params, model = "gravity") {

    ## gravity model needs K, pow_from, pow_to,
    ## pow_dist
    if (model == "gravity") {

        needed <- c(
            "k", "pow_from", "pow_to", "pow_dist"
        )

        if (!all(names(params) %in% needed)) {

            msg <- "params should be a named list with names"
            msg <- paste(msg, needed)
            msg <- paste(msg, ". Found", names(params))
        }

        if (! all(sapply(params, is.numeric)))
            stop("all parameters should be numeric")

        if (! all(sapply(params, is.finite)))
            stop("all parameters should be finite")


    }
}
