##' Given the populations of A and B and the distance between them,
##' return the estimated population flow between
##' them modeled as
##' \deqn{\phi(A,B) = K n_A^{\alpha}n_B^{\beta}/r_{AB}^{\gamma}}
##' @title Computes the flow from A to B under the gravity model
##' @param n_from population of the source
##' @param n_to population of the destination
##' @param distance distance between the two places
##' @param params a named list of gravity model parameters. The
##' parameters are k, pow_from, pow_to, pow_dist.
##' destination
##' @return estimated flow between source and destination
##' @author Sangeeta Bhatia
##' @export
gravity_model <- function(n_from, n_to, distance, params) {

    params$k * (n_from ^ params$pow_from) * (n_to ^ params$pow_to) /
        (distance ^ params$pow_dist)

}

##' Vector of population flow between locatuons
##'
##' This function returns a vector of population flows under the
##' specified movement model with the given set of parameters.
##'
##' @param pop_from Numeric (optionally named) list of populations. If
##' the input is not named, integer names are assigned.
##' @param pop_to  Numeric (optionally named) vector of populations
##' @param distances Numeric vector of distances. Element i is the
##' distance between location with pop_from[i] and pop_to[i].
##' Therefore the lengths of the two population vectors and the distance
##' vector should be the same
##' @param params parameters for the model. This should be a named
##' list with the names matching those required by the model. For
##' gravity model, these are: k, pow_from, pow_to, pow_dist.
##' \deqn{\phi(i, j) = k * n_i^pow_from * n_j^power_to / dist_{i, j}^pow_dist}
##' @param model Currently only gravity model is supported.
##' @return a named list where the ith element is the flow from
##' (\code{pop_from})[i] to (\code{pop_to})[i].
##' is the flow between pop_from[i] and pop_to[j].
##' @author Sangeeta Bhatia
##' @export
flow_vector <- function(pop_from, pop_to, distances, params, model = "gravity") {

    model <- match.arg(model)

    ## Sanity chcecks on inputs
    stopifnot(length(pop_from) == length(pop_to))
    stopifnot(length(pop_from) == length(distances))

    assert_pop(pop_from)
    assert_pop(pop_to)
    assert_dist(distances)
    assert_params(params, model)

    if (model == "gravity")
      out <- gravity_model(pop_from, pop_to, distances, params)

    from_names <- names(pop_from)
    to_names <- names(pop_to)

    if (is.null(from_names)) from_names <- seq_along(pop_from)
    if (is.null(to_names)) to_names <- seq_along(pop_to)

    names(out) <- paste0(from_names, "_", to_names)


    out


}
