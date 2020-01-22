##' Given the populations of A and B and the distance between them,
##' return the estimated population flow between
##' them modeled as
##' \deqn{\phi(A,B) = K n_A^{\alpha}n_B^{\beta}/r_{AB}^{\gamma}}
##' @title Computes the flow from A to B under the gravity model
##' @param n_from population of the source
##' @param n_to population of the destination
##' @param distance distance between the two places
##' @param params a named list of model parameters. For gravity model
##' the parameters are k, pow_from, pow_to, pow_dist.
##' destination
##' @return estimated flow between source and destination
##' @author Sangeeta Bhatia
##' @export
gravity_model <- function(n_from, n_to, distance, params) {

    params$k * (n_from ^ params$pow_from) * (n_to ^ params$pow_to) /
        (distance ^ params$pow_dist)

}

##' Vector of population flow between locations
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


##' Population flow matrix
##' @details flow from a location to itself is undefined (NA) under
##' gravity model.
##' @param populations vector of populations.
##' @param distances  distance matrix. This is assumed to be symmetric
##' so only lower triangle is of use.
##' @param params a named list of model parameters. For gravity model
##' the parameters are k, pow_from, pow_to, pow_dist.
##'
##' @param model at the moment only gravity model is supported.
##'
##' @param place_names optional. If not NULL, rownames of the returned
##' matrix will be set.
##' @param symmetric is the flow matrix symmetric. defaults to TRUE.
##'
##' @return N X N flow matrix where N is the length of the populations
##' vector. i,jth element in this matrix is the flow from location i
##' to location j. The diagonal is NA.
##'
##' @author Sangeeta Bhatia
flow_matrix <- function(populations,
                        distances,
                        params,
                        model = "gravity",
                        place_names = NULL,
                        symmetric = TRUE) {


    assert_pop(populations)
    assert_params(params, model)

    if ((! is.matrix(distances)) ||
        (nrow(distances) != length(populations))) {
        msg <- paste(
            "distances should be a ",
            length(populations),
            " X ",length(populations),
            "matrix."
        )
        stop(msg)
     }


    out <- matrix(
        NA, nrow = length(populations), ncol = length(populations)
    )

    ## out[i, j] = flow from i to j.
    ## Fill the lower triangle by columns and the upper triangle by
    ## rows.
    for (to in seq_along(populations)) {

        ## flow from a place to itself is undefined
        ## at least under gravtiy model. So we grab everything except
        ## the diagonal element.
        ## For other models, this needs
        ## to be checked.

        n_from <- populations[-to]
        n_to <- rep(populations[to], length(n_from))
        distance <-  distances[-to, to]
        flow <- flow_vector(
            pop_from = n_from, pop_to = n_to, distance, params, model
        )
        if (to == 1) flow <- c(NA, flow)
        else flow <- append(flow, NA, to - 1)
        out[ , to] <- flow

        if (symmetric) {

            out[to, ] <- flow

        } else {

            flow <- flow_vector(
                pop_from = n_to, pop_to = n_from, distance, params, model
            )

            if (to == 1) flow <- c(NA, flow)
            else flow <- append(flow, NA, to - 1)

            out[to, ] <- flow
        }
    }

    if (! is.null(place_names)) {
        if (length(place_names) != nrow(out)) {
            warning(
                "Length of names provided not same as row count of matrix.
                 rownames not set."
            )
        } else {
            rownames(out) <- place_names
            colnames(out) <- place_names
        }
    }

    out
}



