context("movement models")

test_that("Gravity model vector output is OK", {

    params <- list(k = 1, pow_from = 1, pow_to = 1, pow_dist = 2)
    pop_from <- c(a = 1000, b = 4000)
    pop_to <- c(b = 2000, a = 1000)
    distances <- c(100, 200)
    correct <- c(a_b = 200, b_a = 100)
    out <- flow_vector(pop_from, pop_to, distances, params)

    expect_true(all(out == correct))

   }
 )


test_that("Gravity model flow matrix works", {

    params <- list(k = 1, pow_from = 1, pow_to = 1, pow_dist = 1)
    pops <- c(a = 1000, b = 2000)

    ## lat  <- c(12.5, 34.5)
    ## long <- c(-70, 69)
    ## distances <- geosphere::distm(matrix(c(long, lat), nrow = 2))

    distances <- matrix(
        c(0, 13247174, 13247174, 0), nrow = 2, byrow = FALSE
    )

    correct <- matrix(
        c(NA, 0.15, 0.15, NA), nrow = 2, byrow = FALSE
    )

    out <- flow_matrix(pops, distances, params)
    out <- round(out, 2)

    expect_true(all(out == correct, na.rm = TRUE))

    ## Asymmetric powers
    params <- list(k = 1, pow_from = 1, pow_to = 2, pow_dist = 1)
    correct <- matrix(
        c(NA, 150.98, 301.95, NA), nrow = 2, byrow = FALSE
    )

    out <- flow_matrix(pops, distances, params, symmetric = FALSE)
    out <- round(out, 2)

    expect_true(all(out == correct, na.rm = TRUE))


   }
 )
