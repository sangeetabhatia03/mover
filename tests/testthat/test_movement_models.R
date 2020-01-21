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
