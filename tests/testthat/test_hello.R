context("saying hello")

test_that("correct greeting", {
    expect_output(hello(), "Hello, world!")
})
