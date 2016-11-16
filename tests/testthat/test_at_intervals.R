context("at_intevals: error catching")

test_that("error raised if x is not a data.frame", {
    x <- list(ts = 2, x = 1)
    expect_error(at_intervals(x,  ts = "x"), "is.data.frame\\(x\\) is not TRUE")
})

test_that("error raised if there is not a POSIX or Date", {

    x <- tibble::tribble(
        ~ts,                  ~x,
        # -------------------\---
        "2016-01-01 00:00:00", 1
    )

    expect_error(at_intervals(x,  ts = 2),
                 "cannot find a column of type POSIX or Date in")
})

test_that("error raised if there are multiple POSIX or Date", {

    x <- tibble::tribble(
        ~ts_1,                ~ts_2,      ~x, ~y,
        # -------------------\-----------\---\---\
        "2016-01-01 00:00:00", "2016-01-01", 1, 1,
        "2016-01-01 00:05:00", "2016-01-01", 2, 2,
        "2016-01-01 00:10:00", "2016-01-01", 3, 3
    ) %>%
        dplyr::mutate(ts_1 = lubridate::ymd_hms(ts_1)) %>%
        dplyr::mutate(ts_2 = lubridate::ymd(ts_2))

    expect_error(at_intervals(x,  ts = 2),
                 "more than one column of type POSIX or Date in")
})

context("at_intevals: correct behaviour")

test_that("correct with date-times, no NAs, using by", {

    x <- tibble::tribble(
        ~ts,           ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01 00:00:00", 1, 1, 1,
        "2016-01-01 00:05:00", 2, 2, 2,
        "2016-01-01 00:10:00", 3, 3, 3
    ) %>% dplyr::mutate(ts = lubridate::ymd_hms(ts))

    actual <- at_intervals(x, by = "2 min")

    expected <- tibble::tribble(
        ~ts,                  ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01 00:00:00", 1, 1, 1,
        "2016-01-01 00:02:00", 1, 1, 1,
        "2016-01-01 00:04:00", 1, 1, 1,
        "2016-01-01 00:06:00", 2, 2, 2,
        "2016-01-01 00:08:00", 2, 2, 2,
        "2016-01-01 00:10:00", 3, 3, 3
    ) %>% dplyr::mutate(ts = lubridate::ymd_hms(ts))

    expect_equal(actual, expected)

})

test_that("correct with date, no NAs, using by", {

    x <- tibble::tribble(
        ~ts,           ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01", 1, 1, 1,
        "2016-01-05", 2, 2, 2,
        "2016-01-08", 3, 3, 3,
        "2016-01-11", 4, 4, 4
    ) %>% dplyr::mutate(ts = lubridate::ymd(ts))

    actual <- at_intervals(x, by = "2 day")

    expected <- tibble::tribble(
        ~ts,                  ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01", 1, 1, 1,
        "2016-01-03", 1, 1, 1,
        "2016-01-05", 2, 2, 2,
        "2016-01-07", 2, 2, 2,
        "2016-01-09", 3, 3, 3,
        "2016-01-11", 4, 4, 4
    ) %>% dplyr::mutate(ts = lubridate::ymd(ts))

    expect_equal(actual, expected)

})

test_that("correct with date, no NAs, using length.out", {

    x <- tibble::tribble(
        ~ts,           ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01", 1, 1, 1,
        "2016-01-05", 2, 2, 2,
        "2016-01-08", 3, 3, 3,
        "2016-01-11", 4, 4, 4
    ) %>% dplyr::mutate(ts = lubridate::ymd(ts))

    actual <- at_intervals(x, length.out = 6)

    expected <- tibble::tribble(
        ~ts,                  ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01", 1, 1, 1,
        "2016-01-03", 1, 1, 1,
        "2016-01-05", 2, 2, 2,
        "2016-01-07", 2, 2, 2,
        "2016-01-09", 3, 3, 3,
        "2016-01-11", 4, 4, 4
    ) %>% dplyr::mutate(ts = lubridate::ymd(ts))

    expect_equal(actual, expected)

})

test_that("correct with date-times and NAs, na.rm = TRUE (default)", {

    x <- tibble::tribble(
        ~ts,           ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01 00:00:00", NA, 1,  1,
        "2016-01-01 00:05:00", 2,  NA, NA,
        "2016-01-01 00:10:00", 3,  3,  NA
    ) %>% dplyr::mutate(ts = lubridate::ymd_hms(ts))

    actual <- at_intervals(x, by = "2 min")

    expected <- tibble::tribble(
        ~ts,                  ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01 00:00:00", NA, 1, 1,
        "2016-01-01 00:02:00", NA, 1, 1,
        "2016-01-01 00:04:00", NA, 1, 1,
        "2016-01-01 00:06:00", 2,  1, 1,
        "2016-01-01 00:08:00", 2,  1, 1,
        "2016-01-01 00:10:00", 3,  3, 1
    ) %>% dplyr::mutate(ts = lubridate::ymd_hms(ts))

    expect_equal(actual, expected)

})

test_that("correct with date-times and NAs, na.rm = FALSE", {

    x <- tibble::tribble(
        ~ts,           ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01 00:00:00", NA, 1,  1,
        "2016-01-01 00:05:00", 2,  NA, NA,
        "2016-01-01 00:10:00", 3,  3,  NA
    ) %>% dplyr::mutate(ts = lubridate::ymd_hms(ts))

    actual <- at_intervals(x, by = "2 min", na.rm = FALSE)

    expected <- tibble::tribble(
        ~ts,                  ~x, ~y, ~z,
        # -------------------\---\----\----\
        "2016-01-01 00:00:00", NA, 1,  1,
        "2016-01-01 00:02:00", NA, 1,  1,
        "2016-01-01 00:04:00", NA, 1,  1,
        "2016-01-01 00:06:00", 2,  NA, NA,
        "2016-01-01 00:08:00", 2,  NA, NA,
        "2016-01-01 00:10:00", 3,  3,  NA
    ) %>% dplyr::mutate(ts = lubridate::ymd_hms(ts))

    expect_equal(actual, expected)

})
