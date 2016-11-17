#' Values of arbitrary time series at regular intervals.
#'
#' Time series are assumed to be structured as a data frame with one column
#' being time stamps and the remaining columns being features that are observed.
#' In general, features are observed irregularly and asynchronously.  The
#' purpose of this function is derive a regular sequenced, synchronised time
#' series.
#'
#' The function is designed to be used with the \link[dplyr]{dplyr} family.
#
#' @param x A data frame
#' @param ... Arguments passed to \code{seq} method
#' @param na.rm A logical scalar.  Defaults to TRUE.
#'
#' @return A data frame
#' @export
#'
#' @importFrom magrittr "%>%"
#' @import stats
#' @import stats
#' @import data.table
#'
#' @seealso \code{\link[base]{seq}}
#'
#'
#' @examples
#'  x <- tibble::tribble(
#'    ~ts,                   ~x, ~y, ~z,
#'  #|----------------------|---|---|---|
#'    "2016-01-01 00:00:00", NA, 1,  1,
#'    "2016-01-01 00:05:00", 2,  NA, NA,
#'    "2016-01-01 00:10:00", 3,  3,  NA
#'    )
#'
#'  x <- dplyr::mutate(x, ts = lubridate::ymd_hms(ts))
#'
#'  at_intervals(x, by = "2 min", na.rm = FALSE)
#'
at_intervals <- function(x, ..., na.rm = TRUE) {

    stopifnot(is.data.frame(x))

    # find the time stamp column -----------------------------------------------
    ts_ind <- purrr::map_lgl(x,
        ~ (lubridate::is.POSIXt(.) || lubridate::is.Date(.)))
    if (sum(ts_ind) == 1) {
        ts <- names(which(ts_ind))
        var_names <- names(which(!ts_ind))
    } else if (sum(ts_ind) < 1) {
        stop(paste("cannot find a column of type POSIX or Date in: ", x))
    } else {
        stop(paste("more than one column of type POSIX or Date in: ", x))
    }

    # convert to long data table to make it easy to handle NAs------------------
    x <- x %>%
        dplyr::rename_(.dots = setNames(ts, "time_stamp")) %>%
            tidyr::gather_("name", "value", var_names, na.rm = na.rm) %>%
            data.table::as.data.table() %>%
            data.table::setkey(name, time_stamp)

    # make a lookup table for rolling join -------------------------------------
    # sequence is determined by ... argument
    time_range <- range(x[["time_stamp"]])
    time_stamp <- seq(time_range[1], time_range[2], ...)
    name <- rep(var_names, each = length(time_stamp))
    lookup <- data.table::data.table(time_stamp, name) %>%
        data.table::setkey(name, time_stamp)

    # user data.table rolling join ---------------------------------------------
    x[lookup, roll = TRUE] %>%
        data.table::dcast(time_stamp~name, value.var = "value", fun = mean) %>%
        dplyr::as_data_frame() %>%
        dplyr::rename_(.dots = stats::setNames("time_stamp", ts))

}
