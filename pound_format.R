# quick script to use the pound sign in axis labels

pound_format <- function(largest_with_cents = 100000000) {
  function(x) {
    x <- round_any(x, 0.01)
    if (max(x, na.rm = TRUE) < largest_with_cents &
        !all(x == floor(x), na.rm = TRUE)) {
      nsmall <- 2L
    } else {
      x <- round_any(x, 1)
      nsmall <- 0L
    }
    str_c("£", format(x, nsmall = nsmall, trim = TRUE, big.mark = ",", scientific = FALSE, digits=1L))
  }
}
