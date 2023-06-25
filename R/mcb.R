as.data.frame.nemenyi <- function(x, ...){
  as_tibble(x$means, rownames = "name") %>%
    mutate(cd = x$cd,
           l = value - cd/2,
           u = value + cd/2,
    ) %>%
    arrange(value) %>%
    mutate(
      name = paste0(
        name,
        " - ",
        format(round(value, 2), width = 5, nsmall = 2))) %>%
    mutate(col =
             (u[[grep("Benchmark ETS", name)]] <=u &
                u[[grep("Benchmark ETS", name)]] >=l) |
             (u[[grep("Benchmark ETS", name)]] >=u &
                l[[grep("Benchmark ETS", name)]] <=u)
    ) %>%
    mutate(fpval = x$fpval,
           fH = x$fH)
}
