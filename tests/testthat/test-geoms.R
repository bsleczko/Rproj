library(rcap)
context("Testing that the geoms run and return the correct objects")

test_that("geom_timeline runs correctly", {
  noaa_df <- read.delim(file = "inst/extdata/signif.txt", stringsAsFactors = FALSE) %>%
    eq_location_clean() %>%
    dplyr::mutate(DATE = paste(YEAR, NAto01(MONTH), NAto01(DAY), sep = "-")) %>%
    dplyr::filter(COUNTRY == "MEXICO" | COUNTRY == "GUATEMALA") %>%
    dplyr::filter(lubridate::year(DATE) >= 2000)


  plt <- ggplot2::ggplot(noaa_df, aes(x = as.Date(DATE), y = COUNTRY,
                      color = as.numeric(TOTAL_DEATHS),
                      size = as.numeric(EQ_PRIMARY))) +
    geom_timeline() +
    labs(size = "Richter scale", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "right",
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::xlab("DATE")

  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})

test_that("geom_timeline_label runs correctly", {
  noaa_df <- read.delim(file = "inst/extdata/signif.txt", stringsAsFactors = FALSE) %>%
    eq_location_clean() %>%
    dplyr::mutate(DATE = paste(YEAR, NAto01(MONTH), NAto01(DAY), sep = "-")) %>%
    dplyr::filter(COUNTRY == "MEXICO" | COUNTRY == "GUATEMALA") %>%
    dplyr::filter(lubridate::year(DATE) >= 2000)


  plt <- ggplot2::ggplot(noaa_df, aes(x = as.Date(DATE), y = COUNTRY,
                                      color = as.numeric(TOTAL_DEATHS),
                                      size = as.numeric(EQ_PRIMARY))) +
    geom_timeline_label(data = noaa_df) +
    labs(size = "Richter scale", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "right",
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::xlab("DATE")


  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})
