library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(grid)
library(leaflet)


#' @title NAto01
#' @import stringr
#' @export
#' @example \dontrun{NAto01(noaa_df$MONTH)}
NAto01 <- function(vector){
  sapply(vector, function(x){
    if(is.na(x)) return("01")
    else return(as.character(str_pad(x, width=2, side="left", pad="0")))
    })
}

#' @title eq_location_clean
#' @import stringr
#' @export
#' @example \dontrun{eq_location_clean(noaa_df)}
eq_location_clean <- function(df){
  df <- df %>%
    dplyr::mutate(LOCATION_NAME = stringr::str_match(LOCATION_NAME, "(?<=:  )([:graph:]|[:blank:])+")[,1]) %>%
    dplyr::mutate(LOCATION_NAME = str_to_title(LOCATION_NAME))
  return(df)
}

#' @title eq_map
#' @import leaflet
#' @export
#' @example \dontrun{eq_map(noaa_df, "DATE")}
eq_map <- function(df, annot_col){
  lmap <- df %>% leaflet::leaflet() %>% leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng=df$LONGITUDE,
                              lat=df$LATITUDE,
                              radius=df$EQ_PRIMARY,
                              popup=df[[annot_col]],
                              color="red",
                              weight=1,
                              opacity=0.5)
}

#' @title eq_create_label
#' @export
#' @example \dontrun{eq_create_label(noaa_df)}
eq_create_label <- function(df){
  len <- length(df$LOCATION_NAME)
  date <- df$DATE
  locations <- df$LOCATION_NAME
  magnitude <- df$EQ_PRIMARY
  deaths <- df$DEATHS

  ptxt <- rep("", len)
  for(i in 1:len){
    txt <- paste0("<b>Date: </b>", date[i], "</br>",
                  "<b>Location: </b>", locations[i], "</br>",
                  "<b>Magnitude: </b>", magnitude[i], "</br>",
                  "<b>Total Deaths: </b>", deaths[i])
    ptxt[i] <- txt
  }
  return(ptxt)
}


#'
#' @title eq_read_data
#' @export
#' @example \dontrun{eq_read_data()}
#'
eq_read_data <- function(){
  filepath = system.file("extdata", sprintf("signif.txt", year), package="Rproj")
  return(read.delim(file = filepath, stringsAsFactors = FALSE))
}

##########################################
#' @title GeomTimeline class definition
#'
#' @description ggproto() constructs a new geom class
#'
#' @import ggplot2
#' @import grid
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  No example, this is part of ggplot2 geom function defintion
#' }
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y = 1, shape = 21, colour = "grey", fill = "grey", size = 1, alpha = 0.8),
                                 draw_key = ggplot2::draw_key_polygon,
                                 draw_group = function(data, panel_params, coord){
                                   coords <- coord$transform(data, panel_params)
                                   points <- grid::pointsGrob(coords$x, coords$y,
                                                              pch = coords$shape,
                                                              size = grid::unit(coords$size / 5.5, "lines"),
                                                              gp = gpar(col = alpha(coords$colour, coords$alpha),
                                                                        fill = alpha(coords$colour, coords$alpha)
                                                              )
                                   )
                                   line <- grid::segmentsGrob(
                                     x0 = 0, y0 = coords$y,
                                     x1 = 1, y1 = coords$y,
                                     gp = gpar(col = "grey20", alpha = 0.8, size = 1)
                                   )
                                   grid::gList(line, points)
                                 }
)

#' @title geom_timeline function definition
#'
#' @description Using new geom class we can plot the timelines
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  ggplot(noaa_df, aes(x = DATE, y = COUNTRY,
#'         color = as.numeric(TOTAL_DEATHS),
#'         size = as.numeric(EQ_PRIMARY),
#'         label = LOCATION_NAME)) +
#'  geom_timeline()
#' }
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

##########################################
#' @title GeomTimelineLabel class definition
#'
#' @description ggproto() constructs a new geom class
#'
#' @import ggplot2
#' @import grid
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  No example, this is part of ggplot2 geom function defintion
#' }
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y = 1, alpha = 0.7, fill = "grey", colour = "grey"),
                                      draw_key = ggplot2::draw_key_label,
                                      draw_group = function(data, panel_scales, coord) {
                                        coords <- coord$transform(data, panel_scales)
                                        line <- grid::segmentsGrob(
                                          x0 = coords$x, y0 = coords$y,
                                          x1 = coords$x, y1 = coords$y + 0.05,
                                          gp = grid::gpar(col = "grey20", alpha = 0.7, size=1)
                                        )
                                        text <- grid::textGrob(
                                          label=coords$label,
                                          x = coords$x,
                                          y = coords$y + 0.05,
                                          rot = 45,
                                          just = c("left", "bottom")
                                        )
                                        grid::gList(line, text)
                                      }
)


#' @title geom_timeline_label function definition
#'
#' @description Using new geom class we can plot the timeline labels
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  ggplot(noaa_df, aes(x = DATE, y = COUNTRY,
#'         color = as.numeric(TOTAL_DEATHS),
#'         size = as.numeric(EQ_PRIMARY),
#'         label = LOCATION_NAME)) +
#'  geom_timeline_label()
#' }
geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, n_max = 5, ...) {

  data <- data %>% dplyr::mutate(COUNTRY = as.character(COUNTRY), EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
    dplyr::arrange(COUNTRY, desc(EQ_PRIMARY))

  countries <- unique(data$COUNTRY)
  df_all <- data.frame()
  for(country in countries){
    df <- data %>% dplyr::filter(COUNTRY == country) %>% head(n_max)
    df_all <- rbind(df_all, df)
  }
  data <- df_all

  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
##########################################


noaa_df <- eq_read_data() %>%
  eq_location_clean() %>%
  dplyr::mutate(DATE = paste(YEAR, NAto01(MONTH), NAto01(DAY), sep = "-")) %>%
  dplyr::filter(COUNTRY == "MEXICO" | COUNTRY == "GUATEMALA") %>%
  dplyr::filter(lubridate::year(DATE) >= 2000)

noaa_df %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text") %>%
  print()

ggplot(noaa_df, aes(x = as.Date(DATE), y = COUNTRY,
                    color = as.numeric(TOTAL_DEATHS),
                    size = as.numeric(EQ_PRIMARY))) +
  geom_timeline() +
  labs(size = "Richter scale", color = "# deaths") +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 legend.position = "right",
                 axis.title.y = ggplot2::element_blank()) +
  ggplot2::xlab("DATE")

ggplot(noaa_df, aes(x = as.Date(DATE), y = COUNTRY,
                    color = as.numeric(TOTAL_DEATHS),
                    size = as.numeric(EQ_PRIMARY),
                    label = LOCATION_NAME)) +
  geom_timeline() +
  labs(size = "Richter scale", color = "# deaths") +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 legend.position = "right",
                 axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
  geom_timeline_label(data = noaa_df)


