# color scheme

red_color <- "red"
blue_color <- "blue"

# background image for scout image plot
img <- png::readPNG("Traits/Craniometric_tracing.png")


# Possible choices for traits
traits <- c("ANS-Menton",
            "ANS-PNS",
            "Articulare-Gnathion",
            "Articulare-Gonion",
            "Articulare-Menton",
            "Articulare-Pogonion",
            "Basion-ANS",
            "Condylion-Gnathion",
            "Condylion-Gonion",
            "Condylion-Pogonion",
            "Condylion-Point A",
            "Gonion-Pogonion",
            "Nasion-ANS",
            "Nasion-Basion",
            "Nasion-Menton",
            "PNS-Point A",
            "Sella-ANS",
            "Sella-Articulare",
            "Sella-Basion",
            "Sella-Gonion",
            "Sella-Menton",
            "Sella-Nasion",
            "Sella-PNS",
            "Sella-Pogonion")


#' Convert "long" trait pair name to abbreviation
#'
#' @param x string to convert
#'
#' @return string converted to "trait1_trait2" format
#'
trait_to_abbrev <- function(x) {
  case_when(
    x == "ANS-Menton" ~ "ans_m",
    x == "ANS-PNS" ~ "ans_pns",
    x == "Articulare-Gnathion" ~ "art_gn",
    x == "Articulare-Gonion" ~ "art_go",
    x == "Articulare-Menton" ~ "art_m",
    x == "Articulare-Pogonion" ~ "art_pog",
    x == "Basion-ANS" ~ "b_ans",
    x == "Condylion-Gnathion" ~ "co_gn",
    x == "Condylion-Gonion" ~ "co_go",
    x == "Condylion-Pogonion" ~ "co_pog",
    x == "Condylion-Point A" ~ "co_pta",
    x == "Gonion-Pogonion" ~ "go_pog",
    x == "Nasion-ANS" ~ "n_ans",
    x == "Nasion-Basion" ~ "n_b",
    x == "Nasion-Menton" ~ "n_m",
    x == "PNS-Point A" ~ "pns-pta",
    x == "Sella-ANS" ~ "s_ans",
    x == "Sella-Basion" ~ "s_b",
    x == "Sella-Gonion" ~ "s_go",
    x == "Sella-Menton" ~ "s_m",
    x == "Sella-Nasion" ~ "s_n",
    x == "Sella-PNS" ~ "s_pns",
    x == "Sella-Pogonion" ~ "s_pog"
  )
}


# Percentiles to use for plot
percentiles_to_plot <- c("1%", "5%", "10%", "25%", "50%",
                         "75%", "90%", "95%", "99%")


# Coordinates for landmark points for scout image 
coords <- tibble::tribble(
  ~ LM, ~ x, ~ y,
  "ANS", 318, 249,
  "Articulare", 44, 193,
  "Basion", 7, 221,
  "Condylion", 59, 169,
  "Gnathion", 249, 421,
  "Gonion", 73.5, 332,
  "Menton", 215, 415,
  "Nasion", 313, 96,
  "PNS", 153, 241,
  "Pogonion", 239, 424,
  "Point A", 303, 263,
  "Sella", 79, 116
) |> 
  mutate(y = dim(img)[1] - y)


#' Plot percentiles
#' 
#' Plot percentiles for a trait pair. This function calls add_percentiles()
#' to actually add the percentile lines to the plot
#'
#' @param q tibble containing the percentiles
#' @param trait_string string containing the trait pair (not abbreviated)
#'
#' @return a ggplot object
#'
plot_percentile <- function(q, trait_string) {
  ggplot() +
    labs(x = "Age (y)", y = "Distance (mm)",
         title = trait_string) +
    add_percentiles(q) +
    scale_x_continuous(n.breaks = 21) +
    theme_bw(base_size = 12)
}


#' Add percentile lines to a plot
#' 
#' This function programmatically adds percentile lines to a ggplot object
#'
#' @param pcts tibble containing the set of percentiles for an age 
#' @param ... Additional options passed to plotting functions
#'
#' @return list containing the lines to be added to the plot
#'
add_percentiles <- function(pcts, ...) {
  # Extract the y position for the last row, needed to align the labels below
  last_row <- pcts[nrow(pcts), ] |> 
    pivot_longer(cols = -age)
  
  # Pivot to long form for plotting
  pcts_long <- pcts |> pivot_longer(cols = -age)
  
  return(list(
    geom_line(data = pcts_long, aes(x = age, y = value, group = name),
              linewidth = 0.5, color = blue_color),
    geom_label(data = last_row, aes(x = age,
                                    y = value,
                                    label = name),
               fill = "white",
               label.size = 0,
               label.padding = unit(0, "lines"),
               label.r = unit(0, "lines"), ...))
  )
}


#' Convert years-months to decimal year
#'
#' @param yrs numeric number of years
#' @param mos numeric number of months
#'
#' @return numeric age in decimal years
#'
yrmo_to_years <- function(yrs, mos) {
  return(as.numeric(yrs) + months(as.numeric(mos)) / years(1))
}


#' Split trait on "-"
#' 
#' This function splits the trait selected from the dropdown menu into
#' its two component landmarks. Needed for plotting the scout image
#'
#' @param trait string trait from reactive dropdown
#'
#' @return vector of string for the two traits
#'
split_trait <- function(trait) {
  return(str_split(trait, "-", simplify = TRUE) |> as.vector())
}


#' Plot the scout image schematic image for a pair of traits
#'
#' @param trait string of traits separated by "-" 
#'
#' @return ggplot object
#'
plot_schematic <- function(trait) {
  
  tt <- split_trait(trait)
  c_sub <- coords  |> 
    dplyr::filter(LM %in% tt)
  
  p <- ggplot(data = c_sub, aes(x, y, label = LM)) +
    annotation_raster(img, xmin = 0, xmax = dim(img)[2],
                      ymin = 0, ymax = dim(img)[1]) +
    geom_point(color = red_color, size = 3) +
    geom_line(color = red_color, linewidth = 1) +
    geom_label_repel(box.padding = 0.5, color = red_color) +
    xlim(c(0, dim(img)[2])) +
    ylim(c(0, dim(img)[1])) +
    coord_equal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) +
    ggpubr::theme_transparent()
  
  return(p)
}
