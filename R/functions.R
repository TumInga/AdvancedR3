#' Building descriptive stats for metabolite
#'
#' @param data A lipodomics dataset
#'
#' @returns A data.frame/tibble

create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean,median=median, sd = sd, iqr=IQR))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) sprintf("%.1f", round(x, digits = 1)))) |>
    dplyr::mutate(MeanSD = glue::glue("{value_mean} ({value_sd})")) |>
    dplyr::mutate(MeanIQR = glue::glue("{value_median} ({value_iqr})")) |>
    dplyr::select(Metabolite = metabolite, "Mean SD" = MeanSD,"Mean IQR" = MeanIQR)
}

#' Plotting the distribution of metabolite
#'
#' @param data lipidomics dataset
#'
#' @returns a figure

create_plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free") + # it is variable and the scale is free (for each histogram)
    ggplot2::ggtitle('Histogram of metabolic distribution') +
    ggplot2::labs(
      x = "Metabolite Value",
      y = "Count"
    ) +
    ggplot2::theme_minimal()
}


#' Do some cleaning to fix issue later
#'
#' @param data lipidomics data set
#'
#' @returns A data.frame

clean <- function(data) {
  data |>
    dplyr::group_by(dplyr::pick(-value)) |>
    dplyr::summarise(value = mean(value), .groups = "keep") |>
    dplyr::ungroup()
}
