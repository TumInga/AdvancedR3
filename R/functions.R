#' Building descriptive stats for metabolite
#'
#' @param data A data.frame/tibble
#'
#' @returns A data.frame/tibble

create_table_descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) sprintf("%.1f", round(x, digits = 1)))) %>%
    dplyr::mutate(MeanSD = glue::glue("{value_mean} ({value_sd})")) %>%
    dplyr::select(Metabolite = metabolite, "Mean SD" = MeanSD)
}
