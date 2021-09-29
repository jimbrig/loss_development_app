#' Derive Triangles
#'
#' @param loss_dat loss data
#' @param type paid, reported, case, or n_claims
#' @param limit optinal limit
#'
#' @return list of triangle data
#'
#' @importFrom devtri dev_tri spread_tri ata_tri idf ldf_avg ldf_avg_wtd idf2cdf cdf
#' @importFrom dplyr rename filter mutate
#' @importFrom purrr map2_dfr
#' @importFrom rlang set_names
#' @importFrom tidyr pivot_wider
derive_triangles <- function(loss_dat, type = c("paid", "reported", "case", "n_claims"), limit = NULL) {

  agg_dat <- loss_dat %>% aggregate_loss_data(limit = limit)

  tri_dat <- devtri::dev_tri(
    origin = agg_dat$accident_year,
    age = agg_dat$devt,
    value = agg_dat[[type]]
  )

  tri <- tri_dat %>%
    devtri::spread_tri() %>%
    dplyr::rename(AYE = origin)

  if (type == "case") {
    return(
      list(
        "aggregate_data" = agg_dat,
        "triangle_data" = tri_dat,
        "triangle" = tri
      )
    )
  }

  ata_dat <- tri_dat %>%
    devtri::ata_tri(loss_dat) %>%
    dplyr::filter(!is.na(value))

  ata_tri <- ata_dat %>%
    devtri::spread_tri() %>%
    dplyr::rename(AYE = origin) %>%
    dplyr::mutate(AYE = as.character(AYE))

  # ata_tri <- triangle_data[[input$type]]$age_to_age_triangle %>%
  #   mutate(AYE = as.character(AYE))

  ldf_avg <- devtri::idf(devtri::ldf_avg(tri_dat)$idfs)

  ldf_avg_wtd <- devtri::idf(devtri::ldf_avg_wtd(tri_dat)$idfs)

  sel <- ldf_avg_wtd

  cdf <- devtri::idf2cdf(sel)

  params <- list("Straight Average:" = ldf_avg,
                 "Weighted Average:" = ldf_avg_wtd,
                 "Selected:" = sel,
                 "CDF:" = cdf)

  hold <- purrr::map2_dfr(params, names(params), function(dat, type_ ) {
    dat %>%
      tidyr::pivot_wider(names_from = age, values_from = names(dat)[2]) %>%
      rlang::set_names(names(ata_tri)) %>%
      dplyr::mutate(AYE = type_)
  })

  list(
    "aggregate_data" = agg_dat,
    "triangle_data" = tri_dat,
    "triangle" = tri,
    "age_to_age_data" = ata_dat,
    "age_to_age_triangle" = ata_tri,
    "averages" = hold
  )

}



#' Aggregate Loss data
#'
#' @param claim_dat claims data
#' @param limit optional limit
#'
#' @return df
#'
#' @importFrom dplyr mutate group_by summarise n ungroup
aggregate_loss_data <- function(claim_dat, limit = NA) {

  if (!is.na(limit)) {
    claim_dat <- claim_dat %>%
      dplyr::mutate(
        paid = pmin(limit, .data$paid),
        reported = pmin(limit, .data$reported),
        case = reported - paid
      )
  }

  claim_dat %>%
    dplyr::group_by(accident_year, devt) %>%
    dplyr::summarise(
      paid = sum(.data$paid, na.rm = TRUE),
      reported = sum(.data$reported, na.rm = TRUE),
      n_claims = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(case = reported - paid)
}
