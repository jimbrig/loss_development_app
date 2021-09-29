#' loss_run
#'
#' view losses as of a specific date
#'
#' @param val_date date the valuation date of the loss run.  Claim values from `trans`
#' will be values as of the `val_date`
#' @param trans_ data frame of claims transactions
#'
#' @return data frame of claims (1 claim per row) valued as of the `val_date`
#'
#' @importFrom dplyr filter group_by top_n ungroup mutate arrange desc select everything
#' @importFrom lubridate ymd year
loss_run <- function(val_date, trans_) {
  out <- trans_ %>%
    dplyr::filter(transaction_date <= val_date) %>%
    dplyr::group_by(claim_num) %>%
    dplyr::top_n(1, wt = trans_num) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(reported = paid + case,
                  eval_date = lubridate::ymd(as.character(val_date)),
                  case = reported - paid,
                  accident_year = lubridate::year(.data$accident_date),
                  report_year = lubridate::year(.data$report_date),
                  eval_year = lubridate::year(.data$eval_date),
                  ay_start = lubridate::ymd(paste0(as.character(lubridate::year(accident_date)), "-01-01")),
                  ay_end = lubridate::ymd(paste0(as.character(lubridate::year(accident_date)), "-12-31")),
                  ay_avg = lubridate::ymd(paste0(as.character(lubridate::year(accident_date)), "-07-01")),
                  devt_in_days = as.numeric(eval_date - ay_avg),
                  devt = round(devt_in_days / 365.25 * 12, 0) + 6) %>%
    dplyr::arrange(dplyr::desc(transaction_date), claim_num, dplyr::desc(eval_date)) %>%
    dplyr::select(eval_date, dplyr::everything())
}

#' loss_run_all_evals
#'
#' wrapper around loss_run
#'
#' @param trans_ transactional claims data
#' @param devt_months optinal character vector (numeric) of months to inlcude
#' @param merge logical - merge into data.frame or keep as list?
#'
#' @return either a list or merged df
#'
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom lubridate month
#' @importFrom purrr map
#' @importFrom rlang set_names
loss_run_all_evals <- function(trans_, devt_months = NULL, merge = FALSE) {
  trans_dates <- pull_unique(trans_, "transaction_date")

  evals <- end_of_month(trans_dates) %>% unique() %>% sort()
  if (!is.null(devt_months)) evals <- evals[lubridate::month(evals) %in% devt_months]

  hold <- purrr::map(evals, loss_run, trans_ = trans_) %>% rlang::set_names(evals)

  if (!merge) return(hold)

  hold %>% dplyr::bind_rows() %>% dplyr::arrange(claim_num, dplyr::desc(eval_date))
}
