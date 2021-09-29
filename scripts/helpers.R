#' Create Dockerfile from
#'
#' @param path defaults to getwd()
#' @param base_image base image for `FROM` in Dockerfile
#' @param app_config app config, defaults to `default`
#' @param maintainer maintainer - defaults to `fullname` from [whoami::whoami()]
#' @param date date - defaults to [base::Sys.Date()]
#' @param packages R package dependencies returned from [get_package_deps()]
#' @param sysreqs logical - should sysreqs be included in image (via [get_sysreqs()])
#' @param additional_r_commands any additional r commands to include in image
#'
#' @return
#' @export
#'
#' @importFrom usethis use_template
create_dockerfile <- function(path = getwd(),
                              base_image = "rocker/r-ver:latest",
                              app_config = "default",
                              mainainer = whoami::whoami()["fullname"],
                              date = Sys.Date(),
                              additional_r_commands = NULL) {

  pkgdeps <- get_package_deps(path)
  packages <- pkgdeps$package

  sysreqs <- get_sysreqs(packages)

  system_deps_string <- paste(paste0("  ", sysreqs), collapse = " \\ \n")

  sysreqs_out <- paste0(
    "RUN apt-get update && apt-get install -y \\ \n",
    system_deps_string
  )

  usethis::use_template(
    "Dockerfile",
    save_as = as.character(fs::path_rel(path = fs::path(path, "Dockerfile"))),
    data = list(base_image = base_image,
                app_config = app_config,
                maintianer = maintainer,
                # date = as.character(date),
                sysreqs = sysreqs_out,
                additional_r_commands = additional_r_commands),
    package = "jimstemplates"
  )

  out <- list(
    package_deps = pkgdeps,
    sysreqs = sysreqs
  )

  return(invisible(out))
}

#' Create .dockerignore from template
#'
#' @param open logical - open file?
#'
#' @export
#'
#' @importFrom usethis use_template
create_dockerignore <- function(open = TRUE) {

  usethis::use_template(
    "dockerignore",
    save_as = ".dockerignore",
    ignore = FALSE,
    open = open,
    package = "jimstemplates"
  )

}

#' Get R Package Dependencies
#'
#' This function takes a path to a directory and parses the code from all
#' `.R` and `.Rmd` files, retrieving any detected package dependencies, and
#' optionally outputs a `deps.yaml` and `deps.R` file.
#'
#' @param path path to directory
#' @param write_yaml logical - should `deps.yaml` be created?
#' @param write_r logical - should `deps.R` be created?
#' @param include_versions logical - should package versions and github referenced be included?
#'
#' @return silently returns a data.frame with R package details
#' @export
#'
#' @importFrom cli cli_alert_warning cat_bullet
#' @importFrom dplyr bind_rows mutate select
#' @importFrom purrr safely map_depth pluck map flatten_chr compact
#' @importFrom rlang set_names
#' @importFrom yaml write_yaml
get_package_deps <- function(path = getwd(),
                             write_yaml = TRUE,
                             write_r = TRUE,
                             include_versions = TRUE) {

  # get package dependencies based off supplied directory
  # first detect any R scripts or RMD files
  files <- list.files(
    path = path,
    pattern = "^.*\\.R$|^.*\\.Rmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  # loop through files gathering packages using `parse_packages`
  pkg_names_init <- lapply(files, purrr::safely(parse_packages))
  pkg_names <- purrr::map_depth(pkg_names_init, 1, purrr::pluck, "result") %>%
    purrr::map(function(x) {
      if (length(x) == 0) return(NULL) else return(x)
    }) %>%
    purrr::flatten_chr() %>%
    unique()

  if (length(pkg_names) == 0) {
    cli::cli_alert_warning("warning: no packages found in specified directory")
    return(invisible(NULL))
  }

  hold <- lapply(pkg_names, purrr::safely(get_package_details)) %>%
    rlang::set_names(pkg_names)

  out <- purrr::map_depth(hold, 1, purrr::pluck, "result") %>%
    purrr::map(function(x) {
      if (length(x) == 0) return(NULL) else return(x)
    }) %>%
    purrr::compact()

  df <- dplyr::bind_rows(out) %>%
    dplyr::mutate(
      Repository = ifelse(is.na(Repository), "Github", Repository),
      install_cmd = ifelse(
        Repository == "CRAN",
        paste0("remotes::install_version(", shQuote(Package), ", version = ", shQuote(Version), ")"),
        paste0("remotes::install_github(", shQuote(paste0(GithubUsername, "/", Package)), ", ref = ", shQuote(GithubSHA1), ")")
      )
    )

  if (write_yaml) {
    yaml::write_yaml(out, fs::path(path, "deps.yaml"))
    cli::cat_bullet(
      "Created file `deps.yaml`.",
      bullet = "tick",
      bullet_col = "green"
    )
  }

  if (write_r) {
    txt <- paste0("options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/latest'))\ninstall.packages('remotes')\n",
                  paste(df$install_cmd, collapse = "\n"))
    cat(txt, file = fs::path(path, "deps.R"))
    cli::cat_bullet(
      "Created file `deps.R`.",
      bullet = "tick",
      bullet_col = "green"
    )
  }

  out_df <- df %>% dplyr::select(package = Package, src = Repository, version = Version, install_cmd)

  return(invisible(out_df))

}

#' @keywords internal
#' @noRd
get_package_details <- function(pkg_name) {
  pkg_d <- packageDescription(pkg_name)
  is.cran <- !is.null(pkg_d$Repository) && pkg_d$Repository ==
    "CRAN"
  is.github <- !is.null(pkg_d$GithubRepo)
  is.base <- !is.null(pkg_d$Priority) && pkg_d$Priority ==
    "base"
  if (!is.cran & !is.github & !is.base)
    stop("CRAN or GitHub info for ", pkg_name, " not found. Other packages repos are not supported.",
         call. = FALSE)
  if (is.cran)
    return(pkg_d[c("Package", "Repository", "Version")])
  if (is.github)
    return(pkg_d[c("Package", "GithubUsername",
                   "GithubRepo", "GithubRef", "GithubSHA1")])
}

#' @keywords internal
#' @noRd
#' @importFrom purrr map
parse_packages <- function(fl) {

  lns <- get_lines(fl)

  rgxs <- list(
    library = "(?<=(library\\()|(library\\([\"']{1}))[[:alnum:]|.]+",
    require = "(?<=(require\\()|(require\\([\"']{1}))[[:alnum:]|.]+",
    colon = "[[:alnum:]|.]*(?=:{2,3})"
  )

  found_pkgs <- purrr::map(rgxs, finder, lns = lns) %>% unlist() %>%
    unique()

  found_pkgs <- found_pkgs[!found_pkgs %in% c("", " ")]

  return(found_pkgs)

}

#' @keywords internal
#' @noRd
#' @importFrom formatR tidy_source
#' @importFrom knitr purl
get_lines <- function(file_name) {

  if (grepl(".Rmd", file_name, fixed = TRUE)) {
    tmp.file <- tempfile()
    knitr::purl(input = file_name,
                output = tmp.file,
                quiet = TRUE)
    file_name <- tmp.file
  }

  lns <- tryCatch(
    formatR::tidy_source(
      file_name,
      comment = FALSE,
      blank = FALSE,
      arrow = TRUE,
      brace.newline = TRUE,
      output = FALSE
    )$text.mask,
    error = function(e) {
      message(paste("Could not parse R code in:",
                    file_name))
      message("   Make sure you are specifying the right file name")
      message("   and check for syntax errors")
      stop("", call. = FALSE)
    }
  )
  if (is.null(lns)) {
    stop("No parsed text available", call. = FALSE)
  }

  return(lns)

}

#' @keywords internal
#' @noRd
finder <- function(rgx, lns) {
  regmatches(lns, gregexpr(rgx, lns, perl = TRUE)) %>% unlist()
}


#' Get system requirements by R package
#'
#' @param packages character vector of packages names
#' @param quiet boolean if TRUE the function is quiet
#' @param batch_n number of simultaneous packages to ask
#'
#' @return character vector of sysreqs
#' @export
#'
#' @import magrittr
#' @importFrom remotes package_deps
#' @example get_sysreqs("shiny_app")
get_sysreqs <- function(packages, quiet = TRUE, batch_n = 30) {

  all_deps <- sort(unique(c(packages, unlist(remotes::package_deps(packages)$package))))

  sp <- split(all_deps, ceiling(seq_along(all_deps)/batch_n))

  hold <- lapply(sp, function(.x) {
    get_batch_sysreqs(.x, quiet = quiet)
  }) %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    sort()

  setdiff(hold, sysreqs_in_base)

}

#' @keywords internal
#' @noRd
#' @importFrom fs file_temp file_delete
#' @importFrom jsonlite fromJSON
#' @importFrom utils download.file
get_batch_sysreqs <- function(all_deps, quiet = TRUE) {

  url <- sprintf("https://sysreqs.r-hub.io/pkg/%s/linux-x86_64-debian-gcc",
                 paste(all_deps, collapse = ","))

  path <- fs::file_temp()

  utils::download.file(url, path, mode = "wb", quiet = quiet)

  out <- jsonlite::fromJSON(path)

  fs::file_delete(path)

  unique(out[!is.na(out)])

}

sysreqs_in_base <- c("gdebi-core",
                     "git-core",
                     "libcurl4-gnutls-dev",
                     "wget")
