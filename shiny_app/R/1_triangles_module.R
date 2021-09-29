#' Triangles Module - UI
#'
#' @param id Namespace ID
#'
#' @return html tagList
#' @export
#'
#' @importFrom DT DTOutput
#' @importFrom shiny NS fluidRow column radioButtons
#' @importFrom shinycustomloader withLoader
#' @importFrom shinydashboard box
#' @importFrom shinyjs hidden
triangles_module_ui <- function(id, loss_data) {

  ns <- shiny::NS(id)

  min_eval <- min(loss_data$eval_date, na.rm = TRUE)
  max_eval <- max(loss_data$eval_date, na.rm = TRUE)

  tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Metrics",
        class = "text-center",
        collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shiny::radioButtons(
              inputId = ns("type"),
              label = icon_text("calculator", "Loss Type"),
              choices = c(
                "Paid Loss" = "paid",
                "Reported Loss" = "reported",
                "Case Reserves" = "case",
                "Reported Claim Counts" = "n_claims"
              ),
              selected = "paid",
              inline = TRUE
            )
          ),
          shiny::column(
            width = 3,
            shiny::numericInput(
              inputId = ns("lmt"),
              label = icon_text("dollar-sign", "Per Claim Limit (000s)"),
              value = NA_real_,
              min = 0,
              step = 1
            ),
            shiny::helpText("Leave blank for Unlimited.")
          )
        )
      ),
      box(
        title = "Loss Development Triangle",
        width = 12,
        collapsible = TRUE,
        # h3(
        #   class = "text-center",
        shiny::htmlOutput(ns("tri_title")),
        # ),
        DT::DTOutput(ns("triangle")) %>%
          shinycssloaders::withSpinner(image = 'images/pwc_spinner_01.gif')
      ),
      div(
        id = ns("devt"),
        box(
          title = "Age to Age Triangle",
          collapsible = TRUE,
          width = 12,
          DT::DTOutput(ns("devt_factors")) %>%
            shinycssloaders::withSpinner(image = 'images/pwc_spinner_01.gif')
        )
      )
    )
  )
}

#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom devtri ldf_avg
#' @importFrom dplyr data_frame bind_rows mutate
#' @importFrom DT renderDT datatable
#' @importFrom shiny observeEvent reactive
#' @importFrom shinyjs show hide
#' @importFrom tibble tibble
#' @importFrom tidyr spread
triangles_module <- function(input, output, session, loss_data, selected_eval) {

  shiny::observeEvent(input$type, {
    if (input$type != "case") shinyjs::show("devt") else shinyjs::hide("devt")
  })

  output$tri_title <- shiny::renderUI({
    txt <- switch(input$type,
                  "paid" = "Paid Loss Development",
                  "reported" = "Reported Loss Development",
                  "case" = "Case Reserve Development",
                  "n_claims" = "Reported Claim Count Development")

    if (!is.na(input$lmt) && input$lmt != 0) {
      txt <- paste0(txt, " - Claims Limited to $", prettyNum(input$lmt * 1000, big.mark = ","))
    } else {
      txt <- paste0(txt, " - Unlimited Claims")
    }

    eval_txt <- paste0("Latest Evaluation Date of ", format(selected_eval(), "%B %d, %Y"))

    tagList(
      tags$h3(
        class = "text-center",
        txt
      ),
      tags$h4(
        class = "text-center",
        eval_txt
      )
    )

  })

  triangle_data <- reactive({

    # browser()

    lmt <- if (is.na(input$lmt)) NA else input$lmt * 1000
    type <- input$type

    agg_dat <- loss_data() %>%
      aggregate_loss_data(limit = lmt)

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
  })

  output$triangle <- DT::renderDT({
    out <- triangle_data()$triangle

    n_row <- nrow(out)
    col_width <- paste0(round(1/ncol(out),0) * 100, "%")

    hold <- DT::datatable(
      out,
      rownames = FALSE,
      caption = "Development Age in Months of Maturity",
      colnames = c("Accident Year", names(out)[-1]),
      extensions = c("Buttons"),
      selection = "none",
      class = "display",
      callback = DT::JS('return table'),
      options = list(
        dom = "Bt",
        paging = FALSE,
        scrollX = TRUE,
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = "dev-triangle"
          )
        ),
        ordering = FALSE,
        pageLength = n_row,
        columnDefs = list(
          list(targets = "_all", className = "dt-center", width = col_width)
        )
      )
    ) %>%
      DT::formatCurrency(
        column = 2:length(out),
        currency = "",
        digits = 0
      )
  })

  devt_prep <- shiny::reactive({
    req(input$type != "case")

    out <- triangle_data()$age_to_age_triangle %>%
      summaryrow::blank_row()

    out <- dplyr::bind_rows(
      out,
      triangle_data()$averages
    )

    tail_df <- tibble(
      "tail" = rep(NA, times = nrow(out))
    )

    cbind(
      out,
      tail_df
    )

  })

  output$devt_factors <- DT::renderDT({
    req(devt_prep())
    out <- devt_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    col_width <- paste0(round(1 / ncol(out), 0) * 100, "%")

    DT::datatable(
      out,
      rownames = FALSE,
      caption = "Age-to-Age Development Factors in Months of Maturity",
      colnames = c(
        "Accident Year",
        paste0(names(out)[-c(1, n_col - 1, n_col)], "-", names(out)[-c(1, 2, n_col)]),
        paste0(names(out)[n_col - 1], "-Ult"),
        ""
      ),
      extensions = c("Buttons"),
      selection = "none",
      class = "display",
      callback = DT::JS('return table'),
      options = list(
        dom = "Bt",
        paging = FALSE,
        ordering = FALSE,
        pageLength = n_row,
        scrollX = TRUE,
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = "ata-triangle"
          )
        ),
        columnDefs = list(
          list(targets = "_all", className = "dt-center", width = col_width)
        )
      )
    ) %>%
      DT::formatCurrency(
        column = 2:n_col,
        currency = "",
        digits = 3
      )
  })

  # return
  triangle_data

}
