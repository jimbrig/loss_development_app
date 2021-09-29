server <- function(input, output, session) {

  shiny::callModule(
    polished::profile_module,
    "profile"
  )

  selected_eval <- reactiveVal(lubridate::ymd("2019-12-31"))

  observeEvent(input$maturity_month, {

    years <- loss_data_all %>%
      filter(month(eval_date) == input$maturity_month) %>%
      pull(eval_date) %>%
      lubridate::year() %>%
      unique()

    hold <- paste0(years, "-", input$maturity_month, "-20")
    choices_ <- end_of_month(hold)

    shinyWidgets::updateAirDateInput(
      session = session,
      "valuation_year",
      "Select Latest Evaluation Year:",
      value = max(choices_)
    )

    selected_eval(max(choices_))

  }, ignoreInit = TRUE)

  observeEvent(input$valuation_year, {
    hold <- end_of_month(lubridate::ymd(input$valuation_year))
    selected_eval(hold)
  }, ignoreInit = TRUE)

  observe({
    print(list("Selected Eval: ", selected_eval()))
  })

  # adjust loss data
  loss_data <- reactive({
    msg <- showNotification("Filtering loss data...")
    on.exit(removeNotification(msg), add = TRUE)

    loss_data_all %>%
      filter(lubridate::month(eval_date) == input$maturity_month, eval_date <= selected_eval())
  })

  triangle_data <- shiny::callModule(
    triangles_module,
    "triangles",
    loss_data = loss_data,
    selected_eval = selected_eval
  )

}

polished::secure_server(
  server #,
  # custom_sign_in_server = sign_in_module_2("")
)
