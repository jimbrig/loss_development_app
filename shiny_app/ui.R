
#  ------------------------------------------------------------------------
#
# Title : UI
#    By :
#  Date : 2020-11-10
#
#  ------------------------------------------------------------------------

header <- shinydashboard::dashboardHeader(
  title = "Loss Development",
  polished::profile_module_ui("profile")
)

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id = "menu",
    selectInput(
      "maturity_month",
      "Select Month of Maturity",
      choices = maturity_choices,
      selected = 12,
      selectize = FALSE,
      multiple = FALSE
    ),
    shinyWidgets::airYearpickerInput(
      "valuation_year",
      "Select Latest Evaluation Year:",
      minDate = min(loss_data_all$eval_date),
      maxDate = max(loss_data_all$eval_date),
      value = "2019-12-31",
      autoClose = TRUE,
      # update_on = "close",
      addon = "none",
      width = "100%"
    ),
    shinydashboard::menuItem(
      text = " Triangles",
      tabName = "triangles",
      icon = shiny::icon("sort-amount-asc")
    ),
    shinydashboard::menuItem(
      text = " AvE",
      tabName = "ave",
      icon = shiny::icon("balance-scale"),
      badgeLabel = "Coming Soon!",
      badgeColor = "green"
    ),
    shinydashboard::menuItem(
      text = " Ultimate",
      tabName = "ult",
      icon = shiny::icon("search-dollar"),
      badgeLabel = "Coming Soon!",
      badgeColor = "green"
    )
  )
)

body <- shinydashboard::dashboardBody(
  tags$head(
    tags$link(
      rel = "shortcut icon",
      type = "image/png",
      href = "images/pwc-logo.png"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  shinyjs::useShinyjs(),
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "triangles",
      triangles_module_ui("triangles", loss_data_all)
    ),
    shinydashboard::tabItem(
      tabName = "ave"#,
      # ave_module_ui("ave")
    ),
    shinydashboard::tabItem(
      tabName = "ult"#,
      # ult_module_ui("ave")
    )
  )
)

ui <- shinydashboard::dashboardPage(
  header, sidebar, body,
  title = "Loss Development",
  skin = "black"
)

polished::secure_ui(
  ui,
  sign_in_page_ui = sign_in_page()
)
