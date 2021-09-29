sign_in_page <- function() {
  polished::sign_in_ui_default(
    color = "#006CB5",
    company_name = "PwC",
    logo_top = tags$img(
      src = "images/pwc-inverted.png",
      alt = "PwC Logo",
      style = "width: 125px; margin-top: 30px; margin-bottom: 30px;"
    ),
    logo_bottom = tags$div(
      style = "background-color: #FFF; width: 300px;",
      tags$img(
        src = "images/pwc-logo-transparent.png",
        alt = "PwC Logo",
        style = "width: 200px; margin-bottom: 15px; padding-top: 15px;"
      )
    ),
    icon_href = "images/pwc-logo.png",
    background_image = "images/milky_way.jpeg"
  )
}