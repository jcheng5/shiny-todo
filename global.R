# Custom input control for a textbox that only sends its value
# when you hit Enter.
# See www/binding.js and http://shiny.rstudio.com/articles/building-inputs.html
commandInput <- function(inputId, ..., autoclear = TRUE) {
  tagList(
    tags$head(singleton(tags$script(src="binding.js"))),
    tags$input(id=inputId, type="text", class="command-text",
      `data-autoclear` = autoclear,
      ...)
  )
}
