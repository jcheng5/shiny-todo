shinyUI(basicPage(
  h1("todos"),
  commandInput("newtask", placeholder = "What needs to be done?"),
  uiOutput("tasks"),
  uiOutput("count"),
  radioButtons("filter", NULL, c("All", "Active", "Completed"),
    selected = "All"
  ),
  actionButton("clear", "Clear completed")
))
