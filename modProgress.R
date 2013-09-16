progressInit.2 <- function() {
  addResourcePath('progress', system.file('progress',
                                          package='shinyIncubator'))
  tagList(
    singleton(
      tags$head(
        tags$script(src='progress/progress.js'),
        tags$link(rel="stylesheet", type="text/css", href="style.css")
      )
    )
  )
}