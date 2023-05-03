evolucao<-tabItem(tabName = "evolucao",
  fluidRow(
    box(width = 12,
      column(width = 12,align="right",
        radioGroupButtons(
          inputId = "tp_plot1",
          label = NULL,
          choiceNames = list(icon("chart-column"),icon("chart-line"),icon("table"),icon("download")),
          choiceValues = c("Barras", "Linhas", "Tabela","Download"),
          status = "primary"
        )
      ),
      hr(),
      fluidRow(
        conditionalPanel(condition = "input.tp_plot1!='Download'",
          h3(htmlOutput("title_plot1", align = "center"))
        ),
        conditionalPanel(condition = "input.tp_plot1=='Barras' | input.tp_plot1=='Linhas'",
          withSpinner(ggiraphOutput("plot1", width = "100%", height = 600), type = 2)
          #withLoader(ggiraphOutput("plot1", width = "100%", height = 600), type="html", loader="loader9")
        ),
        conditionalPanel(condition = "input.tp_plot1=='Tabela'",
          box(width = 12,div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab1",height=400,width = "100%")))
        )
      )
    )
  )
)
