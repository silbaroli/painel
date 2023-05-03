
status<-tabItem(tabName = "status",
  fluidRow(
    box(width = 12,
      column(width = 9,
        conditionalPanel(condition = "input.tp_plot3=='Barras'",
          h5(radioButtons("select3","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
        )
      ),
      column(width = 3,align="right",
        radioGroupButtons(
          inputId = "tp_plot3",
          label = NULL,
          choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download")),
          choiceValues = c("Barras", "Linhas","Setor","Tabela","Download"),
          status = "primary"
        )
      ),
      hr(),hr(),
      fluidRow(
        conditionalPanel(condition = "input.tp_plot3!='Download'",
          h3(htmlOutput("title_plot3", align = "center"))
        ),
        conditionalPanel(condition = "input.tp_plot3=='Barras' | input.tp_plot3=='Linhas' | input.tp_plot3=='Setor'",
          withSpinner(ggiraphOutput("plot3", width = '100%', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot3=='Tabela'",
          box(width = 12,div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab3",height=400)))
        )
      )
    )
  )
)
