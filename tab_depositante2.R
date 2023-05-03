tp_pessoa<-tabItem(tabName = "tp_pessoa",
  fluidRow(
    box(width = 12,
      column(width = 9,
        conditionalPanel(condition = "input.tp_plot42=='Barras'",
          h5(radioButtons("select42","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
        )
      ),
      column(width = 3,align="right",
        radioGroupButtons(
          inputId = "tp_plot42",
          label = NULL,
          choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download")),
          choiceValues = c("Barras", "Linhas","Setor","Tabela","Download"),
          status = "primary"
        )
      ),
      hr(),hr(),
      fluidRow(
        conditionalPanel(condition = "input.tp_plot42!='Download'",
          h3(htmlOutput("title_plot42", align = "center"))
        ),
        conditionalPanel(condition = "input.tp_plot42!='Tabela' & input.tp_plot42!='Download'",
          withSpinner(ggiraphOutput("plot42", width = 'auto', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot42=='Tabela'",
          box(width = 12,div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab42",height=400)))
        )
      )
    )
  )
)