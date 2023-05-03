origem<-tabItem(tabName = "origem",
  fluidRow(
    box(width = 12,
      column(width = 4,
        h5(radioButtons("local","Localização",choices = c("América","Brasil"),inline = T,selected = "Brasil"),align="left")
      ),
      column(width = 5,
        conditionalPanel(condition = "input.tp_plot41=='Barras'",
          h5(radioButtons("select41","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
        )
      ),
      column(width = 3,align="right",
        radioGroupButtons(
          inputId = "tp_plot41",
          label = NULL,
          choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download")),
          choiceValues = c("Barras", "Linhas","Setor","Tabela","Download"),
          status = "primary"
        )
      ),
      hr(),hr(),
      fluidRow(
        conditionalPanel(condition = "input.tp_plot41!='Download'",
          h3(htmlOutput("title_plot41", align = "center"))
        ),
        conditionalPanel(condition = "input.tp_plot41!='Tabela' & input.tp_plot41!='Download'",
          withSpinner(ggiraphOutput("plot41", width = 'auto', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot41=='Tabela'",
          box(width = 12,div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab41",height=400)))
        )
      )
    )
  )
)