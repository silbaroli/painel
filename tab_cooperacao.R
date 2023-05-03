cooperacao<-tabItem(tabName = "cooperacao",
  fluidRow(
    box(width = 12,
      column(width = 4,
        h5(radioButtons("coopera","Cooperação",choices = c("Nacional"="nacional","Internacional"="internacional"),inline = T,selected = "internacional"),align="left")
      ),
      column(width = 5,
        conditionalPanel(condition = "input.tp_plot6=='Barras'",
          h5(radioButtons("select6","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
        )
      ),
      column(width = 3,align="right",
        radioGroupButtons(
          inputId = "tp_plot6",
          label = NULL,
          choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download")),
          choiceValues = c("Barras", "Linhas","Setor","Tabela","Download"),
          status = "primary"
        )
      ),
      hr(),hr(),
      fluidRow(
        conditionalPanel(condition = "input.tp_plot6!='Download'",
          h3(htmlOutput("title_plot6", align = "center"))
        ),
        conditionalPanel(condition = "input.tp_plot6!='Tabela' & input.tp_plot6!='Download'",
          withSpinner(ggiraphOutput("plot6", width = 'auto', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot6=='Tabela'",
          box(width = 12,div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab6",height=400)))
        )
      )
    )
  )
)