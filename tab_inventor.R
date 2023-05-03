 
inventor<-tabItem(tabName = "inventor",
  fluidRow(
    box(width = 12,
      column(width = 9,
        conditionalPanel(condition = "input.tp_plot5=='Barras'",
          h5(radioButtons("select5","Indicador",choices = c("Número absoluto","Proporção"),inline = T),align="left")
        )
      ),
      column(width = 3,align="right",
        radioGroupButtons(
          inputId = "tp_plot5",
          label = NULL,
          choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download")),
          choiceValues = c("Barras", "Linhas","Setor","Tabela","Download"),
          status = "primary"
        )
      ),
      hr(),hr(),
      fluidRow(
        conditionalPanel(condition = "input.tp_plot5!='Download'",
          h3(htmlOutput("title_plot5", align = "center"))
        ),
        conditionalPanel(condition = "input.tp_plot5=='Barras' | input.tp_plot5=='Linhas' | input.tp_plot5=='Setor'",
          withSpinner(ggiraphOutput("plot5", width = '100%', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot5=='Tabela'",
          box(width = 12,div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab5",height=400)))
        ),
        h5(HTML(paste("&nbsp", "Esta informação está disponível apenas para registros de pedidos de patente com inventores brasileiros")))
      )
    )
  )
)
