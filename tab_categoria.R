

categoria<-tabItem(tabName = "categoria",
  fluidRow(
    box(width = 12,
      column(width = 4,
        h5(radioButtons("nivel","Agrupamento",choices = c("Nível 1","Nível 2"),inline = T))
      ),
      column(width = 5,
        conditionalPanel(condition = "input.tp_plot2=='Barras'",
          h5(radioButtons("select2","Indicador",choices = c("Número absoluto","Proporção"),inline = T))
       )
      ),
      column(width = 3,align="right",
        radioGroupButtons(
          inputId = "tp_plot2",
          label = NULL,
          choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download")),
          choiceValues = c("Barras", "Linhas","Setor","Tabela","Download"),
          status = "primary"
        )
      ),
      hr(),hr(),
      fluidRow(
        conditionalPanel(condition = "input.tp_plot2!='Download'",
          h3(htmlOutput("title_plot2", align = "center"))
        ),
        conditionalPanel(condition = "input.tp_plot2=='Barras' | input.tp_plot2=='Linhas' | input.tp_plot2=='Setor'",
          withSpinner(ggiraphOutput("plot2", width = '100%', height = 600), type = 2)
        ),
        conditionalPanel(condition = "input.tp_plot2=='Tabela'",
          box(width = 12,div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab2",height=400)))
        )
      )
    )
  )
)




            
            
            
            

                  
                  