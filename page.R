source("tab_evolucao.R")
source("tab_categoria.R")
source("tab_status.R")
source("tab_depositante1.R")
source("tab_depositante2.R")
source("tab_inventor.R")
source("tab_cooperacao.R")
source("tab_explorar.R")
source("countries.R")
source("uf.R")

#setwd("/Users/silvanooliveira/Google Drive/Meu Drive/Consultoria/CEPAL/painel/")

cat=read.csv("categorias_iea.csv")
cat$nivel1=stringr::str_replace_all(cat$nivel1,"iea","")
cat$nivel2=stringr::str_replace_all(cat$nivel2,"iea","")

page <- dashboardBody(
  fluidPage(
    fluidRow(
      h1(htmlOutput("title")),
      hr(),
      box(width = 12, collapsed = F, collapsible = TRUE,title = "Filtros", solidHeader = TRUE, status = "primary",
        column(width = 4,
          h4(pickerInput("nivel1","Tecnologia energética nível 1",choices = unique(cat$label1),
                              options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count",`count-selected-text` = "{0}/{1} categorias selecionadas"),multiple = T,selected = unique(cat$label1)))
        ),
        column(width = 4,
          h4(pickerInput("nivel2","Tecnologia energética nível 2",choices = unique(cat$label2),
                              options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count",`count-selected-text` = "{0}/{1} categorias selecionadas"),multiple = T,selected = unique(cat$label2)))  
        ),
        column(width = 4,
          h4(pickerInput("status","Situação",choices = c("Concedido","Deferido","Depositado","Fase Nacional - PCT",
                                                         "Indeferido","Publicado","Sem informação"),
                              options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count",`count-selected-text` = "{0}/{1} status selecionados"),multiple = T,selected = c("Concedido","Deferido","Depositado","Fase Nacional - PCT",
                                                                                                                                                                                                                                                                         "Indeferido","Publicado")))
        ),
        column(width = 3,
          h4(pickerInput("tp_origem","Agrupamento",choices = c("UF (Brasil)"="Nacional","Países"="Internacional"),multiple = F,selected = "Internacional"))
        ),
        column(width = 3,
          conditionalPanel(condition = "input.tp_origem=='Internacional'",
            h4(pickerInput("nacionalidade","País",choices = countries,
                              options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",
                                                `select-all-text` = "Marcar todas",size = 10,
                                                `selected-text-format` = "count",`count-selected-text` = "{0}/{1} países selecionados"),multiple = T,selected = countries))
          ),
          conditionalPanel(condition = "input.tp_origem=='Nacional'",
            h4(pickerInput("uf","Unidade Federada",choices = uf,
                             options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",
                                                `select-all-text` = "Marcar todas",size = 10,
                                                `selected-text-format` = "count",`count-selected-text` = "{0}/{1} UF's selecionadas"),multiple = T,selected = uf))
           )   
        ),
        column(width = 3,
          h4(pickerInput("tp_ano","Ano",choices = c("Pedido"="pedido","Concessão"="Concedido","Deferimento"="Deferido","Indeferimento"="Indeferido",
                                                    "Publicação"="Publicado","Depósito"="Depositado"),
                              options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count > 3"),multiple = F,selected = "pedido"))
        ),
        column(width = 3,
          setSliderColor("Teal", 1),
          h4(sliderInput("date","Período",min=2000,max=year(Sys.Date()),value = c(2010,2020),sep=""))
        )
      )
    ),
    tabItems(
      evolucao,
      categoria,
      status,
      origem,
      tp_pessoa,
      inventor,
      cooperacao,
      explorar
    )
  )
)
