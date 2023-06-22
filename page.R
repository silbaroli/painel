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

countries=countries[which(countries %in% unique(DBI::dbReadTable(dbConnect(RSQLite::SQLite(),"patentes_25Mai2023.db"),"pessoa")$pais_iso))]

cat=read.csv("https://raw.githubusercontent.com/silbaroli/painel/main/data/categorias_iea.csv")
cat$nivel1=stringr::str_replace_all(cat$nivel1,"iea","")
cat$nivel2=stringr::str_replace_all(cat$nivel2,"iea","")

page <- dashboardBody(
  
  tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #ffffff;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #ffffff;margin-left: 10px;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #215264; font-family: "Calibri", sans-serif;
                                color: #ffffff;font-size:20px;font-weight: bold;border-radius: 4px;margin-left: 10px;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #ffffff;
                                color: #000000;font-family: "Calibri", sans-serif;
                                font-size:18px;border-radius: 4px;margin-left: 10px;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #e5e5e5;border-radius: 4px;margin-left: 10px;
                                }
                                
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #e5e5e5;
                                }
                                
                                /* toggle button when hovered selected */
                                .skin-blue .main-header .navbar .sidebar-toggle.selected{
                                background-color: #215264;
                                }
                            
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }
                                
                                /* box filter */
                                .box.box-solid.box-primary>.box-header {
                                background-color: #215264;
                                font-family: "Calibri", sans-serif;font-size:20px;
                                }
                                
                                '))
  ),
  fluidPage(
    fluidRow(
      h1(htmlOutput("title")),
      br(),
      box(width = 12, collapsed = F, collapsible = TRUE,title = "Filtros", solidHeader = TRUE, status = "primary",
          column(width = 3,
                 h4(pickerInput("nivel1","Tecnologia energética nível 1",choices = unique(cat$label1),
                                options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count",`count-selected-text` = "{0}/{1} categorias selecionadas"),multiple = T,selected = unique(cat$label1)))
          ),
          column(width = 3,
                 h4(pickerInput("nivel2","Tecnologia energética nível 2",choices = unique(cat$label2),
                                options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count",`count-selected-text` = "{0}/{1} categorias selecionadas"),multiple = T,selected = unique(cat$label2)))  
          ),
          column(width = 3,
                 h4(pickerInput("status","Situação",choices = c("Vigente","Não vigente","Pendente","Extinta"),
                                options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count",`count-selected-text` = "{0}/{1} status selecionados"),multiple = T,selected = c("Vigente")))
          ),
          column(width = 3,
                 h4(pickerInput("pct","Origem do pedido",choices = c("Nacional"=0,"Internacional (PCT)"=1),
                                options = list(`actions-box` = TRUE,`deselect-all-text` = "Desmarcar todas",`select-all-text` = "Marcar todas",size = 10,`selected-text-format` = "count > 3"),multiple = T,selected = c(1,0)))
          ),
          column(width = 2,
                 h4(pickerInput("tp_origem","Agrupamento",choices = c("UF (Brasil)"="Nacional","Países"="Internacional"),multiple = F,selected = "Internacional"))
          ),
          column(width = 2,
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
          column(width = 2,
                 h4(pickerInput("tipo","Tipo",choices = c("Modelo de utilidade","Patente de invenção"),multiple = T,selected = c("Modelo de utilidade","Patente de invenção")))
          ),
          column(width = 6,
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
      cooperacao,
      explorar
    )
  )
)
