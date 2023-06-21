library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(DT)
library(ggplot2)
library(ggiraph)
library(plotly)
library(stringr)
library(lubridate)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(DBI)


options(spinner.color = "#7197A4", spinner.color.background = "#ffffff", spinner.size = 2, shiny.reactlog=TRUE)


sidebar <- dashboardSidebar(
  width = 300,
  useShinyjs(),
  sidebarMenu(id="tab",
              menuItem("Evolução das patentes", tabName = "evolucao", icon = icon("chart-line")),
              menuItem("Classificação tecnológica", tabName = "categoria", icon = icon("bars")),
              menuItem("Situação das patentes", tabName = "status", icon = icon("check")),
              #menuItem("Perfil do depositante", tabName = "origem", icon = icon("flask")),
              #menuItem("Perfil do inventor", tabName = "inventor", icon = icon("user-gear")),
              menuItem("Cooperação", tabName = "cooperacao", icon = icon("globe")),
              menuItem("Explorar patentes", tabName = "explorar", icon = icon("magnifying-glass"))
              #menuItem("Nota metodológica",tabName = "nota",icon = icon("book"))
  )
)


header <- dashboardHeader(disable = TRUE)

evolucao<-tabItem(tabName = "evolucao",
                  fluidRow(
                    box(width = 12,
                        column(width = 3,offset=9,align="right",
                               radioGroupButtons(
                                 inputId = "tp_plot1",
                                 label = NULL,
                                 choiceNames = list(icon("chart-column"),icon("chart-line"),icon("table"),icon("download"),icon("triangle-exclamation")),
                                 choiceValues = c("Barras", "Linhas", "Tabela","Download","Alerta"),
                                 status = "primary"
                               ),
                               tags$script("$(\"input:radio[name='tp_plot1'][value='Barras']\").parent().css('background-color', '#215264');"),
                               tags$script("$(\"input:radio[name='tp_plot1'][value='Linhas']\").parent().css('background-color', '#215264');"),
                               tags$script("$(\"input:radio[name='tp_plot1'][value='Tabela']\").parent().css('background-color', '#215264');"),
                               tags$script("$(\"input:radio[name='tp_plot1'][value='Download']\").parent().css('background-color', '#215264');"),
                               tags$script("$(\"input:radio[name='tp_plot1'][value='Alerta']\").parent().css('background-color', '#F4BB47');")
                               
                        ),
                        tags$hr(),
                        fluidRow(
                          conditionalPanel(condition = "input.tp_plot1!='Download' & input.tp_plot1!='Alerta'",
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
                                  choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download"),icon("triangle-exclamation")),
                                  choiceValues = c("Barras", "Linhas","Setor","Tabela","Download","Alerta"),
                                  status = "primary"
                                ),
                                tags$script("$(\"input:radio[name='tp_plot2'][value='Barras']\").parent().css('background-color', '#215264');"),
                                tags$script("$(\"input:radio[name='tp_plot2'][value='Linhas']\").parent().css('background-color', '#215264');"),
                                tags$script("$(\"input:radio[name='tp_plot2'][value='Tabela']\").parent().css('background-color', '#215264');"),
                                tags$script("$(\"input:radio[name='tp_plot2'][value='Setor']\").parent().css('background-color', '#215264');"),
                                tags$script("$(\"input:radio[name='tp_plot2'][value='Download']\").parent().css('background-color', '#215264');"),
                                tags$script("$(\"input:radio[name='tp_plot2'][value='Alerta']\").parent().css('background-color', '#F4BB47');")
                         ),
                         tags$hr(),
                         tags$hr(),
                         fluidRow(
                           conditionalPanel(condition = "input.tp_plot2!='Download' & input.tp_plot2!='Alerta'",
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
                               choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download"),icon("triangle-exclamation")),
                               choiceValues = c("Barras", "Linhas","Setor","Tabela","Download","Alerta"),
                               status = "primary"
                             ),
                             tags$script("$(\"input:radio[name='tp_plot3'][value='Barras']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot3'][value='Linhas']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot3'][value='Tabela']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot3'][value='Setor']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot3'][value='Download']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot3'][value='Alerta']\").parent().css('background-color', '#F4BB47');")
                      ),
                      tags$hr(),
                      tags$hr(),
                      fluidRow(
                        conditionalPanel(condition = "input.tp_plot3!='Download' & input.tp_plot3!='Alerta'",
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
                               choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download"),icon("triangle-exclamation")),
                               choiceValues = c("Barras", "Linhas","Setor","Tabela","Download","Alerta"),
                               status = "primary"
                             ),
                             tags$script("$(\"input:radio[name='tp_plot41'][value='Barras']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot41'][value='Linhas']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot41'][value='Tabela']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot41'][value='Setor']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot41'][value='Download']\").parent().css('background-color', '#215264');"),
                             tags$script("$(\"input:radio[name='tp_plot41'][value='Alerta']\").parent().css('background-color', '#F4BB47');")
                      ),
                      tags$hr(),
                      tags$hr(),
                      fluidRow(
                        conditionalPanel(condition = "input.tp_plot41!='Download' & input.tp_plot41!='Alerta'",
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
                                   choiceNames = list(icon("chart-column"),icon("chart-line"),icon("chart-pie"),icon("table"),icon("download"),icon("triangle-exclamation")),
                                   choiceValues = c("Barras", "Linhas","Setor","Tabela","Download","Alerta"),
                                   status = "primary"
                                 ),
                                 tags$script("$(\"input:radio[name='tp_plot6'][value='Barras']\").parent().css('background-color', '#215264');"),
                                 tags$script("$(\"input:radio[name='tp_plot6'][value='Linhas']\").parent().css('background-color', '#215264');"),
                                 tags$script("$(\"input:radio[name='tp_plot6'][value='Tabela']\").parent().css('background-color', '#215264');"),
                                 tags$script("$(\"input:radio[name='tp_plot6'][value='Setor']\").parent().css('background-color', '#215264');"),
                                 tags$script("$(\"input:radio[name='tp_plot6'][value='Download']\").parent().css('background-color', '#215264');"),
                                 tags$script("$(\"input:radio[name='tp_plot6'][value='Alerta']\").parent().css('background-color', '#F4BB47');")
                          ),
                          tags$hr(),
                          tags$hr(),
                          fluidRow(
                            conditionalPanel(condition = "input.tp_plot6!='Download' & input.tp_plot6!='Alerta'",
                                             h3(htmlOutput("title_plot6", align = "center"))
                            ),
                            conditionalPanel(condition = "input.tp_plot6!='Tabela' & input.tp_plot6!='Download' & input.tp_plot6!='Alerta'",
                                             withSpinner(ggiraphOutput("plot6", width = 'auto', height = 600), type = 2)
                            ),
                            conditionalPanel(condition = "input.tp_plot6=='Tabela'",
                                             box(width = 12,div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab6",height=400)))
                            )
                          )
                      )
                    )
)


explorar<-tabItem(tabName = "explorar",
                  box(width = 12,
                      div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab7",height=600)),
                      uiOutput("popup")
                  )
)

countries=c("Afeganistão"="AF","África do Sul"="ZA","Albânia"="AL","Alemanha"="DE","Andorra"="AD","Angola"="AO",
            "Anguila"="AI","Antiga e Barbuda"="AG","Antilhas Holandesas"="AN","Arábia Saudita"="SA",
            "Argélia"="DZ","Argentina"="AR","Arménia"="AM","Aruba"="AW","Austrália"="AU","Áustria"="AT",
            "Azerbaijão"="AZ","Bahamas"="BS","Bangladexe"="BD","Barbados"="BB","Barém"="BH","Bélgica"="BE",
            "Belize"="BZ","Benim"="BJ","Bermuda"="BM","Bielorrússia"="BY","Bolívia"="BO","Bonaire"="XA",
            "Bósnia e Herzegovina"="BA","Botsuana"="BW","Brasil"="BR","Brunei"="BN","Bulgária"="BG",
            "Burquina Faso"="BF","Burúndi"="BI","Butão"="BT","Cabo Verde"="CV","Camarões"="CM","Camboja"="KH",
            "Canadá"="CA","Catar"="QA","Cazaquistão"="KZ","Chade"="TD","Chile"="CL","China"="CN","Chipre"="CY",
            "Colômbia"="CO","Comores"="KM","Congo"="CG","Coreia do Norte"="KP","Coreia do Sul"="KR",
            "Costa do Marfim"="CI","Costa Rica"="CR","Croácia"="HR","Cuaite"="KW","Cuba"="CU","Curaçao"="CW",
            "Dinamarca"="DK","Djibouti"="DJ","Dominica"="DM","Egito"="EG","El Salvador"="SV",
            "Emirados Árabes Unidos"="AE","Equador"="EC","Eritreia"="ER","Eslováquia"="SK","Eslovénia"="SI",
            "Espanha"="ES","Estados Unidos"="US","Estônia"="EE","Etiópia"="ET","Fiji"="FJ","Filipinas"="PH",
            "Finlândia"="FI","França"="FR","Gabão"="GA","Gâmbia"="GM","Gana"="GH","Geórgia"="GE","Gibraltar"="GI",
            "Granada"="GD","Grécia"="GR","Guadalupe"="GP","Guatemala"="GT","Guiana"="GY","Guiana Francesa"="GF",
            "Guiné"="GN","Guiné Equatorial"="GQ","Guiné-Bissau"="GW","Haiti"="HT","Honduras"="HN","Hong Kong"="HK",
            "Hungria"="HU","Iémen"="LB","Iêmen"="YE","Ilha de Man"="IM","Ilha de São Martinho"="MF","Ilhas Cayman"="KY",
            "Ilhas Cook"="CK","Ilhas Malvinas"="FK","Ilhas Marechal"="MH","Ilhas Salomão"="SB",
            "Ilhas Turcas e Caicos"="TC","Ilhas Virgens Americanas"="VI","Ilhas Virgens Britânicas"="VG",
            "Índia"="IN","Indonésia"="ID","Irão"="IR","Iraque"="IQ","Irlanda"="IE","Islândia"="IS","Israel"="IL",
            "Itália"="IT","Iugoslávia"="YU","Jamaica"="JM","Japão"="JP","Jordânia"="JO","Laus"="LA","Lesoto"="LS",
            "Letônia"="LV","Líbano"="LR","Líbia"="LY","Liechtenstein"="LI","Lituânia"="LT","Luxemburgo"="LU",
            "Macedónia do Norte"="MK","Madagascar"="MG","Malásia"="MY","Maláui"="MW","Maldivas"="MV","Mali"="ML",
            "Malta"="MT","Marrocos"="MA","Martinica"="MQ","Maurícia"="MU","Mauritânia"="MR","Melilha"="EA",
            "México"="MX","Mianmar"="MM","Micronésia"="FM","Moçambique"="MZ","Moldávia"="MD","Mónaco"="MC",
            "Mongólia"="MN","Montenegro"="ME","Montserrat"="MS","Namíbia"="NA","Nauru"="NR","Nepal"="NP",
            "Nicarágua"="NI","Níger"="NE","Nigéria"="NG","Niue"="NU","Noruega"="NO","Nova Caledônia"="NC",
            "Nova Zelândia"="NZ","Omã"="OM","Países Baixos"="NL","Palau"="PW","Panamá"="PA","Papua Nova Guiné"="PG",
            "Paquistão"="PK","Paraguai"="PY","Peru"="PE","Polónia"="PL","Porto Rico"="PR","Portugal"="PT",
            "Quénia"="KE","Quirguistão"="KG","Quiribáti"="KI","Reino Unido"="GB","República Centro-Africana"="CF",
            "República Checa"="CZ","República Democrática do Congo"="CD","República Dominicana"="DO","Roménia"="RO",
            "Ruanda"="RW","Rússia"="RU","Saba"="XC","Samoa"="WS","Santa Lúcia"="LC","Santo Eustáquio"="XB",
            "São Bartolomeu"="BL","São Cristóvão e Neves"="KN","São Marinho"="SM","São Martinho"="SX",
            "São Pedro e Miquelão"="PM","São Tomé e Príncipe"="ST","São Vicente e Granadinas"="VC",
            "Seicheles"="SC","Senegal"="SN","Serra Leoa"="SL","Sérvia"="RS","Singapura"="SG","Síria"="SY",
            "Somália"="SO","Sri Lanca"="LK","Suazilândia"="SZ","Sudão"="SD","Sudão do Sul"="SS","Suécia"="SE",
            "Suíça"="CH","Suriname"="SR","Tailândia"="TH","Taiwan"="TW","Tajiquistão"="TJ","Tanzânia"="TZ",
            "Timor-Leste"="TL","Togo"="TG","Tonga"="TO","Trindade e Tobago"="TT","Tunísia"="TN","Turcomenistão"="TM",
            "Turquia"="TR","Tuvalu"="TV","Ucrânia"="UA","Uganda"="UG","Uruguai"="UY","Usbequistão"="UZ","Vanuatu"="VU",
            "Venezuela"="VE","Vietname"="VN","Wallis e Futuna"="WF","Zâmbia"="ZM","Zimbábue"="ZW","Não informado")

countries=countries[which(countries %in% unique(DBI::dbReadTable(dbConnect(RSQLite::SQLite(),"patentes_25Mai2023.db"),"pessoa")$pais_iso))]

uf=c("Rondônia"="RO","Acre"="AC","Amazonas"="AM","Pará"="PA","Roraima"="RR",
     "Amapá"="AP","Tocantins"="TO","Maranhão"="MA","Piauí"="PI","Ceará"="CE",
     "Rio Grande do Norte"="RN","Paraíba"="PB","Pernambuco"="PE","Alagoas"="AL",
     "Sergipe"="SE","Bahia"="BA","Minas Gerais"="MG","Espírito Santo"="ES",
     "Rio de Janeiro"="RJ","São Paulo"="SP","Paraná"="PR","Santa Catarina"="SC",
     "Rio Grande do Sul"="RS","Mato Grosso do Sul"="MS","Mato Grosso"="MT",
     "Goiás"="GO","Distrito Federal"="DF","Não informado")

cat=read.csv("https://raw.githubusercontent.com/silbaroli/painel/main/data/categorias_iea.csv")
#cat=read.csv("data/categorias_iea.csv")
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
                                font-family: "Calibri", sans-serif;font-size:18px;
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


shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

ui <- dashboardPage(header,sidebar,page)

server <- function(input, output, session) {
  
  observeEvent(input$nivel1, {
    cat<-cat %>% filter(label1 %in% c(input$nivel1))
    
    updatePickerInput(session = session, inputId = "nivel2",
                      choices = unique(cat$label2),selected = unique(cat$label2))
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$tp_plot1,{
    req(input$tp_plot1=="Download")
    
    showModal(tags$div(id="modal1", modalDialog(
      tags$head(tags$style("#modal1 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Download de arquivo"),
      htmlOutput("alerta_modal1"),
      br(),
      selectInput("format1","Formato:",c(".xlsx",".csv")),
      downloadButton("data1", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot1,{
    req(input$tp_plot1=="Alerta")
    
    showModal(tags$div(id="modal1", modalDialog(
      tags$head(tags$style("#modal1 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Descrição dos dados"),
      htmlOutput("alerta_modal1"),
      tags$hr(),
      h3("Nota metodológica"),
      downloadButton("pdf", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot2,{
    req(input$tp_plot2=="Download")
    
    showModal(tags$div(id="modal2", modalDialog(
      tags$head(tags$style("#modal2 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Download de arquivo"),
      htmlOutput("alerta_modal2"),
      br(),
      selectInput("format2","Formato:",c(".xlsx",".csv")),
      downloadButton("data2", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot2,{
    req(input$tp_plot2=="Alerta")
    
    showModal(tags$div(id="modal2", modalDialog(
      tags$head(tags$style("#modal2 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Descrição dos dados"),
      htmlOutput("alerta_modal2"),
      tags$hr(),
      h3("Nota metodológica"),
      downloadButton("pdf", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot3,{
    req(input$tp_plot3=="Download")
    
    showModal(tags$div(id="modal3", modalDialog(
      tags$head(tags$style("#modal3 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Download de arquivo"),
      htmlOutput("alerta_modal3"),
      br(),
      selectInput("format3","Formato:",c(".xlsx",".csv")),
      downloadButton("data3", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot3,{
    req(input$tp_plot3=="Alerta")
    
    showModal(tags$div(id="modal3", modalDialog(
      tags$head(tags$style("#modal3 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Descrição dos dados"),
      htmlOutput("alerta_modal3"),
      tags$hr(),
      h3("Nota metodológica"),
      downloadButton("pdf", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot41,{
    req(input$tp_plot41=="Download")
    
    showModal(tags$div(id="modal4.1", modalDialog(
      tags$head(tags$style("#modal4.1 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Download de arquivo"),
      htmlOutput("alerta_modal4.1"),
      br(),
      selectInput("format4.1","Formato:",c(".xlsx",".csv")),
      downloadButton("data4.1", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot41,{
    req(input$tp_plot41=="Alerta")
    
    showModal(tags$div(id="modal4.1", modalDialog(
      tags$head(tags$style("#modal4.1 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Descrição dos dados"),
      htmlOutput("alerta_modal4.1"),
      tags$hr(),
      h3("Nota metodológica"),
      downloadButton("pdf", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot6,{
    req(input$tp_plot6=="Download")
    
    showModal(tags$div(id="modal6", modalDialog(
      tags$head(tags$style("#modal6 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Download de arquivo"),
      htmlOutput("alerta_modal6"),
      br(),
      selectInput("format6","Formato:",c(".xlsx",".csv")),
      downloadButton("data6", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )
  
  observeEvent(input$tp_plot6,{
    req(input$tp_plot6=="Alerta")
    
    showModal(tags$div(id="modal6", modalDialog(
      tags$head(tags$style("#modal6 .modal-header {background-color: #215264; text-align: center}")),
      title=tags$a(style = "color: white;font-family: Calibri, sans-serif;font-size:20px;font-weight: bold", "Descrição dos dados"),
      htmlOutput("alerta_modal6"),
      tags$hr(),
      h3("Nota metodológica"),
      downloadButton("pdf", label = "Download", class = NULL,icon = icon("download")),
      tags$span(style = "color:blue"),
      easyClose = TRUE,footer = NULL)
    ))}
  )

  observeEvent(input$select_button, {
    toggleModal(session, "detalhes", "open")
  })
  
  SelectedRow <- eventReactive(input$select_button,{
    as.numeric(strsplit(input$select_button, "_")[[1]][2])
  })
  
  #url="https://github.com/silbaroli/painel_patentes/blob/main/data/patentes_14Abr2023.db?raw=true"
  #url="https://github.com/silbaroli/painel/blob/main/data/patentes_02Mai2023.db"
  #download.file(url,"patentes_02Mai2023.db")
  
  con <- dbConnect(RSQLite::SQLite(),"patentes_25Mai2023.db")
  #con <- dbConnect(RSQLite::SQLite(),"data/patentes.db")
  
  sqltb1 <- DBI::dbReadTable(con,"patente") %>% mutate(pct=ifelse(is.na(pct),0,pct)) %>% dplyr::select(-uf)
  sqltb2 <- DBI::dbReadTable(con,"iea_patente")
  sqltb3 <- DBI::dbReadTable(con,"iea_categoria")
  sqltb5 <- DBI::dbReadTable(con,"pessoa") %>% 
    mutate(america=ifelse(str_detect(pais_iso,paste(c("AR","BO","BR","CL","CO","EC","PE","PY","UY","VE"),collapse = "|")),1,0))
  sqltb6 <- DBI::dbReadTable(con,"iea_grupo")
  sqltb7 <- DBI::dbReadTable(con,"ipc_patente")
  
  database <- reactive({
    
    sqltb3 <- sqltb3 %>% 
      filter(descricao %in% c(input$nivel2))

    sqltb5 <-full_join(
      sqltb5 %>% 
        filter(categoria_pessoa == "Depositante"),
      sqltb1[,c("id_patente","tipo")],by="id_patente") %>% 
      mutate(pais = case_when(is.na(pais) ~ "Não informado",
                              (pais %in% c("IP","OA","PB","YU")) ~ "Não informado",
                              pais=="DD" ~ "DE", 
                              pais=="NY" ~ "US",
                              pais=="UK" ~ "GB",
                              pais=="SU" ~ "RU",
                              TRUE ~ pais)) %>% 
      filter(case_when(input$tp_origem=="Nacional" ~ str_detect(uf,paste(c(input$uf),collapse = "|")),
                       TRUE ~ str_detect(pais,paste(c(input$nacionalidade),collapse = "|"))))
    
    aux=sqltb5 %>% 
      filter(categoria_pessoa=="Depositante" & is.na(tipo_pessoa)) %>% 
      mutate(brasil = ifelse(str_detect(pais,"BR"),1,0),
             outros = ifelse(str_detect(pais,"BR")==F,1,0)) %>% 
      group_by(id_patente) %>% 
      summarise(brasil=sum(brasil),outros=sum(outros)) %>% 
      mutate(cooper_nac = ifelse(brasil>1,1,0),cooper_inter = ifelse(brasil>=1 & outros>=1,1,0)) %>% 
      select(id_patente,cooper_nac,cooper_inter)
    
    
    df<- sqltb1 %>%
      inner_join(
        inner_join(
          sqltb2[!duplicated(sqltb2$id_patente),c("id_patente","codigo_categoria")],
          sqltb3,by=c("codigo_categoria"="codigo")),
        by="id_patente") %>% 
      inner_join(sqltb5[!duplicated(sqltb5$id_patente),c("id_patente","tipo_pessoa","pais")],by="id_patente") %>% 
      left_join(aux,by="id_patente") %>% 
      mutate(ano=ano_pedido) %>%
      mutate(status_atual = ifelse(is.na(status_atual),"Sem informação",status_atual)) %>% 
      mutate(feminino = case_when(brasil==1 & presenca_feminina_ibge=="F" ~ 1,
                                  brasil==1 & presenca_feminina_ibge=="N" ~ 0,
                                  brasil==1 & presenca_feminina_ibge=="I" ~ 2,
                                  TRUE ~ 9)) %>% 
      mutate(cooper_nac = ifelse(is.na(cooper_nac) & brasil==1,9,cooper_nac)) %>% 
      mutate(cooper_inter = ifelse(is.na(cooper_inter) & brasil==1,9,cooper_inter)) %>% 
      mutate(count=1) %>% 
      filter(ano>=min(input$date) & ano<=max(input$date)) %>% 
      filter(status_atual %in% c(input$status)) %>% 
      filter(excluir ==0) %>% 
      filter(pct %in% c(input$pct)) %>% 
      filter(tipo %in% c(input$tipo)) %>% 
      collect()
  })
  
  database2 <- reactive({
    
    df2 <- sqltb2 %>% 
      inner_join(sqltb3 %>% rename("nivel2" = "descricao"),by=c("codigo_categoria"="codigo")) %>% 
      inner_join(sqltb6 %>% rename("nivel1" = "descricao"),by=c("codigo_grupo"="codigo")) %>% 
      inner_join(sqltb1 %>% filter(pct %in% c(input$pct)) %>% select(id_patente,status_atual,ano_pedido,excluir,tipo),by="id_patente") %>% 
      inner_join(sqltb5 %>% 
                   filter(categoria_pessoa == "Depositante") %>% 
                   select(id_patente,pais,uf) %>% 
                   group_by(id_patente) %>% 
                   mutate(pais = na.omit(paste0(pais,collapse = ","))) %>% 
                   mutate(uf = na.omit(paste0(uf,collapse = ","))) %>%
                   filter(!duplicated(id_patente)),
                 by="id_patente") %>% 
      select(-codigo_categoria,-codigo_grupo) %>%
      mutate(ano=ano_pedido) %>%
      filter(nivel2 %in% c(input$nivel2)) %>% 
      filter(ano>=min(input$date) & ano<=max(input$date)) %>% 
      filter(status_atual %in% c(input$status)) %>% 
      filter(case_when(input$tp_origem=="Nacional" ~ str_detect(uf,paste(c(input$uf),collapse = "|")),
                       TRUE ~ str_detect(pais,paste(c(input$nacionalidade),collapse = "|")))) %>% 
      filter(excluir ==0) %>%
      filter(tipo %in% c(input$tipo)) %>% 
      mutate(count=1) %>% 
      collect()
  })
  
  database3 <- reactive({
    
    df3<-sqltb1 %>% 
      inner_join(
        sqltb2 %>% 
          filter(!duplicated(paste(id_patente,codigo_categoria))) %>% 
          inner_join(sqltb3,by=c("codigo_categoria"="codigo")) %>% 
          group_by(id_patente) %>% 
          summarise(nivel2=paste(descricao,collapse = ", ")),
        by="id_patente") %>% 
      inner_join(
        sqltb7 %>% 
          group_by(id_patente) %>% 
          summarise(ipc=paste(categoria,collapse = ", ")),
        by="id_patente") %>% 
      full_join(
        sqltb5 %>% 
          filter(categoria_pessoa == "Depositante") %>% 
          group_by(id_patente) %>% 
          summarise(depositantes=paste(nome,collapse = ", "),
                    pais=paste(pais_iso,collapse = ", "),
                    uf=paste(uf,collapse = ", ")),
        by="id_patente") %>% 
      mutate(ano=ano_pedido) %>%
      filter(!is.na(ipc)) %>% 
      filter(ano>=min(input$date) & ano<=max(input$date)) %>% 
      filter(status_atual %in% c(input$status)) %>% 
      filter(case_when(input$tp_origem=="Nacional" ~ str_detect(uf,paste(c(input$uf),collapse = "|")),
                       TRUE ~ str_detect(pais,paste(c(input$nacionalidade),collapse = "|")))) %>% 
      filter(str_detect(nivel2,paste(c(input$nivel2),collapse = "|"))) %>% 
      filter(excluir==0) %>% 
      filter(pct %in% c(input$pct)) %>% 
      filter(tipo %in% c(input$tipo)) %>% 
      dplyr::select(numero_pedido,titulo,resumo,depositantes,status_atual,ano,pais,uf,ipc,nivel2) %>%
      collect()
    
  })
  
  output$title <- renderUI({

    if(input$tab=="evolucao"){
      htmlText = paste("<div style='font-weight: bold';'font-family: Calibri, sans-serif;font-size: 30px';'margin-left: 0.2px'!important;>","Evolução temporal dos pedidos de patentes","</div>")
    } else if(input$tab=="categoria"){
      htmlText = paste("<div style='font-weight: bold';'font-family: Calibri, sans-serif;font-size: 30px';'margin-left: 0.2px'!important;>","Pedidos de patentes por classificação tecnológica","</div>")
    } else if(input$tab=="status"){
      htmlText = paste("<div style='font-weight: bold';'font-family: Calibri, sans-serif;font-size: 30px';'margin-left: 0.2px'!important;>","Situação dos pedidos de patentes","</div>")
    } else if(input$tab=="origem" | input$tab=="tp_pessoa"){
      htmlText = paste("<div style='font-weight: bold';'font-family: Calibri, sans-serif;font-size: 30px';'margin-left: 0.2px'!important;>","Perfil do depositante","</div>")
    } else if(input$tab=="inventor"){
      htmlText = paste("<div style='font-weight: bold';'font-family: Calibri, sans-serif;font-size: 30px';'margin-left: 0.2px'!important;>","Perfil do inventor","</div>")
    } else if(input$tab=="cooperacao"){
      htmlText = paste("<div style='font-weight: bold';'font-family: Calibri, sans-serif;font-size: 30px';'margin-left: 0.2px'!important;>","Cooperação entre os pedidos de patentes","</div>")
    } else if(input$tab=="explorar"){
      htmlText = paste("<div style='font-weight: bold';'font-family: Calibri, sans-serif;font-size: 30px';'margin-left: 0.2px'!important;>","Explorar os pedidos de patentes","</div>")
    }

    HTML(htmlText)
  })
  
  output$title2 <- renderUI({
    
    htmlText = paste("<div style='font-weight: bold';'font-family: Calibri, sans-serif;font-size: 30px';'margin-left: 0.2px'!important;>","Nota metodológica","</div>")
    HTML(htmlText)
  })
  
  output$alerta_modal1 <- renderUI({
    
    fmt1="<div style='font-family: Calibri, sans-serif;font-size: 16px';'margin-left: 0.2px'!important;>"
    fmt2="</div>"
    
    htmlText = paste(
      fmt1,
      "<p>Estes dados representam a quantidade de patentes depositadas por ano e compreende o período de",
      min(database()$ano,na.rm=T),"a",max(database()$ano,na.rm=T),
      "e considera os seguintes filtros:</p></div>",
      "<br><b>Tecnologias energéticas:</b>",ifelse(length(input$nivel2)==17,"Todas categorias",paste(unique(input$nivel2),collapse = ', ')),
      "<br><b>Situação das patentes:</b>",ifelse(length(input$status)==4,"Todos os status",paste(unique(input$status),collapse = ', ')),
      #"<br><b>Origem do pedido:</b>",paste(unique(input$pct),collapse = ','),
      "<br><b>",ifelse(input$tp_origem=="Internacional","País(es):","UF(s):"),"</b>",ifelse(input$tp_origem=="Nacional" & length(input$uf)==28,"Todas as UFs",
                                                                                            ifelse(input$tp_origem=="Nacional" & length(input$uf)<28,paste(unique(input$uf),collapse = ","),
                                                                                                   ifelse(input$tp_origem=="Internacional" & length(input$nacionalidade)==110,"Todos os países",
                                                                                                          paste(unique(input$nacionalidade),collapse = ", ")))),
      "<br><b>Tipo:</b>",paste(unique(input$tipo),collapse = ', '),
      "</br>",
      "<p></p>",
      "<b>Observação: </b>Estes números representam a contagem do número de patentes, ou seja, 
      não há dupla contagem quando uma patente se encontra em mais de uma categoria tecnológica ou mais de um país/UF.",
      fmt2)
    
    HTML(htmlText)
  })
  
  output$alerta_modal2 <- renderUI({
    
    fmt1="<div style='font-family: Calibri, sans-serif;font-size: 16px';'margin-left: 0.2px'!important;>"
    fmt2="</div>"
    
    htmlText = paste(
      fmt1,
      "<p>Estes dados representam a quantidade de patentes depositadas por categoria tecnoógica e ano e compreende o período de",
      min(database()$ano,na.rm=T),"a",max(database()$ano,na.rm=T),
      "e considera os seguintes filtros:</p></div>",
      "<br><b>Tecnologias energéticas:</b>",ifelse(length(input$nivel2)==17,"Todas categorias",paste(unique(input$nivel2),collapse = ', ')),
      "<br><b>Situação das patentes:</b>",ifelse(length(input$status)==4,"Todos os status",paste(unique(input$status),collapse = ', ')),
      #"<br><b>Origem do pedido:</b>",paste(unique(input$pct),collapse = ','),
      "<br><b>",ifelse(input$tp_origem=="Internacional","País(es):","UF(s):"),"</b>",ifelse(input$tp_origem=="Nacional" & length(input$uf)==28,"Todas as UFs",
                                                                                            ifelse(input$tp_origem=="Nacional" & length(input$uf)<28,paste(unique(input$uf),collapse = ","),
                                                                                                   ifelse(input$tp_origem=="Internacional" & length(input$nacionalidade)==110,"Todos os países",
                                                                                                          paste(unique(input$nacionalidade),collapse = ", ")))),
      "<br><b>Tipo:</b>",paste(unique(input$tipo),collapse = ', '),
      "</br>",
      "<p></p>",
      "<b>Observação: </b>Estes números representam a contagem do número de patentes para cada categoria tecnológica, ou seja, 
      a soma das categorias tecnológicas é maior que o número total de patentes.",
      fmt2)
    
    HTML(htmlText)
  })
  
  output$alerta_modal3 <- renderUI({
    
    fmt1="<div style='font-family: Calibri, sans-serif;font-size: 16px';'margin-left: 0.2px'!important;>"
    fmt2="</div>"
    
    htmlText = paste(
      fmt1,
      "<p>Estes dados representam a quantidade de patentes depositadas por status e ano e compreende o período de",
      min(database()$ano,na.rm=T),"a",max(database()$ano,na.rm=T),
      "e considera os seguintes filtros:</p></div>",
      "<br><b>Tecnologias energéticas:</b>",ifelse(length(input$nivel2)==17,"Todas categorias",paste(unique(input$nivel2),collapse = ', ')),
      "<br><b>Situação das patentes:</b>",ifelse(length(input$status)==4,"Todos os status",paste(unique(input$status),collapse = ', ')),
      #"<br><b>Origem do pedido:</b>",paste(unique(input$pct),collapse = ','),
      "<br><b>",ifelse(input$tp_origem=="Internacional","País(es):","UF(s):"),"</b>",ifelse(input$tp_origem=="Nacional" & length(input$uf)==28,"Todas as UFs",
                                                                                            ifelse(input$tp_origem=="Nacional" & length(input$uf)<28,paste(unique(input$uf),collapse = ","),
                                                                                                   ifelse(input$tp_origem=="Internacional" & length(input$nacionalidade)==110,"Todos os países",
                                                                                                          paste(unique(input$nacionalidade),collapse = ", ")))),
      "<br><b>Tipo:</b>",paste(unique(input$tipo),collapse = ', '),
      "</br>",
      "<p></p>",
      "<b>Observação: </b>Estes números representam a contagem do número de patentes, ou seja, 
      não há dupla contagem quando uma patente se encontra em mais de uma categoria tecnológica ou mais de um país/UF.",
      fmt2)
    
    HTML(htmlText)
  })
  
  output$alerta_modal4.1 <- renderUI({
    
    fmt1="<div style='font-family: Calibri, sans-serif;font-size: 16px';'margin-left: 0.2px'!important;>"
    fmt2="</div>"
    
    htmlText = paste(
      fmt1,
      "<p>Estes dados representam a quantidade de patentes depositadas segundo perfil do depositante e ano e compreende o período de",
      min(database()$ano,na.rm=T),"a",max(database()$ano,na.rm=T),
      "e considera os seguintes filtros:</p></div>",
      "<br><b>Tecnologias energéticas:</b>",ifelse(length(input$nivel2)==17,"Todas categorias",paste(unique(input$nivel2),collapse = ', ')),
      "<br><b>Situação das patentes:</b>",ifelse(length(input$status)==4,"Todos os status",paste(unique(input$status),collapse = ', ')),
      #"<br><b>Origem do pedido:</b>",paste(unique(input$pct),collapse = ','),
      "<br><b>",ifelse(input$tp_origem=="Internacional","País(es):","UF(s):"),"</b>",ifelse(input$tp_origem=="Nacional" & length(input$uf)==28,"Todas as UFs",
                                                                                            ifelse(input$tp_origem=="Nacional" & length(input$uf)<28,paste(unique(input$uf),collapse = ","),
                                                                                                   ifelse(input$tp_origem=="Internacional" & length(input$nacionalidade)==110,"Todos os países",
                                                                                                          paste(unique(input$nacionalidade),collapse = ", ")))),
      "<br><b>Tipo:</b>",paste(unique(input$tipo),collapse = ', '),
      "</br>",
      "<p></p>",
      "<b>Observação: </b>Estes números representam a contagem do número de patentes, ou seja, 
      não há dupla contagem quando uma patente se encontra em mais de uma categoria tecnológica ou mais de um país/UF.",
      fmt2)
    
    HTML(htmlText)
  })
  
  output$alerta_modal6 <- renderUI({
    
    fmt1="<div style='font-family: Calibri, sans-serif;font-size: 16px';'margin-left: 0.2px'!important;>"
    fmt2="</div>"
    
    htmlText = paste(
      fmt1,
      "<p>Estes dados representam a quantidade de patentes depositadas segundo cooperação e ano e compreende o período de",
      min(database()$ano,na.rm=T),"a",max(database()$ano,na.rm=T),
      "e considera os seguintes filtros:</p></div>",
      "<br><b>Tecnologias energéticas:</b>",ifelse(length(input$nivel2)==17,"Todas categorias",paste(unique(input$nivel2),collapse = ', ')),
      "<br><b>Situação das patentes:</b>",ifelse(length(input$status)==4,"Todos os status",paste(unique(input$status),collapse = ', ')),
      #"<br><b>Origem do pedido:</b>",paste(unique(input$pct),collapse = ','),
      "<br><b>",ifelse(input$tp_origem=="Internacional","País(es):","UF(s):"),"</b>",ifelse(input$tp_origem=="Nacional" & length(input$uf)==28,"Todas as UFs",
                                                                                            ifelse(input$tp_origem=="Nacional" & length(input$uf)<28,paste(unique(input$uf),collapse = ","),
                                                                                                   ifelse(input$tp_origem=="Internacional" & length(input$nacionalidade)==110,"Todos os países",
                                                                                                          paste(unique(input$nacionalidade),collapse = ", ")))),
      "<br><b>Tipo:</b>",paste(unique(input$tipo),collapse = ', '),
      "</br>",
      "<p></p>",
      "<b>Observação: </b>Estes números representam a contagem do número de patentes, ou seja, 
      não há dupla contagem quando uma patente se encontra em mais de uma categoria tecnológica ou mais de um país/UF.",
      fmt2)
    
    HTML(htmlText)
  })
  
  output$title_plot1 <- renderUI({
    
    if(input$tp_plot1!="Download"){
      htmlText = paste0("Número de patentes depositadas por ano do depósito, ", min(input$date)," a ",max(input$date))
    }
    
    HTML(paste0("<div style='font-family: Calibri, sans-serif;font-size: 26px';>",htmlText,"</div>"))
  })
  
  output$title_plot2 <- renderUI({
    
    
    if(input$tp_plot2!="Download"){
      if(input$tp_plot2=="Barras" | input$tp_plot2=="Linhas" | input$tp_plot2=="Tabela"){
        htmlText = paste0("Número de patentes depositadas segundo classificação tecnológica por ano do depósito, ",min(input$date)," a ",max(input$date))
        
      } else if(input$tp_plot2=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas segundo classificação tecnológica, ",
                          min(input$date)," a ",max(input$date))
        
      }
      HTML(paste0("<div style='text-align:center;font-family: Calibri, sans-serif;font-size: 26px';>",htmlText,"</div>"))
    }
  })
  
  output$title_plot3 <- renderUI({
    
    
    if(input$tp_plot3!="Download"){
      if(input$tp_plot3=="Barras" | input$tp_plot3=="Linhas" | input$tp_plot3=="Tabela"){
        htmlText = paste0("Número de patentes depositadas segundo status por ano do depósito, ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot3=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas segundo status, ",min(input$date)," a ",max(input$date))
      }
      HTML(paste0("<div style='font-family: Calibri, sans-serif;font-size: 26px';>",htmlText,"</div>"))
    }
  })
  
  output$title_plot41 <- renderUI({
    
    if(input$tp_plot41!="Download"){
      if(input$tp_plot41=="Barras" | input$tp_plot41=="Linhas"){
        htmlText = paste0(ifelse(input$select41=="Número absoluto","Número de patentes ","Distribuição proporcional das patentes "),
                          "de depositantes com nacionalidade ",
                          ifelse(input$local=="Brasil","brasileira","de algum país das Américas")," por ano do depósito, ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot41=="Tabela"){
        htmlText = paste0("Número de patentes de depositantes com nacionalidade ",
                          ifelse(input$local=="Brasil","brasileira","de algum país das Américas")," por ano do depósito, ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot41=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes de depositantes com nacionalidade ",
                          ifelse(input$local=="Brasil","brasileira","de algum país das Américas"),", ",min(input$date)," a ",max(input$date))
      }
      
      HTML(paste0("<div style='font-family: Calibri, sans-serif;font-size: 26px';>",htmlText,"</div>"))
    }
    
  })
  
  output$title_plot42 <- renderUI({
    ano<-"do depósito"
    
    if(input$tp_plot42!="Download"){
      if(input$tp_plot42=="Barras"){
        htmlText = paste0(ifelse(input$select42=="Número absoluto","Número de patentes depositadas por pessoa física por ano ",
                                 "Distribuição proporcional das patentes depositadas por tipo de pessoa (física e não física) por ano "),
                          ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot42=="Linhas"){
        htmlText = paste0("Número de patentes depositadas por pessoa física por ano ",
                          ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot42=="Tabela"){
        htmlText = paste0("Número de patentes depositadas por tipo de pessoa (física e não física) por ano ",
                          ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot42=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes depositadas por tipo de pessoa (física e não física), ",min(input$date)," a ",max(input$date))
      }

      HTML(paste0("<div style='font-family: Calibri, sans-serif;font-size: 26px';>",htmlText,"</div>"))
    }

  })
  
  output$title_plot6 <- renderUI({
    ano<-"do depósito"
    
    if(input$tp_plot6!="Download"){
      if(input$tp_plot6=="Barras"){
        htmlText = paste0(ifelse(input$select6=="Proporção",
                                 paste0("Distribuição proporcional das patentes de depositantes brasileiros com cooperação ",input$coopera," por ano "),
                                 paste0("Número de patentes de depositantes brasileiros com cooperação ",input$coopera," por ano ")),
                          ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot6=="Tabela" | input$tp_plot6=="Linhas"){
        htmlText = paste0("Número de patentes de depositantes brasileiros com cooperação ",input$coopera," por ano ",
                          ano,", ",min(input$date)," a ",max(input$date))
      } else if(input$tp_plot6=="Setor"){
        htmlText = paste0("Distribuição proporcional das patentes de depositantes brasileiros com cooperação ", input$coopera,", ",min(input$date)," a ",max(input$date))
      }
      
      HTML(paste0("<div style='font-family: Calibri, sans-serif;font-size: 26px';>",htmlText,"</div>"))
    }
    
  })
  
  output$plot1 <- renderGirafe({
    
    db1=database() %>%
      group_by(date=ano) %>%
      summarise(count=sum(count))
    
    
    xTitle="Ano do depósito"
    yTitle="Número de patentes"
    
    if(input$tp_plot1=="Barras"){
      p=ggplot(db1,aes(x=date,y=count))+
        geom_bar_interactive(stat='identity',aes(tooltip=count),fill="#005266")+
        labs(x=xTitle,y=yTitle)+
        theme_classic()+
        theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              legend.text=element_text(size = 20),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(fill=guide_legend(nrow=1,byrow=TRUE))+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
   
    } else if(input$tp_plot1=="Linhas"){
      p=ggplot(db1,aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle)+
        theme_classic()+
        theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
              axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              legend.text=element_text(size = 20),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(fill=guide_legend(nrow=1,byrow=TRUE))+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))

    }
    
    girafe(ggobj = p,width_svg = 20,height_svg = 8)
  })
  
  output$plot2 <- renderGirafe({
    
    db=database2()
    
    db$classif=switch(input$nivel,
      "Nível 1" = db$nivel1,
      "Nível 2" = db$nivel2
    )
    
    colors=switch (input$nivel,
      "Nível 1" = c("Eficiência Energética"="#E49B64","Fontes de Energia Renováveis"="#498399",
                    "Fissão e Fusão Nuclear"="#8A3D39","Hidrogênio e Células a Combustível"="#B0A3C4",
                    "Outras Tecnologias de Geração e Armazenamento de Energia"="#2B651F"),
      
      "Nível 2" = c("Tecnologias de Eficiência Energética aplicadas à Industria"="#2f1335",                             
                    "Tecnologias de Eficiência Energética aplicada a residências e estabelecimentos comerciais"="#a6cee3",
                    "Tecnologias de Eficiência Energética aplicadas ao setor de transportes"="#1f78b4",
                    "Outras Tecnologias de Eficiência Energética"="#b2df8a",
                    "Energia solar"="#33a02c",
                    "Energia Eólica"="#fb9a99",
                    "Energia dos Oceanos"="#e31a1c",
                    "Biocombustíveis"="#5c1b35",
                    "Energia Geotérmica"="#fdbf6f",
                    "Hidroeletricidade"="#cab2d6",
                    "Fissão Nuclear"="#6a3d9a",
                    "Fusão Nuclear"="#c59538",
                    "Outros fusão e fissão não alocados"="#ffff99",
                    "Células a Combustível"="#b15928",
                    "Geração de energia elétrica"="#fddf2f",
                    "Armazenamento de Energia"="#71dbd2")
    )
    
    
    db1=db %>%
      group_by(date=ano,cat=classif) %>%
      summarise(count=sum(count))
    
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle="Ano do depósito"
    
    if(input$tp_plot2=="Barras"){
      yTitle=switch (input$select2,
        "Número absoluto" = "Número de patentes",
        "Proporção" = "Proporção"
      )
      
      if(input$select2=="Número absoluto"){
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position="stack",aes(tooltip=paste0(cat,": ",count)))
      } else if(input$select2=="Proporção"){
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position="fill",aes(tooltip=paste0(cat,": ",per)))
      }    
      #p=ggplot(db1,aes(x=date,y=count,fill=cat))+
      #  geom_bar_interactive(stat='identity',position=ifelse(input$select2=="Número absoluto","stack","fill"),aes(tooltip=paste0(cat,": ",ifelse(input$select2=="Número absoluto",count,per))))+
        p=p+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "bottom")+
          theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
                axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
                axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                legend.text=element_text(size = 20),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=12),
                strip.text = element_text(size=16),
                axis.line.y = element_blank())+
          guides(fill=guide_legend(nrow=ifelse(input$nivel=="Nível 1",2,8),byrow=TRUE))+
          scale_fill_manual("",values = colors)+
          scale_y_continuous(labels = ifelse(input$select2=="Número absoluto",scales::number,scales::percent),expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot2=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1,aes(x=date,y=count,color=cat))+
        geom_line(size=1.1)+
        geom_point_interactive(aes(tooltip=count))+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
              axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              legend.text=element_text(size = 20),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=ifelse(input$nivel=="Nível 1",2,8),byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot2=="Setor"){
      
      db1=db %>%
        group_by(cat=classif) %>%
        summarise(count=sum(count))
      
      db1$per=db1$count/sum(db1$count)
      
      db1$ymax=cumsum(db1$per)
      db1$ymin=c(0,head(db1$ymax,n=-1))
      
      p=ggplot(db1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = colors)+
        scale_fill_manual("",values = colors)+
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme_void()+
        theme(legend.text=element_text(size = 16),
              text=element_text(size = 20),
              title = element_text(size = 16),
              plot.caption = element_text(size=12),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=ifelse(input$nivel=="Nível 1",2,8),byrow=TRUE))+
        theme(legend.position = "right")
      
    }
    girafe(ggobj = p,width_svg = 20,height_svg = 8)
  })
  
  output$plot3 <- renderGirafe({
    
    db1=database() %>% 
      group_by(date=ano,cat=status_atual) %>% 
      summarise(count=sum(count)) %>% 
      group_by(date) %>% 
      mutate(total=sum(count)) %>% 
      mutate(per=round(count/total*100,1))
    
    colors=c("Vigente"="#a6cee3","Não vigente"="#1f78b4","Pendente"="#b2df8a",
             "Extinta"="#fb9a99","Não válida"="#999999")
    
    xTitle="Ano do depósito"
    
    if(input$tp_plot3=="Barras"){
      
      yTitle=switch (input$select3,
        "Número absoluto" = "Número de patentes",
        "Proporção" = "Proporção"
      )
      
      if(input$select3=="Número absoluto"){
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position="stack",aes(tooltip=paste0(cat,": ",count)))
      } else if(input$select3=="Proporção"){
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position="fill",aes(tooltip=paste0(cat,": ",per)))
      }
      p=p+
        labs(x=xTitle,y=yTitle,fill="")+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
              axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              legend.text=element_text(size = 20),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(fill=guide_legend(nrow=1,byrow=TRUE))+
        scale_fill_manual("",values = colors)+
        scale_y_continuous(labels = ifelse(input$select3=="Número absoluto",scales::number,scales::percent),expand=c(0,0.05))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot3=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1,aes(x=date,y=count,color=cat))+
        geom_line(size=1.1)+
        geom_point_interactive(aes(tooltip=count))+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
              axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              legend.text=element_text(size = 20),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot3=="Setor"){
      db=database() %>% 
        group_by(cat=status_atual) %>% 
        summarise(count=sum(count)) %>% 
        mutate(per = count/sum(count))
      
      db$ymax=cumsum(db$per)
      db$ymin=c(0,head(db$ymax,n=-1))
      
      p=ggplot(db, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = colors)+
        scale_fill_manual("",values = colors)+
        coord_polar(theta="y")+
        xlim(c(2, 4))+
        theme_void()+
        theme(legend.text=element_text(size = 16),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=1,byrow=TRUE))
        theme(legend.position = "right")
      
    }
    girafe(ggobj = p,width_svg = 20,height_svg = 8)
  })
  
  output$plot41 <- renderGirafe({
    
    db=database()
    
    db$cat=switch(input$local,
                  "Brasil" = db$brasil,
                  "América" = db$america
    )
    
    db1<-db %>% 
      group_by(date=ano,cat=cat) %>% 
      summarise(count=sum(count)) %>% 
      group_by(date) %>% 
      mutate(total = sum(count)) %>% 
      mutate(per = round(count/total*100,1))
    
    xTitle="Ano do depósito"
    
    if(input$tp_plot41=="Barras"){
      if(input$select41=="Número absoluto"){
        yTitle="Número de patentes"
        
        p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
          geom_bar_interactive(stat='identity',fill="#005266",aes(tooltip=count))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
                axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
                axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                legend.text=element_text(size = 20),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16),
                axis.line.y = element_blank())+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
        
        
        
      } else if(input$select41=="Proporção"){
        yTitle="Proporção"
        
        db1$cat=factor(db1$cat,levels=c(1,0),labels=c(ifelse(input$local=="Brasil","Brasil","América"),
                                                      ifelse(input$local=="Brasil","Outros países","Outros continentes")))
        
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position=position_fill(reverse = T),aes(tooltip=paste0(cat,": ",per,"%")))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "bottom")+
          theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
                axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
                axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                legend.text=element_text(size = 20),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16),
                axis.line.y = element_blank())+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_fill_manual("",values=c("#005266","#7197A4"))+
          scale_y_continuous(labels = scales::percent,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      }
      
      
    } else if(input$tp_plot41=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
              axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              legend.text=element_text(size = 20),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1[which(db1$cat==1),]$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot41=="Setor"){
      db1=db %>%
        group_by(cat) %>% 
        summarise(count=sum(count))
      
      db1$per=db1$count/sum(db1$count)
      
      db1$ymax=cumsum(db1$per)
      db1$ymin=c(0,head(db1$ymax,n=-1))
      
      db1$cat=factor(db1$cat,levels=c(1,0),labels=c(input$local,ifelse(input$local=="Brasil","Outros países","Outros continentes")))
      
      p=ggplot(db1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = c("#005266","#7197A4"))+
        scale_fill_manual("",values = c("#005266","#7197A4"))+
        coord_polar(theta="y")+
        labs(fill="",color="")+
        xlim(c(2, 4))+
        theme_void()+
        theme(legend.text=element_text(size = 16),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        theme(legend.position = "right")
      
    }
    
    girafe(ggobj = p,width_svg = 20,height_svg = 8)
  })
  
  output$plot42 <- renderGirafe({
    
    db1=database() %>% 
      group_by(date=ano,cat=tp_pessoa) %>% 
      summarise(count=sum(count)) %>% 
      group_by(date) %>% 
      mutate(total = sum(count)) %>% 
      mutate(per = round(count/total*100,1))
    
    db1$per=NA
    for(i in unique(db1$date)){
      db1[db1$date==i,]$per=round(db1[db1$date==i,]$count/sum(db1[db1$date==i,]$count)*100,1)
    }
    
    xTitle="Ano do depósito"
    
    if(input$tp_plot42=="Barras"){
      if(input$select42=="Número absoluto"){
        yTitle="Número de patentes"
        
        p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
          geom_bar_interactive(stat='identity',fill="#005266",aes(tooltip=count))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
                axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
                axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                legend.text=element_text(size = 20),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16),
                axis.line.y = element_blank())+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
        
        
        
      } else if(input$select42=="Proporção"){
        yTitle="Proporção"
        
        db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c("Pessoa física","Não pessoa física","Sem informação de país"))
        
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position=position_fill(reverse = F),aes(tooltip=paste0(cat,": ",per,"%")))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "bottom")+
          theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
                axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
                axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                legend.text=element_text(size = 20),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16),
                axis.line.y = element_blank())+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_fill_manual("",values=c("#005266","#7197A4","grey70"))+
          scale_y_continuous(labels = scales::percent,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      }
      
      
    } else if(input$tp_plot42=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
              axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              legend.text=element_text(size = 20),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1[which(db1$cat==1),]$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot42=="Setor"){
      
      db2=database() %>% 
        group_by(cat=tp_pessoa) %>% 
        summarise(count=sum(count))
      
      db2$per=db2$count/sum(db2$count)
      
      db2$ymax=cumsum(db2$per)
      db2$ymin=c(0,head(db2$ymax,n=-1))
      
      db2$cat=factor(db2$cat,levels=c(1,0,9),labels=c("Pessoa física","Não pessoa física","Sem informação de país"))
      
      p=ggplot(db2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = c("#005266","#7197A4","grey70"))+
        scale_fill_manual("",values = c("#005266","#7197A4","grey70"))+
        coord_polar(theta="y")+
        labs(fill="",color="")+
        xlim(c(2, 4))+
        theme_void()+
        theme(legend.text=element_text(size = 16),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        theme(legend.position = "right")
      
    }
    
    girafe(ggobj = p,width_svg = 20,height_svg = 8)
  })
  
  output$plot6 <- renderGirafe({
    
    db=database()
    db$coopera=switch(input$coopera,
                      "nacional" = db$cooper_nac,
                      "internacional" = db$cooper_inter
    )
    
    db1=db %>% 
      filter(brasil==1) %>% 
      group_by(date=ano,cat=coopera) %>% 
      summarise(count=sum(count)) %>% 
      mutate(total = sum(count)) %>% 
      mutate(per = round(count/total*100,1))
    
    xTitle="Ano do depósito"
    
    if(input$tp_plot6=="Barras"){
      if(input$select6=="Número absoluto"){
        yTitle="Número de patentes"
        
        p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
          geom_bar_interactive(stat='identity',fill="#005266",aes(tooltip=count))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
                axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
                axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                legend.text=element_text(size = 20),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16),
                axis.line.y = element_blank())+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_y_continuous(labels = scales::number,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
        
        
        
      } else if(input$select6=="Proporção"){
        yTitle="Proporção"
        
        db1$cat=factor(db1$cat,levels=c(1,0,9),labels=c(paste0("Com cooperação ",input$coopera),paste0("Sem cooperação ",input$coopera),"Sem informação"))
        
        p=ggplot(db1,aes(x=date,y=count,fill=cat))+
          geom_bar_interactive(stat='identity',position=position_fill(reverse = T),aes(tooltip=paste0(cat,": ",per,"%")))+
          labs(x=xTitle,y=yTitle,fill="")+
          theme_classic()+
          theme(legend.position = "bottom")+
          theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
                axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
                axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
                legend.text=element_text(size = 20),
                text=element_text(size = 16),
                title = element_text(size = 16),
                plot.caption = element_text(size=16),
                strip.text = element_text(size=16),
                axis.line.y = element_blank())+
          guides(fill=guide_legend(nrow=1,byrow=TRUE))+
          scale_fill_manual("",values=c("#005266","#7197A4","grey70"))+
          scale_y_continuous(labels = scales::percent,expand=c(0,0.05))+
          scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      }
      
      
    } else if(input$tp_plot6=="Linhas"){
      yTitle="Número de patentes"
      
      p=ggplot(db1[which(db1$cat==1),],aes(x=date,y=count))+
        geom_line(color="#005266",size=1.1)+
        geom_point_interactive(aes(tooltip=count),color="#005266")+
        labs(x=xTitle,y=yTitle,color=NULL)+
        theme_classic()+
        theme(legend.position = "bottom")+
        theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain",family="Calibri, sans-serif"),  
              axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain",family="Calibri, sans-serif"),
              axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain",family="Calibri, sans-serif"),
              legend.text=element_text(size = 20),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        scale_color_manual("",values = colors)+
        scale_y_continuous(labels = scales::number,expand=c(0,0.05),limits = c(0,max(db1[which(db1$cat==1),]$count)))+
        scale_x_continuous(breaks = seq(min(input$date),max(input$date),1))
      
    } else if(input$tp_plot6=="Setor"){
      
      db2=db %>% 
        filter(brasil==1) %>% 
        group_by(cat=coopera) %>% 
        summarise(count=sum(count))
      
      db2$per=db2$count/sum(db2$count)
      
      db2$ymax=cumsum(db2$per)
      db2$ymin=c(0,head(db2$ymax,n=-1))
      
      db2$cat=factor(db2$cat,levels=c(1,0,9),labels=c(paste0("Com cooperação ",input$coopera),paste0("Sem cooperação ",input$coopera),"Sem informação"))
      
      p=ggplot(db2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=cat))+
        geom_rect_interactive(aes(tooltip=paste0(cat,": ",round(per*100,1),"%")))+
        scale_color_manual("",values = c("#005266","#7197A4","grey70"))+
        scale_fill_manual("",values = c("#005266","#7197A4","grey70"))+
        coord_polar(theta="y")+
        labs(fill="",color="")+
        xlim(c(2, 4))+
        theme_void()+
        theme(legend.text=element_text(size = 16),
              text=element_text(size = 16),
              title = element_text(size = 16),
              plot.caption = element_text(size=16),
              strip.text = element_text(size=16),
              axis.line.y = element_blank())+
        guides(color=guide_legend(nrow=1,byrow=TRUE))+
        theme(legend.position = "right")
      
    }
    
    girafe(ggobj = p,width_svg = 20,height_svg = 8)
  })
  
  output$tab1 <- DT::renderDataTable({
    tab=database() %>% 
      group_by(ano) %>% 
      summarise(count=sum(count))
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),"n")
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word")
    
    
    print(my.table)
  })
  
  output$tab2 <- DT::renderDataTable({
    db=database2()
    
    db$classif=switch(input$nivel,
                      "Nível 1" = db$nivel1,
                      "Nível 2" = db$nivel2
    )
    
    tab=db %>%
      group_by(date=ano,cat=classif) %>%
      summarise(count=sum(count))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word")
    
    
    print(my.table)
  })
  
  output$tab3 <- DT::renderDataTable({
    tab=database() %>% 
      group_by(date=ano,cat=status_atual) %>% 
      summarise(count=sum(count))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word")
    
    
    print(my.table)
  })
  
  output$tab41 <- DT::renderDataTable({
    db=database()
    db$cat=switch(input$local,
                  "Brasil" = db$brasil,
                  "América" = db$america
    )
    
    tab=db %>% 
      group_by(date=ano,cat=cat) %>% 
      summarise(count=sum(count))
    
    tab$cat=switch(input$local,
                   "Brasil" = ifelse(tab$cat==1,"Brasil",ifelse(tab$cat==0,"Outro país","Sem informação")),
                   "América" = ifelse(tab$cat==1,"América",ifelse(tab$cat==0,"Outro continente","Sem informação"))
    )
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word")
    
    
    print(my.table)
  })
  
  output$tab42 <- DT::renderDataTable({
    
    tab=database() %>% 
      group_by(date=ano,cat=tp_pessoa) %>% 
      summarise(count=sum(count))
    
    tab$cat=factor(tab$cat,levels=c(1,0,9),labels=c("Pessoa física","Não pessoa física","Sem informação"))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word")
    
    
    print(my.table)
  })
  
  output$tab6 <- DT::renderDataTable({
    
    db=database()
    db$coopera=switch(input$coopera,
      "nacional" = db$cooper_nac,
      "internacional" = db$cooper_inter
    )
    
    tab=db %>% 
      filter(brasil==1) %>% 
      group_by(date=ano,cat=coopera) %>% 
      summarise(count=sum(count))
    
    tab$cat=factor(tab$cat,levels=c(1,0,9),labels=c(paste0("Com cooperação ",input$coopera),paste0("Sem cooperação ",input$coopera),"Sem informação"))
    
    tab=reshape2::dcast(tab,date~cat,value.var = "count")
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    header.names <- c(paste0("Ano",ifelse(input$tp_ano=="Concessão"," da "," do "),input$tp_ano),
                      names(tab)[-1])
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, rownames = F, width = '100%', extensions = 'Buttons') %>%
      formatStyle(columns = c(2:ncol(tab)),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word")
    
    
    print(my.table)
  })
  
  output$tab7 <- DT::renderDataTable({
    
    tab=database3()
    tab=tab[,c("numero_pedido","titulo","depositantes","ano","status_atual")]
    tab=as.data.frame(cbind(tab,View = shinyInput(actionButton, nrow(tab),'button_', label = icon("magnifying-glass-plus"), onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )))
    
    
    my.options <- list(autoWidth = FALSE,
                       searching = TRUE,
                       ordering = TRUE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       buttons = c('copy', 'csv', 'excel', 'pdf'),
                       rowsGroup = list(0))
    
    header.style <- "th { font-family: 'Calibri'; font-size:16px ;font-weight: bold; color: white; background-color: #005266;}"
    
    #header.names <- c("ID","Título","Ano de pedido","Situação atual")
    header.names <- c("Número do pedido","Título","Depositante","Ano","Status","Mais informações")
    
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: left; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    
    my.table <- datatable(tab, options = my.options, container = my.container, escape = FALSE, rownames = F, width = '100%', extensions = 'Buttons',selection = 'single') %>%
      formatStyle(columns = c(1:ncol(tab)),
                  width = '100px',
                  fontFamily = "Calibri",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word")
    
    
    print(my.table)
  })
  
  output$popup <- renderUI({
    print(input$select_button)
    db1=database3()
    
    tagList(
      bsModal("detalhes", "Mais informações", trigger = "a",
              HTML(paste(
                "<strong>Título:</strong>",db1$titulo[SelectedRow()],
                "<br><strong>Resumo:</strong>",db1$resumo[SelectedRow()],
                "<br><strong>País de origem:</strong>",db1$pais[SelectedRow()],
                #"<br><strong>Inventores:</strong>",db1$invent[SelectedRow()],
                "<br><strong>Categoria IEA:</strong>",db1$nivel2[SelectedRow()],
                "<br><strong>Lista classificação IPC:</strong>",db1$ipc[SelectedRow()]))
              
      ),actionButton("a", "Show modal")
    )
  })
  
  output$data1 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format1)
    },
    
    content = function(file) {
      
      df=database() %>%
        group_by(ano) %>%
        summarise(n=sum(count))
      
      # wb <- createWorkbook()
      # addWorksheet(wb, "Tabela")
      # 
      # hs1 <- createStyle(fgFill = "#215264", halign = "CENTER", textDecoration = "Bold",
      #                    border = "Bottom", fontColour = "white")
      # 
      # writeData(wb, 1, x = paste(str_glue("Tabela. Número de patentes por ano. {min(df$ano)} a {max(df$ano)}")), startRow = 1, startCol = 1)
      # writeData(wb, 1, x=df, startRow = 2, startCol = 1, headerStyle = hs1)
      # writeDataTable(wb, 1, x=df, startRow = 2, startCol = 1, tableStyle = "TableStyleMedium21",withFilter = F)
      # writeData(wb, 1, x = "Nota: Este dado refere-se a contagem do número de patentes ano a ano.", startRow = (nrow(df)+3), startCol = 1)
      
      switch (input$format1,
        ".csv" = write.csv2(df, file,row.names = FALSE),
        ".xlsx" = write.xlsx(df,file)
        #".xlsx" = openxlsx::saveWorkbook(wb, file,overwrite = T)
      )
      
    }
  )
  
  output$data2 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format2)
    },
    
    content = function(file) {
      
      db=database2()
      
      db$classif=switch(input$nivel,
                        "Nível 1" = db$nivel1,
                        "Nível 2" = db$nivel2
      )
      
      df=db %>%
        group_by(Ano=ano,Categoria=classif) %>%
        summarise(n=sum(count))
      
      switch (input$format2,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$data3 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format3)
    },
    
    content = function(file) {
      
      df=database() %>% 
        group_by(Ano=ano,Status=status_atual) %>% 
        summarise(n=sum(count))
      
      switch (input$format3,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$data4.1 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format4.1)
    },
    
    content = function(file) {
      
      db=database()
      
      db$cat=switch(input$local,
                    "Brasil" = db$brasil,
                    "América" = db$america
      )
      
      df=db %>% 
        group_by(Ano=ano,Categoria=cat) %>% 
        summarise(n=sum(count))
      
      df$Categoria=switch(input$local,
                          "Brasil" = ifelse(df$Categoria==1,"Brasil",ifelse(df$Categoria==0,"Outro país","Sem informação")),
                          "América" = ifelse(df$Categoria==1,"América",ifelse(df$Categoria==0,"Outro continente","Sem informação"))
      )
      
      switch (input$format4.1,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$data4.2 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format4.2)
    },
    
    content = function(file) {
      
      df=database() %>% 
        group_by(Ano=ano,Categoria=tp_pessoa) %>% 
        summarise(n=sum(count))
      
      df$Categoria=ifelse(df$Categoria==1,"Pessoa física",ifelse(df$Categoria==0,"Não pessoa física","Sem informação"))
      
      switch (input$format4.2,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  
  output$data6 <- downloadHandler(
    
    filename = function() {
      paste0("data",input$format6)
    },
    
    content = function(file) {
      db=database()
      db$coopera=switch(input$coopera,
                        "nacional" = db$cooper_nac,
                        "internacional" = db$cooper_inter
      )
      
      df=db %>% 
        filter(brasil==1) %>% 
        group_by(Ano=ano,Cooperação=coopera) %>% 
        summarise(count=sum(count))
      
      df$Cooperação=factor(df$Cooperação,levels=c(1,0,9),labels=c(paste0("Com cooperação ",input$coopera),paste0("Sem cooperação ",input$coopera),"Sem informação"))
      
      
      switch (input$format6,
              ".csv" = write.csv2(df, file,row.names = FALSE),
              ".xlsx" = write.xlsx(df,file)
      )
    }
  )
  
  output$pdf <- downloadHandler(
    filename = function() {
      "Nota metodológica.pdf"
    },
    content = function(file) {
      file.copy(file.path("Nota Metodológica.pdf"), file)
    }
  )
}

shinyApp(ui, server)

#runApp(list(ui = ui, server = server))



