sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(id="tab",
    menuItem("Evolução das patentes", tabName = "evolucao", icon = icon("chart-line")),
    menuItem("Classificação tecnológica", tabName = "categoria", icon = icon("bars")),
    menuItem("Situação das patentes", tabName = "status", icon = icon("check")),
    menuItem("Perfil do depositante", icon = icon("flask"),
      menuSubItem("Origem",tabName = "origem",icon=icon("globe")),
      menuSubItem("Tipo de pessoa",tabName = "tp_pessoa",icon=icon("user"))
    ),
    menuItem("Perfil do inventor", tabName = "inventor", icon = icon("user-gear")),
    menuItem("Cooperação", tabName = "cooperacao", icon = icon("globe")),
    menuItem("Explorar patentes", tabName = "explorar", icon = icon("magnifying-glass"))
  )
)




