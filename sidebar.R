sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(id="tab",
    menuItem("Evolução das patentes", tabName = "evolucao", icon = icon("chart-line")),
    menuItem("Classificação tecnológica", tabName = "categoria", icon = icon("bars")),
    menuItem("Situação das patentes", tabName = "status", icon = icon("check")),
    menuItem("Perfil do depositante", tabName = "origem", icon = icon("flask")),
    menuItem("Cooperação", tabName = "cooperacao", icon = icon("globe")),
    menuItem("Explorar patentes", tabName = "explorar", icon = icon("magnifying-glass"))
  )
)




