sidebar <- dashboardSidebar(
  width = 350,
  useShinyjs(),
  sidebarMenu(id="tab",
              menuItem("Evolução das patentes", tabName = "evolucao", icon = icon("chart-line")),
              menuItem("Classificação tecnológica", tabName = "categoria", icon = icon("bars")),
              menuItem("Situação das patentes", tabName = "status", icon = icon("check")),
              menuItem("Cooperação", tabName = "cooperacao", icon = icon("globe")),
              menuItem("Explorar patentes", tabName = "explorar", icon = icon("magnifying-glass")),
              downloadButton("pdf", label = "Nota metodológica", class = NULL,icon = icon("book"),
                             style = "color: #fff; background-color: #F0BB47; border-color: #F0BB47;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; ")
  )
)