
explorar<-tabItem(tabName = "explorar",
  box(width = 12,
    div(style = "overflow:scroll;header:fixed", DT::dataTableOutput("tab7",height=600)),
    uiOutput("popup")
  )
)
     