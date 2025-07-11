library(shiny) 
library(shinydashboard)
library(vembedr)

header <- dashboardHeader(title = "Media Pembelajaran Kimia")
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Peta Konsep", tabName = "youtube", icon = icon("table")
    ),
    menuItem("Tujuan Pembelajaran", tabName = "tujuan_pembelajaran", icon = icon("list-alt")
    ),
    menuItem("Asam dan Basa", tabName = "hasil", icon = icon("th-list"),
             menuSubItem("Materi-1", tabName = "Materi_1"),
             menuSubItem("Materi-2", tabName = "Materi_2"),
             menuSubItem("Materi-3", tabName = "Materi_3")
    ),       
    menuItem("Praktikum", icon = icon("th-list"),
             menuSubItem("Simulasi Praktikum-1", tabName = "Praktikum_1"),
             menuSubItem("Simulasi Praktikum-2", tabName = "Praktikum_2"),
             menuSubItem("Simulasi Praktikum-3", tabName = "Praktikum_3")
    ),
    menuItem("Quiz Teka Teki", icon = icon("th-list"),
             menuSubItem("Teka Teki-1", tabName = "Teka_Teki_1"),
             menuSubItem("Teka Teki-2", tabName = "Teka_Teki_2"),
             menuSubItem("Teka Teki-3", tabName = "Teka_Teki_3")
    ),
     menuItem("Quiz Googleform", icon = icon("th-list"),
             menuSubItem("Googleform-1", tabName = "Googleform_1"),
             menuSubItem("Googleform-2", tabName = "Googleform_2"),
             menuSubItem("Googleform-3", tabName = "Googleform_3")         
    )
  ))


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "youtube", 
            (box(title = "Peta Konsep", width = 12, imageOutput("image1"))
            )),
    tabItem(tabName = "tujuan_pembelajaran",
            (box(title = "Tujuan Pembelajaran", width = 12 ,imageOutput("image"))
            )),
    tabItem(tabName = "Hasil_Model_1_Parameter",
            (box(title = "Hasil Model 1 Parameter", width = 12,verbatimTextOutput("hasil1PL"))
            )),
    tabItem(tabName = "Materi_1",
            (box(title = "Materi", width = 12,verbatimTextOutput("hasil2PL"))        
            )),
    tabItem(tabName = "Materi_2",
            (box(title = "Materi", width = 12,verbatimTextOutput("hasil3PL"))
            )),
    tabItem(tabName = "Praktikum_1",  
            (box(title = "Simulasi Praktikum-1", width = 12, htmlOutput("youtube1"))
            )),
    tabItem(tabName = "Praktikum_2",
            (box(title = "Simulasi Praktikum-2", width = 12, htmlOutput("youtube2"))
            )), 
    tabItem(tabName = "Praktikum_3",
            (box(title = "Simulasi Praktikum-3", width = 12, htmlOutput("youtube3"))
            )),
    tabItem(tabName = "Teka_Teki_1",  
            (box(title = "Teka Teki-1", width = 12, htmlOutput("tekateki1"))
            )),
    tabItem(tabName = "Teka_Teki_2",
            (box(title = "Teka Teki-2", width = 12, htmlOutput("tekateki2"))
            )), 
    tabItem(tabName = "Teka_Teki_3",
            (box(title = "Teka Teki-3", width = 12, htmlOutput("tekateki3"))
            )),
    tabItem(tabName = "Googleform_1",  
            (box(title = "Googleform-1", width = 12, htmlOutput("Googleform1"))
            )),
    tabItem(tabName = "Googleform_2",
            (box(title = "Googleform-2", width = 12, htmlOutput("Googleform2"))
            )), 
    tabItem(tabName = "Googleform_3",
            (box(title = "Googleform-3", width = 12, htmlOutput("Googleform3"))
            ))
  ))


shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    output$image1<-renderImage({ 
      list(height =550, width = 1000, src="Konsep.png")
    }) 
    output$image<-renderImage({
      list(height =550, width = 1000, src="Tujuan Pembelajaran.png")
    }) 
    output$hasil1PL <-renderPrint({
      data <- data()
      rasch <- rasch(data=data)
      print(rasch)
    })
    output$hasil2PL <-renderPrint({
      data <- data()
      model2pl <- ltm(data~z1)
      print(model2pl)
    })
    output$hasil3PL <-renderPrint({
      data <- data()
      model3pl <- tpm(data=data)
      print(model3pl)
    })
    output$youtube1<-renderUI({ 
      HTML('<iframe src="https://phet.colorado.edu/sims/html/acid-base-solutions/latest/acid-base-solutions_in.html"
           width="1000"
           height="550"
           allowfullscreen>
           </iframe>')
    })
    output$youtube2<-renderUI({ 
      HTML('<iframe src="https://phet.colorado.edu/sims/html/ph-scale-basics/latest/ph-scale-basics_en.html"
        width="1000"
           height="550"
           allowfullscreen>
           </iframe>')
    })
    output$youtube3<-renderUI({ 
      HTML('<iframe allow FullScreen="true" width="1000px" height="550px" src="https://viewer.assemblrworld.com/Viewer/-9VwWn9CR6WLSVq6XLrK?" frameborder="0"></iframe>')
    })
    output$tekateki1<-renderUI({ 
      HTML('<iframe width="1000" height="550" style="background-color:white; padding:5px 0 0 5px; border:3px solid black; margin:auto; display:block" frameborder="0" src="https://crosswordlabs.com/embed/teka-teki-kimia-redoks-2"></iframe>')
    })
    output$tekateki2<-renderUI({ 
      HTML('<iframe width="1000" height="550" style="background-color:white; padding:5px 0 0 5px; border:3px solid black; margin:auto; display:block" frameborder="0" src="https://crosswordlabs.com/embed/teka-teki-kimia-40"></iframe>')
    })
    output$Googleform1<-renderUI({ 
      HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSfMKZS8-LxzfFKaBE6Qrv4AQFf12bq646TWVDHa-0DvE9qWjw/viewform?embedded=true" width="1000" height="17804" frameborder="0" marginheight="0" marginwidth="0">Memuat…</iframe>')
    })
    output$Googleform2<-renderUI({ 
      HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLScXY0ub9rX17SO8uwBmaReL1d1umwQmVYzRIsueN28SvdNaZg/viewform?embedded=true" width="1000" height="16785" frameborder="0" marginheight="0" marginwidth="0">Memuat…</iframe>')
    })
  })

