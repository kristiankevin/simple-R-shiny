# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

banjirdat <- read.csv("od_jumlah_kejadian_bencana_banjir.csv") %>% select(nama_kabupaten_kota,tahun,jumlah_bencana)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Dashboard")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)

frowhead <- fluidRow(
  h2("Kasus Bencana Banjir di Jawa Barat tahun 2012-2020")
  
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "Kasus per Tahun"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("plottahun", height = "300px")
  )
  
  ,box(
    title = "Kasus per Tahun"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("boxplottahun", height = "300px")
  ) 
  
)

frow3 <- fluidRow(
  box(
    title = "Kasus per daerah"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,width = 12
    ,plotOutput("bardaerah", height = "450px")
  )
)



# combine the two fluid rows to make the body
body <- dashboardBody(frowhead, frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Kasus Bencana Banjir di Jawa Barat 2012-2020', header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  total.kasus <- sum(banjirdat$jumlah_bencana)
  kasus.by.daerah <- banjirdat %>% group_by(nama_kabupaten_kota) %>% summarise(value = sum(jumlah_bencana)) %>% arrange(desc(value))
  kasus.by.tahun <- banjirdat %>% group_by(tahun) %>% summarise(value = sum(jumlah_bencana)) 
  top.kasus <- kasus.by.daerah %>% filter(value==max(value))
  mean.kasus <- mean(kasus.by.tahun %>% select(value) %>% unlist)
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.kasus, format="d", big.mark=',')
      ,'Total Kasus banjir'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(top.kasus$value, format="d", big.mark=',')
      ,paste('Total Kasus di ',top.kasus$nama_kabupaten_kota)
      ,icon = icon("exclamation-sign",lib='glyphicon')
      ,color = "red")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(mean.kasus, format="f",digits=2, big.mark=',')
      ,'Rata-rata Kasus Tiap tahun'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  #creating the plotOutput content
  
  output$plottahun <- renderPlot({
    ggplot(data=kasus.by.tahun, aes(x=tahun, y=value)) + geom_line(color="red") + geom_text(aes(label=value),hjust=0.5, vjust=-0.5) + geom_point() + labs(x="Tahun", y="Jumlah Banjir")
  })
  
  output$boxplottahun <- renderPlot({
    ggplot(data=banjirdat, aes(x=as.character(tahun) , y=jumlah_bencana, fill = as.character(tahun))) + geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=1) + labs(x="Tahun", y="Jumlah Banjir") + theme(legend.position = "none")
  })
  
  
  output$bardaerah <- renderPlot({
    ggplot(data=kasus.by.daerah, aes(x = nama_kabupaten_kota, y= value, fill=nama_kabupaten_kota))+geom_bar(stat="identity")+coord_flip()+ theme(legend.position = "none") +geom_text(aes(label=value),hjust=-0.1, vjust=0.2) + labs( x="", y="Jumlah Banjir")
  })
  
  
  
}


shinyApp(ui, server)