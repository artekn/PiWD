#Libraries
{
  library(shiny)
  library(shinydashboard)
  library(htmltab)
  library(shinythemes)
  library(shinyWidgets)
  library(shinycssloaders)
  library(DT)
  library(xml2)
  library(rvest)
  library(readr)
  library(dplyr)
  library(fmsb)
  library(reshape2)
  library(ggrepel)}

tabela <- read.csv(file = 'tabela.csv')
tabela <- tabela[,-c(16:18)]
tabela[1] <- sapply(tabela[1],as.numeric)
tabela[3:15] <- sapply(tabela[3:15],as.numeric)
tabela_sh <- tabela[,-c(11:15)]

gole_strzelone <- tabela[,c(1:2,7,11)]
gole_strzelone2 <- tabela[,c(2,7,11)]
gole_strzelone2$xG <- (gole_strzelone2$GF-gole_strzelone2$xG)
gole_stracone <- tabela[,c(1:2,8,12)]
gole_stracone2 <- tabela[,c(2,8,12)]
gole_stracone2$xGA <- (gole_stracone2$xGA-gole_stracone2$GA)
rezultaty <- tabela[,c(1:2,4:6)]




ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("Premier League 2019/2020", theme = shinytheme("united"), 
             tabPanel("Strona główna", fluid = TRUE,
                      fluidRow(column(width=12,
                                      h3(
                                        p("Podsumowanie Premier League 2019/2020")),
                                      h5(p("Projekt zaliczeniowy z przedmiotu Prezentacji i wizualizacja danych - Artur Nieścieruk 76850.")),
                                      hr(),
                                      h5("Źródło danych:",
                                         a("fbref.com",
                                           href = "https://fbref.com/en/"),
                                           img(src = "FBref.jpg", height = "30px")),
                                      h5("Stworzone za pomocą",
                                         img(src = "shiny.png", height = "30px"),
                                         "w ",
                                         img(src = "RStudio.png", height = "30px"),
                                         ".")
                                      )
                              )
             ),
             tabPanel("Podstawowe statystyki drużynowe", fluid=TRUE,
                      fluidRow(column(width=12,
                                      h3("Tabela Premier League 2019/2020"),
                                      dataTableOutput("tabela"),
                                      br(),
                                      h3("Statystyki ofensywne", align = "center"),
                                      plotOutput(outputId = "gole_strzelone"),
                                      plotOutput(outputId = "gole_strzelone2"),
                                      hr(),
                                      h3("Statystyki defensywne", align = "center"),
                                      plotOutput(outputId = "gole_stracone"),
                                      plotOutput(outputId = "gole_stracone2"),
                                      hr(),
                                      plotOutput(outputId = "rezultaty")
                                      ))
                      ),
             navbarMenu("Statystyki bramkarzy", icon = icon("chart-bar")),
             navbarMenu("Porównywanie zawodników", icon = icon("chart-area"))
)
)


server <- function(input, output) {
  
  output$tabela <- DT::renderDataTable(
    tabela_sh,
    options = list(pageLength = 20, #all 20 teams
                   dom = 't', filter = list(position = "top")), #disable search bar
    rownames = FALSE
  )
  
  output$gole_strzelone <- renderPlot({
    gole_strzelone$Squad <- factor(as.character(gole_strzelone$Squad), levels = gole_strzelone$Squad[order(gole_strzelone$Rk)])
    gole_strzelone_long <- gole_strzelone[,c(2:4)]
    gole_strzelone_long <- melt(gole_strzelone_long)
    
    
    ggplot(gole_strzelone_long,aes(Squad,value,fill=variable))+
      geom_bar(stat="identity",position="dodge") +
      labs(fill=NULL) +
      scale_fill_manual(values=c("black","orange")) +
      ggtitle("Gole strzelone a Expected Goals") +
      xlab("Drużyna") + ylab("Wartość") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
  output$gole_strzelone2 <- renderPlot({
    ggplot(gole_strzelone2,aes(x=GF,y=xG,color=as.factor(Squad),label=Squad)) +
      geom_point() +
      geom_hline(yintercept=0) +
      geom_text_repel() + labs(title = "geom_text_repel()") +
      labs(fill=NULL) +
      ggtitle("Gole strzelone i gole strzelone - Expected Goals") +
      xlab("Gole strzelone") + ylab("gole strzelone - Expected Goals") +
      theme(
        legend.position="none",
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      ) +
      annotate("text", x=92, y=1, label= "Szczęśliwe zespoły") +
      annotate("text", x=92, y=-1, label= "Pechowe zespoły") 
  })
  
  output$gole_stracone <- renderPlot({
    gole_stracone$Squad <- factor(as.character(gole_stracone$Squad), levels = gole_stracone$Squad[order(gole_stracone$Rk)])
    gole_stracone_long <- gole_stracone[,c(2:4)]
    gole_stracone_long<-melt(gole_stracone_long)
    
    ggplot(gole_stracone_long,aes(Squad,value,fill=variable))+
      geom_bar(stat="identity",position="dodge") +
      labs(fill=NULL) +
      scale_fill_manual(values=c("black","orange")) +
      ggtitle("Gole stracone a Expected Goals Against") +
      xlab("Drużyna") + ylab("Wartość") +
        theme(
          plot.title = element_text(size=14, face="bold"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold")
        )
  })
  
  output$gole_stracone2 <- renderPlot({
    ggplot(gole_stracone2,aes(x=GA,y=xGA,color=as.factor(Squad),label=Squad)) +
      geom_point() +
      geom_hline(yintercept=0) +
      geom_text_repel() + labs(title = "geom_text_repel()") +
      labs(fill=NULL) +
      ggtitle("Gole stracone i Expected Goals Against - gole stracone") +
      xlab("Gole stracone") + ylab("Expected Goals Against - gole stracone") +
      theme(
        legend.position="none",
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      ) +
      annotate("text", x=70, y=1, label= "Szczęśliwe zespoły") +
      annotate("text", x=70, y=-1, label= "Pechowe zespoły") 
  })
  
  output$rezultaty <- renderPlot({
    rezultaty$Squad <- factor(as.character(rezultaty$Squad), levels = rezultaty$Squad[order(rezultaty$Rk)])
    rezultaty_long <- rezultaty[,c(2:5)]
    rezultaty_long<-melt(rezultaty_long)
    
    ggplot(rezultaty_long,aes(Squad,value,fill=variable))+
      geom_bar(stat="identity",position="fill") +
      labs(fill=NULL) +
      scale_fill_manual(values=c("green","grey","red")) +
      ggtitle("Gole stracone a Expected Goals Against") +
      xlab("Drużyna") + ylab("Udział") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
}

shinyApp(ui = ui, server = server)

