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
  library(ggrepel)
  library(ggalt)
  library(ggfortify)
  library(RColorBrewer)}

theme_set(theme_bw())

tabela <- read.csv(file = 'tabela.csv')
tabela <- tabela[,-c(16:18)]
tabela[1] <- sapply(tabela[1],as.numeric)
tabela[3:15] <- sapply(tabela[3:15],as.numeric)
tabela$Squad <- factor(as.character(tabela$Squad), levels = tabela$Squad[order(tabela$Rk)])
tabela_sh <- tabela[,-c(11:15)]

gole_strzelone <- tabela[,c(1:2,7,11)]
gole_strzelone2 <- tabela[,c(2,7,11)]
gole_strzelone2$xG <- (gole_strzelone2$GF-gole_strzelone2$xG)
gole_stracone <- tabela[,c(1:2,8,12)]
gole_stracone2 <- tabela[,c(2,8,12)]
gole_stracone2$xGA <- (gole_stracone2$xGA-gole_stracone2$GA)
rezultaty <- tabela[,c(1:2,4:6)]

statystyki <- data.frame(statystyki=c("Strzelanie","Podania", "GCA i SCA (Goal and Shot Creating Actions)","Akcje defensywne","Posiadanie piłki","Kadra zespołu","Pozostałe"))

#Strzelanie
strzelanie <- read.csv(file = 'strzelanie.csv')
strzelanie[2:20] <- sapply(strzelanie[2:20], as.numeric)
xg_dif <- strzelanie[,c(1,19)]
xg_dif$type <- cut(xg_dif$G.xG, c(-Inf, 0, Inf), 
                   labels=c('below', 'above'))
xg_dif <- xg_dif[order(xg_dif$G.xG), ] 
xg_dif$Squad <- factor(xg_dif$Squad, levels = xg_dif$Squad)
karne <- strzelanie[,c(1,14:15)]
karne$Pknot <- (karne$PKatt-karne$PK)
karne <- karne[order(-karne$PKatt),]
karne$Squad <- factor(karne$Squad, levels = karne$Squad)
karne <- karne[,-c(3)]

#Podania
podania <- read.csv(file = 'podania.csv')
podania[2:25] <- sapply(podania[2:25], as.numeric)
podania1 <- podania[,c(1,6,8)]
podania1_adv <- podania1[c(6,9:12,17),]

typy_podan <- read.csv(file = 'typy_podan.csv')
typy_podan[2:28] <- sapply(typy_podan[2:28], as.numeric)
typy_podan <- typy_podan[order(typy_podan$Squad, decreasing = TRUE), ] 
typy_podan$Squad <- factor(typy_podan$Squad, levels = typy_podan$Squad)
rozne_in <- typy_podan[,c(1,13)]
rozne_out <- typy_podan[,c(1,14)]
rozne_in$count <- rozne_in$In
rozne_out$count <- rozne_out$Out
colnames(rozne_in)[2] <- "Type"
colnames(rozne_out)[2] <- "Type"
rozne_in$Type <- "In"
rozne_out$Type <- "Out"
rozne2 <- rbind(rozne_in, rozne_out)
rozne2$count[1:20] <- -rozne2$count[1:20]
rozne2 <- rozne2[order(rozne2$Squad),]
podania3 <- podania[,c(1,9,12,15)]


#SCA
sca <- read.csv(file = 'SCA.csv')
sca_sh <- sca[,c(1,4)]
sca_sh <- sca_sh[order(-sca_sh$SCA),]
sca_sh$Squad <- factor(sca_sh$Squad, levels = sca_sh$Squad)
sca_long <- data.frame(Squad=sprintf("Manchester City",seq(1:1229)))

for (i in 2:20) {
  nazwa <- as.character(sca_sh$Squad[i])
  n <- sca_sh$SCA[i]
  sca_long2 <- data.frame(Squad=sprintf(nazwa,seq(1:n)))
  sca_long <- rbind(sca_long, sca_long2)
}

nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
var <- sca_long$Squad
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table
df$category <- factor(rep(names(categ_table), categ_table))  

nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

#POSIADANIE
posiadanie <- read.csv(file = 'posiadanie.csv')
posiadanie <- posiadanie[,c(1,3,5,13)]
posiadanie[2:4] <- sapply(posiadanie[2:4], as.numeric)
posiadanie$opis <- paste0(posiadanie$Squad,", ",posiadanie$Poss,"%")


#CZAS
czas <- read.csv(file = 'czas.csv')
czas <- czas[,c(1:3)]
czas[2:3] <- sapply(czas[2:3], as.numeric)
av_played <- mean(czas$X..Pl)
av_age <- mean(czas$Age)
czas$X..Pl <- (czas$X..Pl-av_played)
czas$Age <- round((czas$Age-av_age),1)

liczba <- czas[c(1:2)]
liczba <- liczba[order(liczba$X..Pl), ] 
liczba$Squad <- factor(liczba$Squad, levels = liczba$Squad)
liczba$type <- cut(liczba$X..Pl, c(-Inf, 0, Inf), 
                   labels=c('below', 'above'))

wiek <- czas[,c(1,3)]
wiek <- wiek[order(wiek$Age), ] 
wiek$Squad <- factor(wiek$Squad, levels = wiek$Squad)
wiek$type <- cut(wiek$Age, c(-Inf, 0, Inf), 
                   labels=c('below', 'above'))

#PANEL3
#gole, xG
zawodnicy_s <- read.csv(file = 'aaa.csv', fileEncoding="utf-16")
zawodnicy_s$Player <- gsub("/.*$","",zawodnicy_s$Player)
zawodnicy_s[6:25] <- sapply(zawodnicy_s[6:25], as.numeric)
druzyny <- zawodnicy_s[,c(2,5)]
druzyny_lista <- as.vector(unique(druzyny$Squad))
gole1 <- zawodnicy_s[,c(2,5,8:9,21)]
gole1 <- gole1[order(gole1$Gls), ]
gole1$Squad <- as.character(gole1$Squad) 
  
 
  
# gole <- zawodnicy_s[,c(2,5,8:9)]
# 
# gole <- group_by(gole, Player) %>% 
#   summarise(gole = sum(Gls))
# 
# test <- left_join(gole, druzyny, by = c("Player"))
# 
# gole <- gole[order(gole$Gls), ] 
# gole$Player <- factor(gole$Player, levels = gole$Player)


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
             tabPanel("Zaawansowane statystyki drużynowe", fluid = TRUE, icon = icon("exchange-alt"),
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(column(width=12,
                                          checkboxGroupInput(
                                            inputId = "statystyki_zespoly",
                                            label = "Wybierz statystyki",
                                            choices = statystyki$statystyki[1:7],
                                            inline = FALSE,
                                            selected = statystyki$statystyki[1:7])
                          )), width = 2
                        ),
                        mainPanel(
                        conditionalPanel(
                          condition = "input.statystyki_zespoly.includes('Strzelanie')",
                          h3("Strzelanie", align = "center"),
                          fluidRow(column(width=6,
                                          plotOutput(outputId = "strzelanie1")
                                          ),
                                   column(width=6,
                                          plotOutput(outputId = "strzelanie2")
                                          )
                                   ),
                          hr()
                      ),
                      conditionalPanel(
                        condition = "input.statystyki_zespoly.includes('Podania')",
                        h3("Podania", align="center"),
                        fluidRow(column(width=12,
                                        plotOutput(outputId = "podania3")
                                        )
                                 ),
                        fluidRow(column(width=6,
                                        plotOutput(outputId = "podania1")
                                        ),
                                 column(width=6,
                                        plotOutput(outputId = "podania2")
                                        )
                                 ),
                        hr()
                      ),
                      conditionalPanel(
                        condition = "input.statystyki_zespoly.includes('GCA i SCA (Goal and Shot Creating Actions)')",
                        h3("GCA i SCA (Goal and Shot Creating Actions)", align="center"),
                        fluidRow(column(width=12,
                                        plotOutput(outputId = "SCA")
                                        )
                                 ),
                        hr()
                      ),
                      conditionalPanel(
                        condition = "input.statystyki_zespoly.includes('Posiadanie piłki')",
                        h3("Posiadanie piłki", align="center"),
                        fluidRow(column(width=12,
                                        plotOutput(outputId = "posiadanie")
                                        )
                                 ),
                        hr()
                      ),
                      conditionalPanel(
                        condition = "input.statystyki_zespoly.includes('Kadra zespołu')",
                        h3("Kadra zespołu", align="center"),
                        fluidRow(column(width=6,
                                        plotOutput(outputId = "kadra1")
                                        ),
                                 column(width=6,
                                        plotOutput(outputId = "kadra2")
                                        )
                                 ),
                        hr()
                      ),
                      width = 10
             )
             )),    
             tabPanel("Rankingi indywidualne", fluid = TRUE, icon = icon("exchange-alt"),
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(column(width=12,
                                          sliderInput(
                                            inputId = "no_players",
                                            label = "Liczba TOP zawodników",
                                            min = 5,
                                            max = 15,
                                            value = 10,
                                            step = 1),
                                          pickerInput(
                                            inputId = "teams_players",
                                            label = "Wybierz drużyny", 
                                            choices=sort(unique(gole1$Squad)),
                                            selected = sort(unique(gole1$Squad)),
                                            options = list(`actions-box` = TRUE),
                                            multiple = T)
                                          )
                                   ),width=2
                        ),
                        mainPanel(
                          h3("Rankingi indywidualne", align="center"),
                          fluidRow(column(width=6,
                                          plotOutput(outputId = "gole1")
                                          ),
                                   column(width=6,
                                          plotOutput(outputId = "gole2")
                                          )
                                   ),
                          hr(),
                          width=10
                        )
                        )
             ),
             navbarMenu("Porównywanie zawodników", icon = icon("chart-area"))
)
)


server <- function(input, output) {
  
  output$myCondition <- reactive({"strzelanie" %in% input$statystyki_zespoly})
  
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
      scale_fill_manual(values=c("#00ba38","grey","#f8766d")) +
      ggtitle("Gole stracone a Expected Goals Against") +
      xlab("Drużyna") + ylab("Udział") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })

  output$strzelanie1 <- renderPlot({
    
    ggplot(xg_dif, aes(x=Squad, y=G.xG, label=Squad)) + 
      geom_bar(stat='identity', aes(fill=type), width=.5)  +
      labs(fill=NULL) +
      scale_fill_manual(labels = c("Underperformance", "Overperformance"), 
                        values = c("above"="#00ba38", "below"="#f8766d")) + 
      ggtitle("Gole strzelone a Expected Goals") +
      xlab("Drużyna") + ylab("Gole strzelone - Expected Goals") +
      coord_flip() +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
  output$strzelanie2 <- renderPlot({
    karne_long<-melt(karne)
    
    ggplot(karne_long,aes(Squad,value,fill=variable))+
      geom_bar(stat="identity") +
      labs(fill=NULL) +
      scale_fill_manual(labels = c("Strzelone", "Niestrzelone"),
                                   values=c("#00ba38","#f8766d")) +
      ggtitle("Rzuty karne") +
      xlab("Drużyna") + ylab("Liczba rzutów karnych") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      )
  })
  
  output$podania3 <- renderPlot({
    
    podania3_long <- melt(podania3)
    
    ggplot(podania3_long,aes(Squad,value,fill=variable))+
      geom_bar(stat="identity",position="dodge") +
      labs(fill=NULL) +
      scale_fill_manual(labels = c("Krótkie", "Średnie","Długie"),
                        values=c("#00ba38","orange","#f8766d")) +
      ggtitle("Typy podań") +
      xlab("Drużyna") + ylab("Liczba podań") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      )
    
  })
  
  output$podania1 <- renderPlot({
    
    ggplot(podania1, aes(x=Cmp., y=PrgDist,color=as.factor(Squad),label=Squad)) + 
      geom_point() +
      scale_y_continuous(limits=c(75000, 130000)) +
      geom_encircle(aes(x=Cmp., y=PrgDist), 
                    data=podania1_adv, 
                    color="orange", 
                    size=2, 
                    expand=0.02) +
      geom_text_repel() + labs(title = "geom_text_repel()") +
      ggtitle("Dokładność podań a dystans progresywny") +
      xlab("Dokładność podań (%)") + ylab("Dystans progresywny (m)") +
      theme(
        legend.position="none",
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
  output$podania2 <- renderPlot({
    
    brks <- seq(-100, 100, 50)
    lbls <- (c(seq(100, 0, -50), seq(50, 100, 50)))
    
    ggplot(rozne2, aes(x = Squad, y = count)) +
      geom_bar(stat = "identity", aes(fill = Type), width = .6) +   # draw the bars
      labs(fill=NULL) +
      scale_y_continuous(breaks = brks,   # Breaks
                         labels = lbls) +
      coord_flip() +  # Flip axes
      ggtitle("Typy dośrodkowań z rzutów rożnych") +
      xlab("Drużyna") + ylab("Liczba rzutów rożnych") + 
      theme(plot.title = element_text(hjust = .5), 
            axis.ticks = element_blank()) +   # Centre plot title
      scale_fill_brewer(palette = "Dark2",
                        labels = c("Piłka dochodząca", "Piłka odchodząca")) +  # Color palette
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
    
  })
  
  output$SCA <- renderPlot({
    
    ggplot(df, aes(x = x, y = y, fill = category)) + 
      geom_tile(color = "black", size = 0.5) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
      scale_fill_manual(values = mycolors) +
      ggtitle("Udział drużyn w całkowitej liczbie SCA") +
      theme(panel.border = element_rect(size = 2),
            plot.title = element_text(size=14, face="bold"),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.position = "right")
  })
  
  output$posiadanie <- renderPlot({
    ggplot(posiadanie, aes(x=Touches, y=Att, size=Poss, col=as.factor(Squad), label=opis), guide=FALSE)+
      geom_point(shape=19)+ scale_size_area(max_size = 20) +
      geom_text_repel(size=4, nudge_y = -0.25, col = "black") + labs(title = "geom_text_repel()") +
      ggtitle("Kontakty z piłką a próby dryblingu a posiadanie piłki") +
      scale_y_continuous(limits=c(450,850))+
      xlab("Kontakty z piłką") + ylab("Próby dryblingu") + 
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none"
        )  
  })  
  
  output$kadra1 <- renderPlot({
    
    ggplot(wiek, aes(x=Squad, y=Age, label=Squad)) + 
      geom_bar(stat='identity', aes(fill=type), width=.5)  +
      labs(fill=NULL) +
      scale_fill_manual(labels = c("Młody zespół", "Stary zespół"), 
                        values = c("above"="#f8766d", "below"="#00ba38")) + 
      ggtitle("Średni wiek zawodników") +
      xlab("Drużyna") + ylab("Różnica od średniego wieku zespołu w lidze") +
      coord_flip() +
      geom_hline(yintercept=0) +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )  +
      annotate("text", x=1, y=1, label= "Średni wiek zespołu: 26.5")
  })
  
  output$kadra2 <- renderPlot({
    
    ggplot(liczba, aes(x=Squad, y=X..Pl, label=Squad)) + 
      geom_bar(stat='identity', aes(fill=type), width=.5)  +
      labs(fill=NULL) +
      scale_fill_manual(labels = c("Wąska kadra", "Szeroka kadra"), 
                        values = c("above"="#00ba38", "below"="#f8766d")) + 
      ggtitle("Liczba graczy, którzy wystąpili w zespole") +
      xlab("Drużyna") + ylab("Różnica od średniej liczby graczy zespołu w lidze") +
      coord_flip() +
      geom_hline(yintercept=0) +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )  +
      annotate("text", x=1, y=2, label= "Średnia liczba graczy zespołu: 26.1")
  })
   
  gole1_filtered <- reactive({
    req(input$no_players)
    req(input$teams_players)
    gole1 <- filter(gole1, Squad %in% input$teams_players)
    gole1 <- gole1[with(gole1,order(-Gls)),]
    gole1 <- gole1[1:input$no_players,]
    gole1$Player <- factor(gole1$Player, levels = gole1$Player)
    gole1   
  })
  
  output$gole1 <- renderPlot({
    ggplot(gole1_filtered(), aes(Player, Gls, fill=as.factor(Player))) + 
      geom_col() +
      geom_text(aes(label = Gls), vjust = 1.5,fontface = "bold") +
      ggtitle("Najlepsi strzelcy") +
      xlab("Zawodnik") + ylab("Gole") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none"
      )
  })
  
  gole2_filtered <- reactive({
    req(input$no_players)
    req(input$teams_players)
    gole1 <- filter(gole1, Squad %in% input$teams_players)
    gole1 <- gole1[with(gole1,order(-xG)),]
    gole1 <- gole1[1:input$no_players,]
    gole1$Player <- factor(gole1$Player, levels = gole1$Player)
    gole1   
  })
  
  output$gole2 <- renderPlot({
    ggplot(gole2_filtered(), aes(Player, xG, fill=as.factor(Player))) + 
      geom_col() +
      geom_text(aes(label = xG), vjust = 1.5,fontface = "bold") +
      ggtitle("Najlepsi zawodnicy pod względem Expected Goals") +
      xlab("Zawodnik") + ylab("Expected Goals") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none"
      )
  })
  
}

shinyApp(ui = ui, server = server)

