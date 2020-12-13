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

statystyki <- data.frame(statystyki=c("Strzelanie","Podania", "GCA i SCA (Goal and Shot Creating Actions)","Posiadanie piłki","Kadra zespołu"))
paleta <- data.frame(team=tabela$Squad,
                     color=c("#d00027","#6caddf","#da020e","#034694","#0053a0","#132257","#fdb913","#EF0107","#ec2227","#fff30d","#ed1a3b","#274488","#f0b83d","#1b458f","#005daa","#7c2c3b","#670E36","#ba0f13","#fbee23","#00a650"))

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


#asysty, xA
zawodnicy_a <- read.csv(file = 'bbb.csv', fileEncoding="utf-16")
zawodnicy_a$Player <- gsub("/.*$","",zawodnicy_a$Player)
zawodnicy_a[6:31] <- sapply(zawodnicy_a[6:31], as.numeric)
asysty1 <- zawodnicy_a[,c(2,5,8,23:24)] 
asysty1 <- asysty1[order(asysty1$Ast), ]
asysty1$Squad <- as.character(asysty1$Squad)

#podania
zawodnicy_po <- read.csv(file = 'ccc.csv', fileEncoding="utf-16")
zawodnicy_po$Player <- gsub("/.*$","",zawodnicy_po$Player)
zawodnicy_po[6:34] <- sapply(zawodnicy_po[6:34], as.numeric)
podania11 <- zawodnicy_po[,c(2,5,8,29)] 
podania11 <- podania11[order(podania11$Cmp), ]
podania11$Squad <- as.character(podania11$Squad)

#pressingi
zawodnicy_pr <- read.csv(file = 'ddd.csv', fileEncoding="utf-16")
zawodnicy_pr$Player <- gsub("/.*$","",zawodnicy_pr$Player)
zawodnicy_pr[6:32] <- sapply(zawodnicy_pr[6:32], as.numeric)
pressingi1 <- zawodnicy_pr[,c(2,5,8,19)] 
pressingi1 <- pressingi1[order(pressingi1$Succ), ]
pressingi1$Squad <- as.character(pressingi1$Squad)

#PANEL4
#90s
druzyna_czas <- zawodnicy_s[,c(2,5,8)]
druzyna_czas <- druzyna_czas[order(druzyna_czas$X90s), ]

#gole
gole11 <- gole1

#pojedynki
pojedynki <- read.csv(file = 'pozostale.csv', fileEncoding="utf-16")
pojedynki$Player <- gsub("/.*$","",pojedynki$Player)
pojedynki[6:24] <- sapply(pojedynki[6:24], as.numeric)
pojedynki1 <- pojedynki[,c(2,5,22:24)]
pojedynki1 <- filter(pojedynki1, Won+Lost >= 10)
pojedynki1$Won. <- (pojedynki1$Won.-50)

#wplyw
wplyw <- read.csv(file = 'wplyw.csv', fileEncoding="utf-16")
wplyw$Player <- gsub("/.*$","",wplyw$Player)
wplyw[6:29] <- sapply(wplyw[6:29], as.numeric)
wplyw1 <- wplyw[,c(2,5,9,29)]
wplyw1 <- na.omit(wplyw1)
wplyw1 <- filter(wplyw1, Min > 270)

#bramkarze
bramkarze <- read.csv(file = 'bramkarze.csv', fileEncoding="utf-16")
bramkarze$Player <- gsub("/.*$","",bramkarze$Player)
bramkarze$PSxG <- round((bramkarze$PSxG / bramkarze$X90s),2)
bramkarze$PSxG.... <- round((bramkarze$PSxG.... / bramkarze$X90s),2)
bramkarze_uno <- bramkarze[c(33,16,30,7,1,35,3,29,23,28,27,37,19,32,34,36,6,9,25,11),]
bramkarze_uno$Squad <- as.character(bramkarze_uno$Squad)
av_psxg <- round(mean(bramkarze_uno$PSxG),2)

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
                                            choices = statystyki$statystyki[1:5],
                                            inline = FALSE,
                                            selected = statystyki$statystyki[1:5])
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
                          fluidRow(column(width=6,
                                          plotOutput(outputId = "asysty1")
                                          ),
                                   column(width=6,
                                          plotOutput(outputId = "asysty2")
                                          )
                                   ),
                          hr(),
                          fluidRow(column(width=6,
                                          plotOutput(outputId = "podania")
                                          ),
                                   column(width=6,
                                          plotOutput(outputId = "pressingi")
                                          )
                                   ),
                          width=10
                        )
                        )
             ),
             navbarMenu("Przegląd sezonu drużyny", icon = icon("chart-area"),
                        tabPanel("Jedna drużyna", fluid = TRUE, icon = icon("exchange-alt"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     fluidRow(column(width=12,
                                                     pickerInput(
                                                       inputId = "teams_players2",
                                                       label = "Wybierz drużynę", 
                                                       choices=sort(unique(gole1$Squad)),
                                                       selected = "Arsenal",
                                                       options = list(`actions-box` = TRUE),
                                                       multiple = F)
                                                     )
                                              ),width=2
                                   ),
                                   mainPanel(
                                     h3(imageOutput(outputId="Logo", height = "125px"),
                                        align="center"),
                                     h2(textOutput(outputId = "Klub"),
                                        align="center"),
                                     h4(textOutput(outputId = "Rank"),
                                        align="center"),
                                     fluidRow(column(width=6,
                                                     plotOutput(outputId = "team_time")
                                                     ),
                                              column(width=6,
                                                     plotOutput(outputId = "team_goals")
                                                     )
                                              ),
                                     br(),
                                     fluidRow(column(width=6,
                                                     plotOutput(outputId = "pojedynki", height = "600px")
                                                     ),
                                              column(width=6,
                                                     plotOutput(outputId = "wplyw")
                                                     )
                                     ),
                                     fluidRow(column(width=6,
                                                     plotOutput(outputId = "bramkarze")
                                                     ),
                                              column(width=6,
                                                     h3("Ocena występu bramkarza"),
                                                     h4(textOutput(outputId = "bramkarz")),
                                                     p(textOutput(outputId = "tekst"))
                                                     )
                                     ),width=10
                                     )
                                   )
                                 ),
                        tabPanel("Porównanie drużyn", fluid = TRUE, icon = icon("exchange-alt"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     fluidRow(column(width=12,
                                                     pickerInput(
                                                       inputId = "teams_players_a",
                                                       label = "Wybierz drużynę", 
                                                       choices=sort(unique(gole1$Squad)),
                                                       selected = "Arsenal",
                                                       options = list(`actions-box` = TRUE),
                                                       multiple = F),
                                                     pickerInput(
                                                       inputId = "teams_players_b",
                                                       label = "Wybierz drużynę", 
                                                       choices=sort(unique(gole1$Squad)),
                                                       selected = "Aston Villa",
                                                       options = list(`actions-box` = TRUE),
                                                       multiple = F)
                                     )
                                     ),width=2
                                   ),
                                   mainPanel(
                                     fluidRow(
                                     column(width=6,
                                     h3(imageOutput(outputId="Logo2", height = "125px"),
                                        align="center"),
                                     h2(textOutput(outputId = "Klub2"),
                                        align="center"),
                                     h4(textOutput(outputId = "Rank2"),
                                        align="center")
                                     ),
                                     column(width=6,
                                            h3(imageOutput(outputId="Logo3", height = "125px"),
                                               align="center"),
                                            h2(textOutput(outputId = "Klub3"),
                                               align="center"),
                                            h4(textOutput(outputId = "Rank3"),
                                               align="center")
                                     )
                                     ),
                                     fluidRow(
                                       column(width=6,
                                              plotOutput(outputId = "team_time_a")
                                              ),
                                       column(width=6,
                                              plotOutput(outputId = "team_time_b")
                                              )
                                     
                                     ),
                                     hr(),
                                     fluidRow(
                                       column(width=6,
                                              plotOutput(outputId = "team_goals_a")
                                              ),
                                       column(width=6,
                                              plotOutput(outputId = "team_goals_b")
                                              ) 
                                     ),
                                     hr(),
                                     fluidRow(
                                       column(width=6,
                                              plotOutput(outputId = "pojedynki_a", height = "600px")
                                              ),
                                       column(width=6,
                                              plotOutput(outputId = "pojedynki_b", height = "600px")
                                              ) 
                                     ),
                                     hr(),
                                     fluidRow(
                                       column(width=6,
                                              plotOutput(outputId = "wplyw_a")
                                              ),
                                       column(width=6,
                                              plotOutput(outputId = "wplyw_b")
                                              ) 
                                     ),
                                     hr(),
                                     fluidRow(
                                       column(width=12,
                                              plotOutput(outputId = "bramkarze_ab")
                                              ) 
                                     ),
                                     width=10
                                   )
                                 )
                        )
                        )
             )
)


server <- function(input, output) {
  
  #output$myCondition <- reactive({"strzelanie" %in% input$statystyki_zespoly})
  
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
      scale_fill_manual(values=c("black","orange"),
                        labels=c("Gole","xG")) +
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
      scale_fill_manual(values=c("black","orange"),
                        labels=c("Gole stracone","xGA")) +
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
      scale_fill_manual(values=c("#00ba38","grey","#f8766d"),
                        labels=c("Zwyciętwa","Remisy","Porażki")) +
      ggtitle("Rozkład rezultatów") +
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
  
  asysty1_filtered <- reactive({
    req(input$no_players)
    req(input$teams_players)
    asysty1 <- filter(asysty1, Squad %in% input$teams_players)
    asysty1 <- asysty1[with(asysty1,order(-Ast)),]
    asysty1 <- asysty1[1:input$no_players,]
    asysty1$Player <- factor(asysty1$Player, levels = asysty1$Player)
    asysty1   
  })
  
  output$asysty1 <- renderPlot({
    ggplot(asysty1_filtered(), aes(Player, Ast, fill=as.factor(Player))) + 
      geom_col() +
      geom_text(aes(label = Ast), vjust = 1.5,fontface = "bold") +
      ggtitle("Najlepsi asystujący") +
      xlab("Zawodnik") + ylab("Asysty") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none"
      )
  })
  
  asysty2_filtered <- reactive({
    req(input$no_players)
    req(input$teams_players)
    asysty1 <- filter(asysty1, Squad %in% input$teams_players)
    asysty1 <- asysty1[with(asysty1,order(-xA)),]
    asysty1 <- asysty1[1:input$no_players,]
    asysty1$Player <- factor(asysty1$Player, levels = asysty1$Player)
    asysty1   
  })
  
  output$asysty2 <- renderPlot({
    ggplot(asysty2_filtered(), aes(Player, xA, fill=as.factor(Player))) + 
      geom_col() +
      geom_text(aes(label = xA), vjust = 1.5,fontface = "bold") +
      ggtitle("Najlepsi zawodnicy pod względem Expected Assists") +
      xlab("Zawodnik") + ylab("Expected Assists") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none"
      )
  })
  
  podania11_filtered <- reactive({
    req(input$no_players)
    req(input$teams_players)
    podania11 <- filter(podania11, Squad %in% input$teams_players)
    podania11 <- podania11[with(podania11,order(-Cmp)),]
    podania11 <- podania11[1:input$no_players,]
    podania11$Player <- factor(podania11$Player, levels = podania11$Player)
    podania11   
  })
  
  output$podania <- renderPlot({
    ggplot(podania11_filtered(), aes(Player, Cmp, fill=as.factor(Player))) + 
      geom_col() +
      geom_text(aes(label = Cmp), vjust = 1.5,fontface = "bold") +
      ggtitle("Najwięcej podający") +
      xlab("Zawodnik") + ylab("Podania") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none"
      )
  })
  
  pressingi1_filtered <- reactive({
    req(input$no_players)
    req(input$teams_players)
    pressingi1 <- filter(pressingi1, Squad %in% input$teams_players)
    pressingi1 <- pressingi1[with(pressingi1,order(-Succ)),]
    pressingi1 <- pressingi1[1:input$no_players,]
    pressingi1$Player <- factor(pressingi1$Player, levels = pressingi1$Player)
    pressingi1   
  })
  
  output$pressingi <- renderPlot({
    ggplot(pressingi1_filtered(), aes(Player, Succ, fill=as.factor(Player))) + 
      geom_col() +
      geom_text(aes(label = Succ), vjust = 1.5,fontface = "bold") +
      ggtitle("Najlepsi zawodnicy w pressingu") +
      xlab("Zawodnik") + ylab("Liczba udanych pressingów") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none"
      )
  })
        
  output$Logo<- renderImage({
    req(input$teams_players2)
    if(input$teams_players2=="Arsenal") Leg<-"www/arsenal.png"
    if(input$teams_players2=="Aston Villa") Leg<-"www/astonvilla.png"
    if(input$teams_players2=="Bournemouth") Leg<-"www/bournemouth.png"
    if(input$teams_players2=="Brighton") Leg<-"www/brighton.png"
    if(input$teams_players2=="Burnley") Leg<-"www/burnley.png"
    if(input$teams_players2=="Chelsea") Leg<-"www/chelsea.png"
    if(input$teams_players2=="Crystal Palace") Leg<-"www/palace.png"
    if(input$teams_players2=="Everton") Leg<-"www/everton.png"
    if(input$teams_players2=="Leicester City") Leg<-"www/leicester.png"
    if(input$teams_players2=="Liverpool") Leg<-"www/liverpool.png"
    if(input$teams_players2=="Manchester City") Leg<-"www/mancity.png"
    if(input$teams_players2=="Manchester Utd") Leg<-"www/manunited.png"
    if(input$teams_players2=="Newcastle Utd") Leg<-"www/newcastle.png"
    if(input$teams_players2=="Norwich City") Leg<-"www/norwich.png"
    if(input$teams_players2=="Sheffield Utd") Leg<-"www/sheffield.png"
    if(input$teams_players2=="Southampton") Leg<-"www/southampton.png"
    if(input$teams_players2=="Tottenham") Leg<-"www/spurs.png"
    if(input$teams_players2=="Watford") Leg<-"www/watford.png"
    if(input$teams_players2=="West Ham") Leg<-"www/westham.png"
    if(input$teams_players2=="Wolves") Leg<-"www/wolves.png"
    list(src=Leg)
  }, deleteFile = FALSE) 
  
  output$Klub<- renderText({
    req(input$teams_players2)
    tekst <- input$teams_players2
  }) 
  
  output$Rank<- renderText({
    req(input$teams_players2)
    if(input$teams_players2=="Arsenal") TekstR<-"8. miejsce"
    if(input$teams_players2=="Aston Villa") TekstR<-"17. miejsce"
    if(input$teams_players2=="Bournemouth") TekstR<-"18. miejsce"
    if(input$teams_players2=="Brighton") TekstR<-"15. miejsce"
    if(input$teams_players2=="Burnley") TekstR<-"10. miejsce"
    if(input$teams_players2=="Chelsea") TekstR<-"4. miejsce"
    if(input$teams_players2=="Crystal Palace") TekstR<-"14. miejsce"
    if(input$teams_players2=="Everton") TekstR<-"12. miejsce"
    if(input$teams_players2=="Leicester City") TekstR<-"5. miejsce"
    if(input$teams_players2=="Liverpool") TekstR<-"1. miejsce"
    if(input$teams_players2=="Manchester City") TekstR<-"2. miejsce"
    if(input$teams_players2=="Manchester Utd") TekstR<-"3. miejsce"
    if(input$teams_players2=="Newcastle Utd") TekstR<-"13. miejsce"
    if(input$teams_players2=="Norwich City") TekstR<-"20. miejsce"
    if(input$teams_players2=="Sheffield Utd") TekstR<-"9. miejsce"
    if(input$teams_players2=="Southampton") TekstR<-"11. miejsce"
    if(input$teams_players2=="Tottenham") TekstR<-"6. miejsce"
    if(input$teams_players2=="Watford") TekstR<-"19. miejsce"
    if(input$teams_players2=="West Ham") TekstR<-"16. miejsce"
    if(input$teams_players2=="Wolves") TekstR<-"7. miejsce"
    TekstR
  })
  
  bramkarz_wyb <- reactive({
    req(input$teams_players2)
    bramkarz_wyb <- bramkarze_uno$Player[bramkarze_uno$Squad == input$teams_players2]
    bramkarz_wyb
  })
  
  output$bramkarz<- renderText({
    req(input$teams_players2)
    bramkarz <- bramkarze_uno$Player[bramkarze_uno$Squad == input$teams_players2]
    bramkarz <- paste0(bramkarz," (",input$teams_players2,")")
    bramkarz
  }) 
  
  output$tekst<- renderText({
    if (bramkarze_uno$PSxG[bramkarze_uno$Squad == input$teams_players2] > av_psxg) string1 <- "więcej" else string1 <-  "mniej"
    if (bramkarze_uno$PSxG....[bramkarze_uno$Squad == input$teams_players2] > 0) string2 <- "mniej" else string2 <-  "więcej"
    if (bramkarze_uno$PSxG[bramkarze_uno$Squad == input$teams_players2] > av_psxg) string3 <- "wyższych umiejętnościach lub po prostu szczęściu." else string3 <-  "niższych umiejętnościach lub po prostu pechu."
    tekst <- paste0(bramkarz_wyb()," musiał się mierzyć ze strzałami wartymi średnio ",
           bramkarze_uno$PSxG[bramkarze_uno$Squad == input$teams_players2],  " PSxG na mecz, co jest o ",
           abs((bramkarze_uno$PSxG[bramkarze_uno$Squad == input$teams_players2])-av_psxg), " PSxG ", string1,
           " od średniej dla podstawowych bramkarzy drużyn Premier League. Dodatkowo ", bramkarz_wyb(),
           " tracił na mecz średnio o ",
           abs(bramkarze_uno$PSxG....[bramkarze_uno$Squad == input$teams_players2]), " ",
           string2, 
           " bramek niż zakłada model Expected Goals, co świadczy o jego ",
           string3)
    tekst
  })
  
  team_color <- reactive({
    req(input$teams_players2)
    color <- as.character(paleta$color[paleta$team == input$teams_players2])
    color
  })
  
  team_time_filtered <- reactive({
    req(input$teams_players2)
    druzyna_czas <- filter(druzyna_czas, Squad %in% input$teams_players2)
    druzyna_czas <- druzyna_czas[with(druzyna_czas,order(X90s)),]
    druzyna_czas <- druzyna_czas[(nrow(druzyna_czas)-11):nrow(druzyna_czas),]
    druzyna_czas$Player <- factor(druzyna_czas$Player, levels = druzyna_czas$Player)
    druzyna_czas   
  })
  
  output$team_time <- renderPlot({
    ggplot(team_time_filtered(), aes(Player, X90s)) + 
      geom_col(fill=team_color()) +
      coord_flip() +
      geom_text(aes(label = X90s), hjust = 1.1,fontface = "bold") +
      ggtitle("11 zawodników z największa liczbą rozegranych minut") +
      xlab("Zawodnik") + ylab("90s") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none"
      )
  })
  
  team_goals_filtered <- reactive({
    req(input$teams_players2)
    gole11 <- filter(gole11, Squad %in% input$teams_players2)
    gole11 <- gole11[with(gole11,order(-Gls,-xG)),]
    gole11 <- gole11[1:10,]
    gole11$Player <- factor(gole11$Player, levels = gole11$Player)
    gole11 <- gole11[,c(1,4:5)]
    gole11 <- melt(gole11)
    gole11   
  })
  
  output$team_goals <- renderPlot({
    
    ggplot(team_goals_filtered(),aes(Player,value,fill=variable))+
      geom_bar(stat="identity",position="dodge") +
      labs(fill=NULL) +
      scale_fill_manual(labels = c("Gole", "xG"),
                        values=c(team_color(),"grey")) +
      ggtitle("Najlepsi strzelcy") +
      xlab("Zawodnik") + ylab("Wartość") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      )
    
  })
  
  pojedynki_filtered <- reactive({
    req(input$teams_players2)
    pojedynki1 <- filter(pojedynki1, Squad %in% input$teams_players2)
    pojedynki1 <- pojedynki1[with(pojedynki1,order(Won.)),]
    pojedynki1$Player <- factor(pojedynki1$Player, levels = pojedynki1$Player)
    pojedynki1   
  })
  
  output$pojedynki <- renderPlot({
  
  ggplot(pojedynki_filtered(), aes(x=Player, y=Won., label=Won.)) + 
    geom_point(stat='identity', color=team_color(), size=8)  +
    geom_segment(aes(y = 0,
                     yend = Won., 
                     xend = Player), 
                     color = team_color()) +
    ggtitle("% wygranych pojedynków powietrznych") +
    xlab("Zawodnik") + ylab("Różnica w efektywności od 50%") +  
    theme(
      plot.title = element_text(size=14, face="bold"),
      axis.title.x = element_text(size=14, face="bold"),
      axis.title.y = element_text(size=14, face="bold")
    ) +
    coord_flip()
    
  })
  
  wplyw_filtered <- reactive({
    req(input$teams_players2)
    wplyw1 <- filter(wplyw1, Squad %in% input$teams_players2)
    wplyw1   
  })
  
  output$wplyw <- renderPlot({
    ggplot(wplyw_filtered(),aes(x=Min,y=On.Off.1,label=Player)) +
      geom_point(color=team_color()) +
      geom_hline(yintercept=0) +
      geom_text_repel() + labs(title = "geom_text_repel()") +
      labs(fill=NULL) +
      ggtitle("Rozegrane minuty a różnica w xG podczas przebywania na boisku") +
      xlab("Rozegrane minuty") + ylab("Różnica xG") +
      theme(
        legend.position="none",
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
  bramkarze_filtered <- reactive({
    req(input$teams_players2)
    bramkarze_uno$Squad[bramkarze_uno$Squad != input$teams_players2] <- "Aa"
    bramkarze_uno  
  })
  
  # bramkarz_filtered <- reactive({
  #   req(input$teams_players2)
  #   bramkarz <- bramkarze_uno[bramkarze_uno$Squad == input$teams_players2, ]
  #   bramkarz
  # })
  
  team <- reactive({
    team <- input$teams_players2
    team
  })
  
  output$bramkarze <- renderPlot({
    ggplot(bramkarze_filtered(), aes(x=PSxG,y=PSxG...., color=as.factor(Squad), label=Player)) +
      geom_point() +
      scale_color_manual(values = c("grey", team_color())) +
      geom_hline(yintercept=0, linetype="dashed") +
      geom_vline(xintercept=av_psxg, linetype="dashed") +
      geom_text_repel() + labs(title = "geom_text_repel()") +
      labs(fill=NULL) +
      ggtitle("Postawa podstawowego bramkarza") +
      xlab("PSxG/90") + ylab("+/- PSxG/90") +
      theme(
        legend.position="none",
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
  output$Logo2<- renderImage({
    req(input$teams_players_a)
    if(input$teams_players_a=="Arsenal") Leg2<-"www/arsenal.png"
    if(input$teams_players_a=="Aston Villa") Leg2<-"www/astonvilla.png"
    if(input$teams_players_a=="Bournemouth") Leg2<-"www/bournemouth.png"
    if(input$teams_players_a=="Brighton") Leg2<-"www/brighton.png"
    if(input$teams_players_a=="Burnley") Leg2<-"www/burnley.png"
    if(input$teams_players_a=="Chelsea") Leg2<-"www/chelsea.png"
    if(input$teams_players_a=="Crystal Palace") Leg2<-"www/palace.png"
    if(input$teams_players_a=="Everton") Leg2<-"www/everton.png"
    if(input$teams_players_a=="Leicester City") Leg2<-"www/leicester.png"
    if(input$teams_players_a=="Liverpool") Leg2<-"www/liverpool.png"
    if(input$teams_players_a=="Manchester City") Leg2<-"www/mancity.png"
    if(input$teams_players_a=="Manchester Utd") Leg2<-"www/manunited.png"
    if(input$teams_players_a=="Newcastle Utd") Leg2<-"www/newcastle.png"
    if(input$teams_players_a=="Norwich City") Leg2<-"www/norwich.png"
    if(input$teams_players_a=="Sheffield Utd") Leg2<-"www/sheffield.png"
    if(input$teams_players_a=="Southampton") Leg2<-"www/southampton.png"
    if(input$teams_players_a=="Tottenham") Leg2<-"www/spurs.png"
    if(input$teams_players_a=="Watford") Leg2<-"www/watford.png"
    if(input$teams_players_a=="West Ham") Leg2<-"www/westham.png"
    if(input$teams_players_a=="Wolves") Leg2<-"www/wolves.png"
    list(src=Leg2)
  }, deleteFile = FALSE) 
  
  output$Klub2<- renderText({
    req(input$teams_players_a)
    tekst2 <- input$teams_players_a
  }) 
  
  output$Rank2<- renderText({
    req(input$teams_players_a)
    if(input$teams_players_a=="Arsenal") tekstR2<-"8. miejsce"
    if(input$teams_players_a=="Aston Villa") tekstR2<-"17. miejsce"
    if(input$teams_players_a=="Bournemouth") tekstR2<-"18. miejsce"
    if(input$teams_players_a=="Brighton") tekstR2<-"15. miejsce"
    if(input$teams_players_a=="Burnley") tekstR2<-"10. miejsce"
    if(input$teams_players_a=="Chelsea") tekstR2<-"4. miejsce"
    if(input$teams_players_a=="Crystal Palace") tekstR2<-"14. miejsce"
    if(input$teams_players_a=="Everton") tekstR2<-"12. miejsce"
    if(input$teams_players_a=="Leicester City") tekstR2<-"5. miejsce"
    if(input$teams_players_a=="Liverpool") tekstR2<-"1. miejsce"
    if(input$teams_players_a=="Manchester City") tekstR2<-"2. miejsce"
    if(input$teams_players_a=="Manchester Utd") tekstR2<-"3. miejsce"
    if(input$teams_players_a=="Newcastle Utd") tekstR2<-"13. miejsce"
    if(input$teams_players_a=="Norwich City") tekstR2<-"20. miejsce"
    if(input$teams_players_a=="Sheffield Utd") tekstR2<-"9. miejsce"
    if(input$teams_players_a=="Southampton") tekstR2<-"11. miejsce"
    if(input$teams_players_a=="Tottenham") tekstR2<-"6. miejsce"
    if(input$teams_players_a=="Watford") tekstR2<-"19. miejsce"
    if(input$teams_players_a=="West Ham") tekstR2<-"16. miejsce"
    if(input$teams_players_a=="Wolves") tekstR2<-"7. miejsce"
    tekstR2
  })
  
  output$Logo3<- renderImage({
    req(input$teams_players_b)
    if(input$teams_players_b=="Arsenal") Leg3<-"www/arsenal.png"
    if(input$teams_players_b=="Aston Villa") Leg3<-"www/astonvilla.png"
    if(input$teams_players_b=="Bournemouth") Leg3<-"www/bournemouth.png"
    if(input$teams_players_b=="Brighton") Leg3<-"www/brighton.png"
    if(input$teams_players_b=="Burnley") Leg3<-"www/burnley.png"
    if(input$teams_players_b=="Chelsea") Leg3<-"www/chelsea.png"
    if(input$teams_players_b=="Crystal Palace") Leg3<-"www/palace.png"
    if(input$teams_players_b=="Everton") Leg3<-"www/everton.png"
    if(input$teams_players_b=="Leicester City") Leg3<-"www/leicester.png"
    if(input$teams_players_b=="Liverpool") Leg3<-"www/liverpool.png"
    if(input$teams_players_b=="Manchester City") Leg3<-"www/mancity.png"
    if(input$teams_players_b=="Manchester Utd") Leg3<-"www/manunited.png"
    if(input$teams_players_b=="Newcastle Utd") Leg3<-"www/newcastle.png"
    if(input$teams_players_b=="Norwich City") Leg3<-"www/norwich.png"
    if(input$teams_players_b=="Sheffield Utd") Leg3<-"www/sheffield.png"
    if(input$teams_players_b=="Southampton") Leg3<-"www/southampton.png"
    if(input$teams_players_b=="Tottenham") Leg3<-"www/spurs.png"
    if(input$teams_players_b=="Watford") Leg3<-"www/watford.png"
    if(input$teams_players_b=="West Ham") Leg3<-"www/westham.png"
    if(input$teams_players_b=="Wolves") Leg3<-"www/wolves.png"
    list(src=Leg3)
  }, deleteFile = FALSE) 
  
  output$Klub3<- renderText({
    req(input$teams_players_b)
    tekst2 <- input$teams_players_b
  }) 
  
  output$Rank3<- renderText({
    req(input$teams_players_b)
    if(input$teams_players_b=="Arsenal") tekstR3<-"8. miejsce"
    if(input$teams_players_b=="Aston Villa") tekstR3<-"17. miejsce"
    if(input$teams_players_b=="Bournemouth") tekstR3<-"18. miejsce"
    if(input$teams_players_b=="Brighton") tekstR3<-"15. miejsce"
    if(input$teams_players_b=="Burnley") tekstR3<-"10. miejsce"
    if(input$teams_players_b=="Chelsea") tekstR3<-"4. miejsce"
    if(input$teams_players_b=="Crystal Palace") tekstR3<-"14. miejsce"
    if(input$teams_players_b=="Everton") tekstR3<-"12. miejsce"
    if(input$teams_players_b=="Leicester City") tekstR3<-"5. miejsce"
    if(input$teams_players_b=="Liverpool") tekstR3<-"1. miejsce"
    if(input$teams_players_b=="Manchester City") tekstR3<-"2. miejsce"
    if(input$teams_players_b=="Manchester Utd") tekstR3<-"3. miejsce"
    if(input$teams_players_b=="Newcastle Utd") tekstR3<-"13. miejsce"
    if(input$teams_players_b=="Norwich City") tekstR3<-"20. miejsce"
    if(input$teams_players_b=="Sheffield Utd") tekstR3<-"9. miejsce"
    if(input$teams_players_b=="Southampton") tekstR3<-"11. miejsce"
    if(input$teams_players_b=="Tottenham") tekstR3<-"6. miejsce"
    if(input$teams_players_b=="Watford") tekstR3<-"19. miejsce"
    if(input$teams_players_b=="West Ham") tekstR3<-"16. miejsce"
    if(input$teams_players_b=="Wolves") tekstR3<-"7. miejsce"
    tekstR3
  })
  
  team_color_a <- reactive({
    req(input$teams_players_a)
    color <- as.character(paleta$color[paleta$team == input$teams_players_a])
    color
  })
  
  team_color_b <- reactive({
    req(input$teams_players_b)
    color <- as.character(paleta$color[paleta$team == input$teams_players_b])
    color
  })
  
  team_time_filtered_a <- reactive({
    req(input$teams_players_a)
    druzyna_czas <- filter(druzyna_czas, Squad %in% input$teams_players_a)
    druzyna_czas <- druzyna_czas[with(druzyna_czas,order(X90s)),]
    druzyna_czas <- druzyna_czas[(nrow(druzyna_czas)-11):nrow(druzyna_czas),]
    druzyna_czas$Player <- factor(druzyna_czas$Player, levels = druzyna_czas$Player)
    druzyna_czas   
  })
  
  output$team_time_a <- renderPlot({
    ggplot(team_time_filtered_a(), aes(Player, X90s)) + 
      geom_col(fill=team_color_a()) +
      coord_flip() +
      geom_text(aes(label = X90s), hjust = 1.1,fontface = "bold") +
      ggtitle("11 zawodników z największa liczbą rozegranych minut") +
      xlab("Zawodnik") + ylab("90s") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none"
      )
  })
  
  team_time_filtered_b <- reactive({
    req(input$teams_players_b)
    druzyna_czas <- filter(druzyna_czas, Squad %in% input$teams_players_b)
    druzyna_czas <- druzyna_czas[with(druzyna_czas,order(X90s)),]
    druzyna_czas <- druzyna_czas[(nrow(druzyna_czas)-11):nrow(druzyna_czas),]
    druzyna_czas$Player <- factor(druzyna_czas$Player, levels = druzyna_czas$Player)
    druzyna_czas   
  })
  
  output$team_time_b <- renderPlot({
    ggplot(team_time_filtered_b(), aes(Player, X90s)) + 
      geom_col(fill=team_color_b()) +
      coord_flip() +
      geom_text(aes(label = X90s), hjust = 1.1,fontface = "bold") +
      ggtitle("11 zawodników z największa liczbą rozegranych minut") +
      xlab("Zawodnik") + ylab("90s") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.position = "none"
      )
  })
  
  team_goals_filtered_a <- reactive({
    req(input$teams_players_a)
    gole11 <- filter(gole11, Squad %in% input$teams_players_a)
    gole11 <- gole11[with(gole11,order(-Gls,-xG)),]
    gole11 <- gole11[1:10,]
    gole11$Player <- factor(gole11$Player, levels = gole11$Player)
    gole11 <- gole11[,c(1,4:5)]
    gole11 <- melt(gole11)
    gole11   
  })
  
  output$team_goals_a <- renderPlot({
    
    ggplot(team_goals_filtered_a(),aes(Player,value,fill=variable))+
      geom_bar(stat="identity",position="dodge") +
      labs(fill=NULL) +
      scale_fill_manual(labels = c("Gole", "xG"),
                        values=c(team_color_a(),"grey")) +
      ggtitle("Najlepsi strzelcy") +
      xlab("Zawodnik") + ylab("Wartość") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      )
    
  })
  
  team_goals_filtered_b <- reactive({
    req(input$teams_players_b)
    gole11 <- filter(gole11, Squad %in% input$teams_players_b)
    gole11 <- gole11[with(gole11,order(-Gls,-xG)),]
    gole11 <- gole11[1:10,]
    gole11$Player <- factor(gole11$Player, levels = gole11$Player)
    gole11 <- gole11[,c(1,4:5)]
    gole11 <- melt(gole11)
    gole11   
  })
  
  output$team_goals_b <- renderPlot({
    
    ggplot(team_goals_filtered_b(),aes(Player,value,fill=variable))+
      geom_bar(stat="identity",position="dodge") +
      labs(fill=NULL) +
      scale_fill_manual(labels = c("Gole", "xG"),
                        values=c(team_color_b(),"grey")) +
      ggtitle("Najlepsi strzelcy") +
      xlab("Zawodnik") + ylab("Wartość") +
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      )
    
  })
  
  pojedynki_filtered_a <- reactive({
    req(input$teams_players_a)
    pojedynki1 <- filter(pojedynki1, Squad %in% input$teams_players_a)
    pojedynki1 <- pojedynki1[with(pojedynki1,order(Won.)),]
    pojedynki1$Player <- factor(pojedynki1$Player, levels = pojedynki1$Player)
    pojedynki1   
  })
  
  output$pojedynki_a <- renderPlot({
    
    ggplot(pojedynki_filtered_a(), aes(x=Player, y=Won., label=Won.)) + 
      geom_point(stat='identity', color=team_color_a(), size=8)  +
      geom_segment(aes(y = 0,
                       yend = Won., 
                       xend = Player), 
                   color = team_color_a()) +
      ggtitle("% wygranych pojedynków powietrznych") +
      xlab("Zawodnik") + ylab("Różnica w efektywności od 50%") +  
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      ) +
      coord_flip()
    
  })
  
  pojedynki_filtered_b <- reactive({
    req(input$teams_players_b)
    pojedynki1 <- filter(pojedynki1, Squad %in% input$teams_players_b)
    pojedynki1 <- pojedynki1[with(pojedynki1,order(Won.)),]
    pojedynki1$Player <- factor(pojedynki1$Player, levels = pojedynki1$Player)
    pojedynki1   
  })
  
  output$pojedynki_b <- renderPlot({
    
    ggplot(pojedynki_filtered_b(), aes(x=Player, y=Won., label=Won.)) + 
      geom_point(stat='identity', color=team_color_b(), size=8)  +
      geom_segment(aes(y = 0,
                       yend = Won., 
                       xend = Player), 
                   color = team_color_b()) +
      ggtitle("% wygranych pojedynków powietrznych") +
      xlab("Zawodnik") + ylab("Różnica w efektywności od 50%") +  
      theme(
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      ) +
      coord_flip()
    
  })
  
  wplyw_filtered_a <- reactive({
    req(input$teams_players_a)
    wplyw1 <- filter(wplyw1, Squad %in% input$teams_players_a)
    wplyw1   
  })
  
  output$wplyw_a <- renderPlot({
    ggplot(wplyw_filtered_a(),aes(x=Min,y=On.Off.1,label=Player)) +
      geom_point(color=team_color_a()) +
      geom_hline(yintercept=0) +
      geom_text_repel() + labs(title = "geom_text_repel()") +
      labs(fill=NULL) +
      ggtitle("Rozegrane minuty a różnica w xG podczas przebywania na boisku") +
      xlab("Rozegrane minuty") + ylab("Różnica xG") +
      theme(
        legend.position="none",
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
  wplyw_filtered_b <- reactive({
    req(input$teams_players_b)
    wplyw1 <- filter(wplyw1, Squad %in% input$teams_players_b)
    wplyw1   
  })
  
  output$wplyw_b <- renderPlot({
    ggplot(wplyw_filtered_b(),aes(x=Min,y=On.Off.1,label=Player)) +
      geom_point(color=team_color_b()) +
      geom_hline(yintercept=0) +
      geom_text_repel() + labs(title = "geom_text_repel()") +
      labs(fill=NULL) +
      ggtitle("Rozegrane minuty a różnica w xG podczas przebywania na boisku") +
      xlab("Rozegrane minuty") + ylab("Różnica xG") +
      theme(
        legend.position="none",
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
  bramkarze_filtered_ab <- reactive({
    req(input$teams_players_a)
    req(input$teams_players_b)
    bramkarze_uno$Squad[(bramkarze_uno$Squad != input$teams_players_a) & (bramkarze_uno$Squad != input$teams_players_b)] <- "Aa"
    bramkarze_uno$Squad[bramkarze_uno$Squad == input$teams_players_a] <- "Bb"
    bramkarze_uno$Squad[bramkarze_uno$Squad == input$teams_players_b] <- "Cc"
    bramkarze_uno
  })
  
  output$bramkarze_ab <- renderPlot({
    ggplot(bramkarze_filtered_ab(), aes(x=PSxG,y=PSxG...., color=as.factor(Squad), label=Player)) +
      geom_point() +
      scale_color_manual(values = c("grey", team_color_a(), team_color_b())) +
      geom_hline(yintercept=0, linetype="dashed") +
      geom_vline(xintercept=av_psxg, linetype="dashed") +
      geom_text_repel() + labs(title = "geom_text_repel()") +
      labs(fill=NULL) +
      ggtitle("Postawa podstawowych bramkarzy") +
      xlab("PSxG/90") + ylab("+/- PSxG/90") +
      theme(
        legend.position="none",
        plot.title = element_text(size=14, face="bold"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")
      )
  })
  
}

shinyApp(ui = ui, server = server)

