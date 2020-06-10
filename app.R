#setwd("C:\\Users\\Jsp_P\\Documents\\Covid19")
#devtools::install_github("italocegatta/brmap")

library(dplyr)
library(ggplot2)
library(leaflet)
library(readxl)
library(scales)
library(shiny)
library(shinydashboard)

MSDIARIO = read_excel("Covid19.xlsx", sheet = 1)
BAHIA = read_excel("Covid19.xlsx", sheet = 2)
DIARIO = read_excel("Covid19.xlsx", sheet = 3)
INFO = read_excel("Covid19.xlsx", sheet = 4)
ATIVOS = read_excel("Covid19.xlsx", sheet = 5)

options(scipen = 10)

info = list(Local = BAHIA$Cidade, ATT = "Atualizado às 19:30 de 09/06/2020")

header = dashboardHeader(title = "COVID-19")

sidebar = dashboardSidebar(disable = T)

body = dashboardBody(
  
  fluidPage(
    box(width = 8, title = strong("Mapa do COVID-19 na Bahia"), 
        footer = info$ATT, 
        leafletOutput("MapaBA", height = 645)),
    
    box(width = 4, height = 480, 
        plotOutput("donuts1"), title = strong("Situação dos casos")),
    box(width = 4, height = 250, 
        valueBoxOutput(width = 12, outputId = "Enfer"),
        valueBoxOutput(width = 12, outputId = "UTI"))),
  
#  fluidPage(
#    box(width = 12, title = strong("Distribuição dos casos e óbitos na Bahia"),
#        div(style = 'overflow-x: scroll', DT::dataTableOutput('tbl')))),  
  
  fluidPage(
    box(width = 4, height = 700,
        selectInput(inputId = "SelBA", label = "", choices = c("Casos", "Óbitos", "Recuperados", "Casos Ativos")),
        valueBoxOutput(width = 12, outputId = "value_1"),
        valueBoxOutput(width = 12, outputId = "value_2"),
        valueBoxOutput(width = 12, outputId = "value_3"),
        valueBoxOutput(width = 12, outputId = "value_4"),
        valueBoxOutput(width = 12, outputId = "value_5")),
    box(width = 8, height = 700,
        plotOutput("Linhas1BA", height = 340),
        plotOutput("Linhas2BA", height = 340)),
    
    box(width = 12, height = 420, plotOutput("BarrasBA"))),
  
  fluidPage(
    box(width = 4, height = 700,
        selectInput(inputId = "SelBR", label = "", choices = c("Casos", "Óbitos", "Recuperados", "Casos Ativos")),
        valueBoxOutput(width = 12, outputId = "value_1BR"),
        valueBoxOutput(width = 12, outputId = "value_2BR"),
        valueBoxOutput(width = 12, outputId = "value_3BR"),
        valueBoxOutput(width = 12, outputId = "value_4BR"),
        valueBoxOutput(width = 12, outputId = "value_5BR")),
    box(width = 8, height = 700, 
        plotOutput("LinhasBR1", height = 340), 
        plotOutput("LinhasBR2", height = 340))),
  
  fluidPage(
    box(width = 12, title = strong("Fontes"), 
        tags$p("Secretaria da Saúde da Bahia:", tags$a("http://www.saude.ba.gov.br")), 
        tags$p("Ministério da Saúde:", tags$a("https://saude.gov.br/"))),
    
    box(width = 12, tags$p("Desenvolvido por Jonatha Pimentel, Universidade Federal da Bahia - UFBA, 
                           Laboratório de Estatística e Data Science - LED."),
        tags$p(strong("LinkedIn: "), tags$a("https://linkedin/in/jspimentel"), ". "),
        tags$p(strong("Blog: "), tags$a("https://blog-jonatha.herokuapp.com/"), ".")))
)

server = function(input, output){
  
  output$value_1 = renderValueBox({
    valueBox(value = paste0(number(DIARIO$Casos[nrow(DIARIO)]), " (", number(DIARIO$Casos[nrow(DIARIO)] - DIARIO$Casos[nrow(DIARIO)-1]), ")"), 
             subtitle = "Casos (Novos)", color = "yellow")
  })
  output$value_2 = renderValueBox({
    valueBox(value = paste0(number(DIARIO$Obitos[nrow(DIARIO)]), " (", number(DIARIO$Obitos[nrow(DIARIO)] - DIARIO$Obitos[nrow(DIARIO)-1]), ")"), 
             subtitle = "Óbitos (Novos)", color = "yellow")
  })
  output$value_3 = renderValueBox({
    valueBox(value = number(as.numeric(INFO[5,2])), 
             subtitle = "Profissionais de Saúde Contaminados", color = "yellow")
  })
  output$value_4 = renderValueBox({
    valueBox(value = paste0(number(DIARIO$Recuperados[nrow(DIARIO)]), " (", number(DIARIO$Recuperados[nrow(DIARIO)] - DIARIO$Recuperados[nrow(DIARIO)-1]), ")"), 
             subtitle = "Recuperados (Novos)", color = "yellow")
  })
  output$value_5 = renderValueBox({
    valueBox(value = paste0(number(DIARIO$Ativos[nrow(DIARIO)]), " (", number(DIARIO$Ativos[nrow(DIARIO)] - DIARIO$Ativos[nrow(DIARIO)-1]), ")"), 
             subtitle = "Casos Ativos (Novos)", color = "yellow")
  })
  
  output$Enfer = renderValueBox({
    valueBox(value = paste0(number(as.numeric(INFO[1,2])), " (", round(as.numeric(INFO[1,2])/as.numeric(INFO[3,2]),2)*100, "%)"), 
             subtitle = "Leitos Ocupados", color = "yellow")
  })
  output$UTI = renderValueBox({
    valueBox(value = paste0(number(as.numeric(INFO[2,2])), " (", round(as.numeric(INFO[2,2])/as.numeric(INFO[4,2]),2)*100, "%)"), 
             subtitle = "UTIs Ocupadas", color = "yellow")
  })
  
  output$donuts1 = renderPlot({
    Total = c(DIARIO$Obitos[nrow(DIARIO)], DIARIO$Recuperados[nrow(DIARIO)], DIARIO$Ativos[nrow(DIARIO)])
    
    count.data = data.frame(class = c("Óbitos", "Recuperados", "Ativos"), 
                            Abs = Total, Porc = round(Total/sum(Total),4)) %>% 
      arrange(desc(class)) %>%  mutate(lab.ypos = cumsum(Porc) - 0.5*Porc)
    
    ggplot(count.data, aes(x = 2, y = Porc, fill = class)) +
      geom_bar(stat = "identity", color = "black") + coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Dark2", name = "") + theme_void() + xlim(0.5, 2.5) + 
      geom_text(aes(y = lab.ypos, label = percent(Porc)), color = "white", size = 5) + 
      theme(legend.position = "bottom", legend.text = element_text(size = 15))
  })

  output$MapaBA = renderLeaflet({
    leaflet() %>% addTiles(group = "Padrão") %>% setView(-42, -14, 6) %>% 
      addCircleMarkers(as.numeric(BAHIA$Longitude), as.numeric(BAHIA$Latitude), color = "red",
                       radius = ifelse(BAHIA$Casos > 2000, 30, BAHIA$Casos*0.020), group = "Casos") %>% 
      addCircleMarkers(as.numeric(BAHIA[BAHIA$Obitos>0,]$Longitude), as.numeric(BAHIA[BAHIA$Obitos>0,]$Latitude),
                       color = "black", radius = BAHIA[BAHIA$Obitos>0,]$Obitos*0.01, group = "Óbitos") %>% 
      addLegend(position = "bottomright", colors = c('black', 'red'), labels = c('Óbitos','Casos'), opacity = 1) %>%
      addEasyButton(easyButton(icon = "fa-globe", title = "", onClick = JS("function(btn, map){ map.setZoom(5.5); }"))) %>% 
      addEasyButton(easyButton(icon = "fa-crosshairs", title = "Localize-se", onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
      addLayersControl(baseGroups = c("Padrão"), overlayGroups = c("Casos", "Óbitos"), options = layersControlOptions(collapsed = FALSE))
  })
  output$tbl = DT::renderDataTable({
    coltopo = JS("function(settings, json) {","$(this.api().table().header()).css(
          {'background-color': 'orange', 'color': 'white'});", "}")
    
    DT::datatable(data.frame(info$Local, BAHIA[,c(2:3)],
                             (number(BAHIA$Populacao)),
                             round(BAHIA$IncidenciaCasos,2)), 
                  filter = 'none', options = list(autoWidth = T, initComplete = coltopo,
                  pageLength = 10, lengthMenu = c(5, 10, nrow(BAHIA)),
                  columnDefs = list(list(className = 'dt-left', targets = 1), 
                                    list(className = "dt-right", targets = 2:5))),
                  colnames = c('Cidades', 'Casos', 'Óbitos','População',
                               'Coeficiente de incidência de casos por 1 milhão de habitantes'))
  })
  output$BarrasBA = renderPlot({

    GRAFBAR = data.frame(ATIVOS[1:11,])
    GRAFBAR = GRAFBAR[order(GRAFBAR$Cidade, decreasing = c(FALSE)), ]
  
    ggplot(GRAFBAR, aes(x = Cidade)) + geom_bar(aes(weight = Casos.Ativos), fill = "orange") +
      ggtitle("Distribuição de Casos Ativos na Bahia - Cidades com mais casos") +
      labs(y = "Casos", x = "Cidades") + theme_classic() + annotate("text", x = 1:11, 
      y = GRAFBAR$Casos.Ativos+250, label = GRAFBAR$Casos.Ativos) 
#      geom_text(label = GRAFBAR$Casos.Ativos, stat = "count", vjust = .5) 
  })
  output$Linhas1BA = renderPlot({
    
    if(input$SelBA == "Casos"){
      ggplot(DIARIO, aes(x = Data)) + geom_bar(aes(weight = Casos), position = "dodge", 
        fill = "orange", color = "black") + labs(title = "Evolução no número de casos confirmados - Acumulado") + 
        theme_classic() + labs(y = "Casos Confirmados", x = "")
    }
    else{
      if(input$SelBA == "Óbitos"){
        ggplot(DIARIO, aes(x = Data)) + geom_bar(aes(weight = Obitos), position = "dodge", 
          fill = "orange", color = "black") + labs(title = "Evolução no número de óbitos confirmados - Acumulado") + 
          theme_classic() + labs(y = "Óbitos Confirmados", x = "")
        
      }
      else{
        if(input$SelBA == "Recuperados"){
          ggplot(DIARIO, aes(x = Data)) + geom_bar(aes(weight = Recuperados), position = "dodge", 
            fill = "orange", color = "black") + labs(title = "Evolução no número de pessoas recuperadas - Acumulado") + 
            theme_classic() + labs(y = "Pessoas Recuperadas", x = "")
        }
        else{
          ggplot(DIARIO, aes(x = Data)) + geom_bar(aes(weight = Ativos), position = "dodge", 
            fill = "orange", color = "black") + labs(title = "Evolução no número de casos ativos - Acumulado") + 
            theme_classic() + labs(y = "Casos Ativos", x = "")
          
        }
      }
    }
    
  })
  output$Linhas2BA = renderPlot({
    
    n = nrow(DIARIO)
    TRABALHO = data.frame(Data = DIARIO$Data,
                          Casos = c(1,DIARIO$Casos[2:n]-DIARIO$Casos[1:(n-1)]),
                          Obitos = c(0,DIARIO$Obitos[2:n]-DIARIO$Obitos[1:(n-1)]),
                          Recuperados = c(0,DIARIO$Recuperados[2:n]-DIARIO$Recuperados[1:(n-1)]),
                          Ativos = c(1,DIARIO$Ativos[2:n]-DIARIO$Ativos[1:(n-1)]))
    
    if(input$SelBA == "Casos"){
      ggplot(TRABALHO, aes(x = Data)) + geom_bar(aes(weight = Casos), position = "dodge", 
        fill = "orange", color = "black") + labs(title = "Número de casos confirmados - diário") + 
        theme_classic() + labs(y = "Casos Confirmados", x = "")
    }
    else{
      if(input$SelBA == "Óbitos"){
        ggplot(TRABALHO, aes(x = Data)) + geom_bar(aes(weight = Obitos), position = "dodge", 
          fill = "orange", color = "black") + labs(title = "Número de óbitos confirmados - diário") + 
          theme_classic() + labs(y = "Óbitos Confirmados", x = "")
      }
      else{
        if(input$SelBA == "Recuperados"){
          ggplot(TRABALHO, aes(x = Data)) + geom_bar(aes(weight = Recuperados), position = "dodge", 
            fill = "orange", color = "black") + labs(title = "Número de pessoas recuperadas - diário") + 
            theme_classic() + labs(y = "Pessoas Recuperadas", x = "")
        }
        else{
          ggplot(TRABALHO, aes(x = Data)) + geom_bar(aes(weight = Ativos), position = "dodge", 
            fill = "orange", color = "black") + labs(title = "Número de casos ativos - diário") + 
            theme_classic() + labs(y = "Casos Ativos", x = "")
        }
      }
    }
  })

  output$value_1BR = renderValueBox({
    valueBox(value = paste0(number(MSDIARIO$Casos[nrow(MSDIARIO)]), " (", number(MSDIARIO$Casos[nrow(MSDIARIO)] - MSDIARIO$Casos[nrow(MSDIARIO)-1]), ")"), 
             subtitle = "Casos Confirmados (Novos)", color = "yellow")
  })
  output$value_2BR = renderValueBox({
    valueBox(value = paste0(number(MSDIARIO$Obitos[nrow(MSDIARIO)]), " (", number(MSDIARIO$Obitos[nrow(MSDIARIO)] - MSDIARIO$Obitos[nrow(MSDIARIO)-1]), ")"), 
             subtitle = "Óbitos Confirmados (Novos)", color = "yellow")
  })
  output$value_3BR = renderValueBox({
    valueBox(value = paste0(round(MSDIARIO[nrow(MSDIARIO),3]/MSDIARIO[nrow(MSDIARIO),2],3)*100, "%"), 
             subtitle = "Taxa de Letalidade", color = "yellow")
  })
  output$value_4BR = renderValueBox({
    valueBox(value = paste0(number(MSDIARIO$Recuperados[nrow(MSDIARIO)]), " (", number(MSDIARIO$Recuperados[nrow(MSDIARIO)] - MSDIARIO$Recuperados[nrow(MSDIARIO)-1]), ")"), 
             subtitle = "Recuperados (Novos)", color = "yellow")
  })
  output$value_5BR = renderValueBox({
    valueBox(value = paste0(number(MSDIARIO$Ativos[nrow(MSDIARIO)]), " (", number(MSDIARIO$Ativos[nrow(MSDIARIO)] - MSDIARIO$Ativos[nrow(MSDIARIO)-1]), ")"), 
             subtitle = "Casos Ativos (Novos)", color = "yellow")
  })
  
  output$LinhasBR1 = renderPlot({
    
    MSDIARIO2 = MSDIARIO[MSDIARIO$Casos > 0,]
    
    if(input$SelBR == "Casos"){
      ggplot(MSDIARIO2, aes(x = Data)) + geom_bar(aes(weight = Casos), position = "dodge", 
        fill = "orange", color = "black") + labs(title = "Evolução no número de casos confirmados - Acumulado") + 
        theme_classic() + labs(y = "Casos Confirmados", x = "")
    }
    else{
      if(input$SelBR == "Óbitos"){
        ggplot(MSDIARIO2, aes(x = Data)) + geom_bar(aes(weight = Obitos), position = "dodge", 
          fill = "orange", color = "black") + labs(title = "Evolução no número de óbitos confirmados - Acumulado") + 
          theme_classic() + labs(y = "Óbitos Confirmados", x = "")
      }
      else{
        MSDIARIO2 = MSDIARIO2[MSDIARIO2$Recuperados > 0,]
        
        if(input$SelBR == "Recuperados"){
          ggplot(MSDIARIO2, aes(x = Data)) + geom_bar(aes(weight = Recuperados), position = "dodge", 
            fill = "orange", color = "black") + labs(title = "Evolução no número de pessoas recuperadas - Acumulado") + 
            theme_classic() + labs(y = "Pessoas Recuperados", x = "")
        }
        else{
          ggplot(MSDIARIO2, aes(x = Data)) + geom_bar(aes(weight = Ativos), position = "dodge", 
            fill = "orange", color = "black") + labs(title = "Evolução no número de casos ativos - Acumulado") + 
            theme_classic() + labs(y = "Casos Ativos", x = "")
        }
      }
    }
    
  })
  output$LinhasBR2 = renderPlot({
    
    MSDIARIO2 = MSDIARIO[MSDIARIO$Casos > 0,]
    
    n = nrow(MSDIARIO2)
    TRABALHO = data.frame(Data = MSDIARIO2$Data,
                          Casos = c(1,MSDIARIO2$Casos[2:n]-MSDIARIO2$Casos[1:(n-1)]),
                          Obitos = c(0,MSDIARIO2$Obitos[2:n]-MSDIARIO2$Obitos[1:(n-1)]),
                          Recuperados = c(0,MSDIARIO2$Recuperados[2:n]-MSDIARIO2$Recuperados[1:(n-1)]),
                          Ativos = c(0,MSDIARIO2$Ativos[2:n]-MSDIARIO2$Ativos[1:(n-1)]))
    
    if(input$SelBR == "Casos"){
      ggplot(TRABALHO, aes(x = Data)) + geom_bar(aes(weight = Casos), position = "dodge", 
        fill = "orange", color = "black") + labs(title = "Evolução no número de casos confirmados - Diário") + 
        theme_classic() + labs(y = "Casos Confirmados", x = "")
    }
    else{
      if(input$SelBR == "Óbitos"){
        ggplot(TRABALHO, aes(x = Data)) + geom_bar(aes(weight = Obitos), position = "dodge", 
          fill = "orange", color = "black") + labs(title = "Evolução no número de óbitos confirmados - Diário") + 
          theme_classic() + labs(y = "Óbitos Confirmados", x = "")
      }
      else{
        TRABALHO = TRABALHO[-c(1:54),]
        
        if(input$SelBR == "Recuperados"){
          ggplot(TRABALHO, aes(x = Data)) + geom_bar(aes(weight = Recuperados), position = "dodge", 
            fill = "orange", color = "black") + labs(title = "Evolução no número de pessoas recuperadas - Diário") + 
            theme_classic() + labs(y = "Pessoas Recuperados", x = "")
        }
        else{
          ggplot(TRABALHO, aes(x = Data)) + geom_bar(aes(weight = Ativos), position = "dodge", 
            fill = "orange", color = "black") + labs(title = "Evolução no número de casos ativos - Diário") + 
            theme_classic() + labs(y = "Casos Ativos", x = "")
        }
      }
    }
  })
  
}

ui = dashboardPage(skin = "yellow", header, sidebar, body)

shinyApp(ui, server) # APP SHINY
