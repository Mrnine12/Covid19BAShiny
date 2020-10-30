library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)
#library(plotly)
library(readxl)
library(scales)
library(stringr)
library(shiny)
library(shinydashboard)

casos = read.csv("https://raw.githubusercontent.com/elhenrico/covid19-Brazil-timeseries/master/confirmed-cases.csv")
casos_novos = read.csv("https://raw.githubusercontent.com/elhenrico/covid19-Brazil-timeseries/master/confirmed-new.csv")
mortes = read.csv("https://raw.githubusercontent.com/elhenrico/covid19-Brazil-timeseries/master/deaths.csv")
mortes_novos = read.csv("https://raw.githubusercontent.com/elhenrico/covid19-Brazil-timeseries/master/deaths-new.csv")
recuperados = read.csv("https://raw.githubusercontent.com/Mrnine12/Covid19BAShiny/master/Recuperados.csv", sep = ";")

CIDADES = read_excel("Covid19.xlsx", sheet = 2)
latlogBR = read_excel("Covid19.xlsx", sheet = 7)

moveis = function(x){
  moveis = as.numeric()
  for(i in 1:(length(x)-7)){
    moveis[i] = mean(x[i:(7+i-1)])
  }
  return(moveis)
}

texto = function(x, casa){
  if(casa == "mil"){
    resul = round(seq(1, max(x), length.out = 4)/1000)
  }
  else{
    resul = round(seq(1, max(x), length.out = 4)/1000000)
  }
  return(resul)
}

datas = seq(from = as.Date("2020-02-26"), to = as.Date(Sys.Date()), by = "day")[1:(ncol(casos)-2)]

BRASIL = data.frame(Data = datas, Casos = as.numeric(casos_novos[1,3:ncol(casos_novos)]),
                    Mortes = as.numeric(mortes_novos[1,3:ncol(mortes_novos)]))

BAHIA = data.frame(Data = datas, Casos = as.numeric(casos_novos[11,3:ncol(casos_novos)]),
                    Mortes = as.numeric(mortes_novos[11,3:ncol(mortes_novos)]))

info = list(ATT = paste0("Atualizado às 00:00 de ", Sys.Date()), 
            BR_Recuperados = recuperados$Brasil[nrow(recuperados)], BA_Recuperados = recuperados$Bahia[nrow(recuperados)])

## Shiny

header = dashboardHeader(title = "COVID-19")

sidebar = dashboardSidebar(disable = T)

body = dashboardBody(
  
  fluidPage(
    box(width = 4, height = 620, 
        valueBoxOutput(width = 12, outputId = "Casos"), 
        valueBoxOutput(width = 12, outputId = "Obitos"),
        valueBoxOutput(width = 12, outputId = "Letal"),
        valueBoxOutput(width = 12, outputId = "Ativos"),
        valueBoxOutput(width = 12, outputId = "Recuperados")),
    
    box(width = 8, title = strong("Mapa do COVID-19 na Bahia"), 
                footer = info$ATT, leafletOutput("MapaBA", height = 515))),
  
  fluidPage(
    box(title = "Bahia", width = 12, height = 760, plotOutput("L_casos_BA", height = 350), 
        plotOutput("L_obitos_BA", height = 350))),
  
  fluidPage(
    box(width = 4, height = 620, 
        valueBoxOutput(width = 12, outputId = "BR_Casos"), 
        valueBoxOutput(width = 12, outputId = "BR_Obitos"),
        valueBoxOutput(width = 12, outputId = "BR_Letal"),
        valueBoxOutput(width = 12, outputId = "BR_Ativos"),
        valueBoxOutput(width = 12, outputId = "BR_Recuperados")),
    
    box(width = 8, title = strong("Mapa do COVID-19 no Brasil"), 
        footer = info$ATT, leafletOutput("MapaBR", height = 515))),
  
  fluidPage(
    box(title = "Brasil", width = 12, height = 760, plotOutput("L_casos_BR", height = 350), 
        plotOutput("L_obitos_BR", height = 350))),
  
  fluidPage(box(width = 12, title = strong("Fonte"), tags$p("Coronavírus Brasil:", tags$a("http://covid.saude.gov.br/")),
                tags$p("Ministério da Saúde:", tags$a("https://saude.gov.br/")), 
                tags$p("Secretaria da Saúde da Bahia:", tags$a("http://www.saude.ba.gov.br"))),
            box(width = 12, tags$p("Desenvolvido por Jonatha Pimentel, Universidade Federal da Bahia - UFBA, 
                                             Laboratório de Estatística e Data Science - LED."), 
                tags$p(strong("LinkedIn: "), tags$a("https://linkedin/in/jspimentel"), ". "), 
                tags$p(strong("Blog: "), tags$a("https://blog-jonatha.herokuapp.com/"), ".")))
)

server = function(input, output){

  output$Casos = renderValueBox({
    valueBox(value = number(casos[11,ncol(casos)]), subtitle = "Casos Confirmados",
             color = "blue")
  })
  output$Obitos = renderValueBox({
    valueBox(value = number(mortes[11,ncol(mortes)]), subtitle = "Óbitos", 
             color = "blue")
  })
  output$Letal = renderValueBox({
    valueBox(value = percent(round(mortes[11,ncol(mortes)]/casos[11,ncol(casos)], 4)),
             subtitle = "Taxa de Letalidade", color = "blue")
  })
  output$Ativos = renderValueBox({
    valueBox(value = number(casos[11,ncol(casos)]-mortes[11,ncol(mortes)]-info$BA_Recuperados), 
             subtitle = "Casos Ativos", color = "blue")
  })
  output$Recuperados = renderValueBox({
    valueBox(value = number(info$BA_Recuperados), subtitle = "Recuperados", color = "blue")
  })
  
  output$L_casos_BA = renderPlot({
    ggplot(BAHIA, aes(x = Data)) + 
      geom_bar(aes(weight = Casos), position = "dodge", fill = "blue", color = "black") +
      geom_line(aes(y = c(rep(1,7),moveis(Casos))), size = 1.5) + theme_light() +
      labs(title = "Casos diários", x = "", y = "Casos confirmados (em milhares)") +
      scale_y_continuous(labels = texto(BAHIA$Casos, "mil"), breaks = texto(BAHIA$Casos, "mil")*1000)
  })
  output$L_obitos_BA = renderPlot({
    ggplot(BAHIA, aes(x = Data)) + 
      geom_bar(aes(weight = Mortes), position = "dodge", fill = "blue", color = "black") +
      geom_line(aes(y = c(rep(0,7),moveis(Mortes))), size = 1.5) + theme_light() +
      labs(title = "Óbitos diários", x = "", y = "Óbitos confirmados")
  })
  
  output$BR_Casos = renderValueBox({
    valueBox(value = number(casos[1,ncol(casos)]), subtitle = "Casos Confirmados",
             color = "blue")
  })
  output$BR_Obitos = renderValueBox({
    valueBox(value = number(mortes[1,ncol(mortes)]), subtitle = "Óbitos", color = "blue")
  })
  output$BR_Letal = renderValueBox({
    valueBox(value = percent(round(mortes[1,ncol(mortes)]/casos[1,ncol(casos)], 4)), 
             subtitle = "Taxa de Letalidade", color = "blue")
  })
  output$BR_Ativos = renderValueBox({
    valueBox(value = number(casos[1,ncol(casos)]-mortes[1,ncol(mortes)]-info$BR_Recuperados),
             subtitle = "Casos Ativos", color = "blue")
  })
  output$BR_Recuperados = renderValueBox({
    valueBox(value = number(info$BR_Recuperados), subtitle = "Recuperados", color = "blue")
  })

  output$L_casos_BR = renderPlot({
    ggplot(BRASIL, aes(x = Data)) + 
      geom_bar(aes(weight = Casos), position = "dodge", fill = "blue", color = "black") +
      geom_line(aes(y = c(rep(1,7),moveis(Casos))), size = 1.5) + theme_light() +
      labs(title = "Casos diários", x = "", y = "Casos confirmados (em milhares)") +
      scale_y_continuous(labels = texto(BRASIL$Casos, "mil"), breaks = texto(BRASIL$Casos, "mil")*1000)
  })
  output$L_obitos_BR = renderPlot({
    ggplot(BRASIL, aes(x = Data)) + 
      geom_bar(aes(weight = Mortes), position = "dodge", fill = "blue", color = "black") +
      geom_line(aes(y = c(rep(0,7),moveis(Mortes))), size = 1.5) + theme_light() +
      labs(title = "Óbitos diários", x = "", y = "Óbitos confirmados")
  })
  
  output$MapaBA = renderLeaflet({
    leaflet() %>% addTiles(group = "Padrão") %>% setView(-42, -14, 6) %>% 
      addCircleMarkers(as.numeric(CIDADES$Longitude), as.numeric(CIDADES$Latitude), color = "red",
                       radius = ifelse(CIDADES$Ativos > 2000, 30, CIDADES$Ativos*0.01), group = "Casos Ativos") %>% 
      addCircleMarkers(as.numeric(CIDADES[CIDADES$Obitos>0,]$Longitude), as.numeric(CIDADES[CIDADES$Obitos>0,]$Latitude),
                       color = "black", radius = CIDADES[CIDADES$Obitos>0,]$Obitos*0.01, group = "Óbitos") %>%
      addCircleMarkers(as.numeric(CIDADES[CIDADES$Ativos==0,]$Longitude), as.numeric(CIDADES[CIDADES$Ativos==0,]$Latitude),
                       color = "darkorange", radius = 3, group = "Sem Casos Ativos") %>%
      addLegend(position = "bottomright", colors = c('black', 'red', 'darkorange'), labels = c('Óbitos','Casos Ativos',"Sem Casos Ativos"), opacity = 1) %>%
      addEasyButton(easyButton(icon = "fa-globe", title = "", onClick = JS("function(btn, map){ map.setZoom(5.5); }"))) %>% 
      addEasyButton(easyButton(icon = "fa-crosshairs", title = "Localize-se", onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
      addLayersControl(baseGroups = c("Padrão"), overlayGroups = c("Casos Ativos", "Óbitos", "Sem Casos Ativos"), options = layersControlOptions(collapsed = FALSE))
  })
  output$MapaBR = renderLeaflet({
    muni_map = data.frame(sigla = casos[,2], casos = casos[,ncol(casos)],
                           mortes = mortes[,ncol(mortes)], 
                           novos_casos = casos_novos[,ncol(casos_novos)],
                           novas_mortes = mortes_novos[,ncol(mortes_novos)]) %>% 
      left_join(latlogBR, by = c("sigla" = "Sigla"))
    
    leaflet() %>% addTiles(group = "Padrão") %>% #setView(-42, -14, 6) %>% 
      addCircleMarkers(as.numeric(muni_map$Longitude), as.numeric(muni_map$Latitude), color = "red",
                       radius = muni_map$casos*0.00005, group = "Casos Acumulados") %>%
      addCircleMarkers(as.numeric(muni_map$Longitude), as.numeric(muni_map$Latitude), color = "black",
                       radius = muni_map$mortes*0.0005, group = "Óbitos Acumulados") %>%
      addCircleMarkers(as.numeric(muni_map$Longitude), as.numeric(muni_map$Latitude), color = "darkorange",
                       radius = muni_map$novos_casos*0.005, group = "Casos Diário") %>%
      addCircleMarkers(as.numeric(muni_map$Longitude), as.numeric(muni_map$Latitude), color = "darkblue",
                       radius = muni_map$novas_mortes*0.1, group = "Óbitos Diário") %>%
      addLegend(position = "bottomright", colors = c('red', 'black', 'darkorange', 'darkblue'), 
                labels = c('Casos Acumulados','Óbitos Acumulados','Casos Diário',"Óbitos Diário"), opacity = 1) %>%
      addEasyButton(easyButton(icon = "fa-globe", title = "", onClick = JS("function(btn, map){ map.setZoom(5.5); }"))) %>% 
      addEasyButton(easyButton(icon = "fa-crosshairs", title = "Localize-se", onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
      addLayersControl(baseGroups = c("Padrão"), overlayGroups = c("Casos Acumulados", "Óbitos Acumulados", "Casos Diário", "Óbitos Diário"), 
                       options = layersControlOptions(collapsed = FALSE))
  })
  
}

ui = dashboardPage(skin = "blue", header, sidebar, body)

shinyApp(ui, server) # APP SHINY
