#setwd("C:\\Users\\Jsp_P\\Documents\\Covid19")
#devtools::install_github("italocegatta/brmap")

library(brmap)
library(dplyr)
library(ggplot2)
library(leaflet)
library(readxl)
library(scales)
library(shiny)
library(shinydashboard)

MSESTADO = read_excel("Covid19.xlsx", sheet = 1)
MSDIARIO = read_excel("Covid19.xlsx", sheet = 2)
BAHIA = read_excel("Covid19.xlsx", sheet = 4)
DIARIO = read_excel("Covid19.xlsx", sheet = 5)
INFO = read_excel("Covid19.xlsx", sheet = 6)

info = list(Local = c("Abaíra","Acajutiba","Adustina","Água Fria","Aiquara","Alagoinhas","Almadina","Amélia Rodrigues","Araci","Aurelino Leal","Barra",
                      "Barra do Choça","Barra do Rocha","Barreiras","Barro Preto","Belmonte","Brumado","Buerarema","Cachoeira","Caetanos","Caldeirão Grande",
                      "Camacan","Camaçari","Camamu","Campo Alegre de Lourdes","Campo Formoso","Canarana","Canavieiras","Candeias","Cansanção","Capim Grosso",
                      "Castro Alves","Catu","Coaraci","Conceição do Coité","Conceição do Jacuípe","Conde","Coração de Maria","Cravolândia","Cruz das Almas",
                      "Curaçá","Dias D'Ávila","Euclides da Cunha","Eunápolis","Feira de Santana","Floresta Azul","Gandu","Gongogi","Ibicaraí","Ibiritaia",
                      "Ibotirama","Ilhéus","Ipiaú","Ipirá","Irecê","Itabela","Itaberaba","Itabuna","Itacaré","Itagi","Itagibá","Itajuípe","Itamaraju","Itaparica",
                      "Itapé","Itapebi","Itapetinga","Itarantim","Itatim","Itororó","Ituberá","Jaguaquara","Jequié","Juazeiro","Laje","Lajedo do Tabocal","Lauro de Freitas",
                      "Licínio de Almeida","Livramento de Nossa Senhora","Luís Eduardo Magalhães","Maragogipe","Medeiros Neto","Mirante","Morpará","Mucugê",
                      "Nilo Peçanha","Nova Soure","Oliveira dos Brejinhos","Palmeiras","Paramirim","Paulo Afonso","Piripá","Porto Seguro","Pojuca","Prado",
                      "Ribeira do Pombal","Rio do Pires","Rio Real","Salvador","Santa Cruz Cabrália","Santa Luzia","Santa Maria da Vitória",
                      "Santa Teresinha","São Domingos","São Francisco do Conde","São José da Vitória","Sátiro Dias","Serra do Ramalho","Serra Preta",
                      "Serrinha","Simões Filho","Taperoá","Teixeira de Freitas","Ubaitaba","Ubatã","Una","Uruçuca","Utinga","Valença","Valente",
                      "Vera Cruz","Vitória da Conquista","Outros Estados","Local de provável infecção diferente da residência"),
            ATTBA = "Atualizado às 18:30 de 26/04/2020", ATTBR = "Atualizado às 18:30 de 26/04/2020")

{
  ## DIARIO
  
  d = c()
  for(i in 1:nrow(DIARIO)){d[i] = paste0(substr(DIARIO$Data[i], 9, 10), substr(DIARIO$Data[i], 5, 7))}
  data.frame(paste0(substr(DIARIO$Data[1], 9, 10), substr(DIARIO$Data[1], 5, 7)))
  DIARIO = data.frame(DIARIO, datacor = d)
  
  ## MINISTERIO DA SAUDE

  brmap_estado = brmap_estado[c(order(brmap_estado$estado_nome)),]
  
  MSESTADO = left_join(brmap_estado, MSESTADO, by = c("estado_sigla" = "estado"))
  
  d = c()
  for(i in 1:nrow(MSDIARIO)){d[i] = paste0(substr(MSDIARIO$data[i], 9, 10), substr(MSDIARIO$data[i], 5, 7))}
  data.frame(paste0(substr(MSDIARIO$data[1], 9, 10), substr(MSDIARIO$data[1], 5, 7)))
  MSDIARIO = data.frame(MSDIARIO, datacor = d)
  
} # TRATAMENTO

header = dashboardHeader(title = "COVID-19")

sidebar = dashboardSidebar(disable = T)

body = dashboardBody(
  
  fluidPage(
    box(width = 9, title = strong("Mapa do COVID-19 na Bahia"), 
        footer = info$ATTBA, 
        leafletOutput("MapaBA", height = 520,)),
    
    box(width = 3, height = 625, 
        valueBoxOutput(width = 12, outputId = "value_1"),
        valueBoxOutput(width = 12, outputId = "value_2"),
        valueBoxOutput(width = 12, outputId = "value_3"),
        valueBoxOutput(width = 12, outputId = "value_4"),
        valueBoxOutput(width = 12, outputId = "value_5")),
    
    box(width = 12, tags$p(strong("Observações:")), 
        tags$p("A secretária de saúde da Bahia (SESAB) informou que do total de ", 
               sum(DIARIO$Casos), " casos confirmados no estado, ", as.numeric(INFO[6,2]),
               " são de profissionais de saúde."))),
  
  fluidPage(
    box(width = 12, title = strong("Distribuição dos casos e óbitos na Bahia"),
        div(style = 'overflow-x: scroll', DT::dataTableOutput('tbl')))),  
  
  fluidPage(
    box(width = 6, height = 420,
        plotOutput("Linhas1BA", height = 200),
        plotOutput("Linhas2BA", height = 200)),
    
    box(width = 6, height = 420, plotOutput("BarrasBA"))),
  
  fluidPage(
    box(width = 4,
        valueBoxOutput(width = 12, outputId = "value_1BR"),
        valueBoxOutput(width = 12, outputId = "value_2BR"),
        valueBoxOutput(width = 12, outputId = "value_3BR"),
        valueBoxOutput(width = 12, outputId = "value_4BR"),
        valueBoxOutput(width = 12, outputId = "value_5BR")),
    
    box(width = 8,
        plotOutput("MAPABR", height = 450), 
        radioButtons(inputId = "BRMAP", label = NULL, selected = "Casos",
                     choices = c("Casos","Incidência","Óbitos")),
        title = strong("Mapa do COVID-19 no Brasil"), 
        footer = info$ATTBR)),
  
  fluidPage(
    box(width = 12, title = strong("Distribuição de casos e óbitos no Brasil"),
        div(style = 'overflow-x: scroll', DT::dataTableOutput('tbl2')))),
  
  fluidPage(
    box(width = 6, plotOutput("LinhasBR1", height = 300)),
    box(width = 6, plotOutput("LinhasBR2", height = 300))),
  
  fluidPage(
    box(width = 12, title = strong("Fontes"), 
        tags$p("Secretaria da Saúde da Bahia:", tags$a("http://www.saude.ba.gov.br")), 
        tags$p("Ministério da Saúde:", tags$a("https://saude.gov.br/"))),
    
    box(width = 12, tags$p("Desenvolvido por Jonatha Pimentel.", strong("LinkedIn: "), 
                           tags$a("https://linkedin/in/jspimentel"), 
                           ", Universidade Federal da Bahia - UFBA, 
                           Laboratório de Estatística e Data Science - LED.")))
)

server = function(input, output){
  
  output$value_1 = renderValueBox({
    valueBox(value = scales::number(sum(DIARIO$Casos)), 
             subtitle = "Casos Confirmados", color = "yellow")
  })
  output$value_2 = renderValueBox({
    valueBox(value = scales::number(sum(BAHIA$Obitos)), 
             subtitle = "Óbitos Confirmados", color = "yellow")
  })
  output$value_3 = renderValueBox({
    valueBox(value = scales::number(as.numeric(INFO[1,2])), 
             subtitle = "Pessoas Recuperadas", color = "yellow")
  })
  output$value_4 = renderValueBox({
    valueBox(value = paste0(scales::number(as.numeric(INFO[2,2])), " (", as.numeric(INFO[3,2]), ")"), 
             subtitle = "Pessoas Internadas (UTI)", color = "yellow")
  })
  output$value_5 = renderValueBox({
    valueBox(value = scales::number(as.numeric(INFO[5,2])), 
             subtitle = "Casos Descartados", color = "yellow")
  })
  
  output$MapaBA = renderLeaflet({
    leaflet() %>% addTiles() %>% 
      addCircleMarkers(as.numeric(BAHIA$Longitude), as.numeric(BAHIA$Latitude), color = "red",
                       radius = BAHIA$Casos*0.05) %>% 
      addCircleMarkers(as.numeric(BAHIA[BAHIA$Obitos>0,]$Longitude), as.numeric(BAHIA[BAHIA$Obitos>0,]$Latitude),
                       color = "black", radius = BAHIA$Obitos*0.1) %>% 
      addLegend(position = "topright", colors = c('black', 'red'),
                labels = c('Óbitos','Casos'), opacity = 1)
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
                  colnames = c('Cidades', 'Casos', 'Óbitos', 'População',
                               'Coeficiente de incidência de casos por 1 milhão de habitantes'))
  })
  output$BarrasBA = renderPlot({

    GRAFBAR = data.frame(Cidades = c(rep(c(BAHIA[BAHIA$Casos > 25,]$Cidade),
                                         c(BAHIA[BAHIA$Casos > 25,]$Casos)),
                                     rep("Outras\nCidades", 
                                         sum(BAHIA[BAHIA$Casos <= 25,2]))))
    
    A = data.frame(table(GRAFBAR))
    
    ggplot(GRAFBAR) + aes(x = Cidades) + geom_bar(fill = "orange") +
      ggtitle("Distribuição de Casos na Bahia - Cidades com mais de 25 casos") +
      scale_y_continuous(name = "Casos", limits = c(0, max(A[,2]) + max(A[,2])/10)) +
      scale_x_discrete(name = "Cidades", labels = c("Camaçari","Feira de\nSantana","Ilhéus","Itabuna","Jequié","Lauro de\nFreitas","Outras\nCidades","Salvador")) +
      theme_classic() + geom_text(label = A[,2], stat = "count", vjust = -.5) 
      
  })
  output$Linhas1BA = renderPlot({
    
    ggplot(DIARIO) + geom_line(aes(x = 1:nrow(DIARIO), y = CasosAcumulados), size = 1.25) +
      geom_point(aes(x = 1:nrow(DIARIO), y = CasosAcumulados),
                                size = 3, color = "orange") +
      annotate("text", x = round(seq(1, nrow(DIARIO), length.out = 10)),
               y = DIARIO$CasosAcumulados[round(seq(1, nrow(DIARIO), 
                                                    length.out = 10))] + 
                 max(DIARIO$CasosAcumulados)/12, 
               label = DIARIO$CasosAcumulados[round(seq(1, nrow(DIARIO), length.out = 10))]) +
      
      scale_x_continuous(name = "", breaks = round(seq(1, nrow(DIARIO), length.out = 10)),
                         labels = DIARIO$datacor[round(seq(1, nrow(DIARIO), length.out = 10))]) +
      scale_y_continuous(name = "Casos", limits = c(0, max(DIARIO$CasosAcumulados) + 
                                                      max(DIARIO$CasosAcumulados)/8)) +
      ggtitle("Evolução dos Casos na Bahia") + theme_classic()
  })
  output$Linhas2BA = renderPlot({
    
    DIARIO2 = DIARIO[c(24:nrow(DIARIO)),]
    
    ggplot(DIARIO2) + geom_line(aes(x = 1:nrow(DIARIO2), y = ObitosAcumulados), 
                                size = 1.25) +
      geom_point(aes(x = 1:nrow(DIARIO2), y = ObitosAcumulados), 
                                size = 3, color = "orange") +

      annotate("text", x = round(seq(1, nrow(DIARIO2), length.out = 10)), 
               y = DIARIO2$ObitosAcumulados[round(seq(1, nrow(DIARIO2), length.out = 10))] + 
                 max(DIARIO2$ObitosAcumulados)/10, 
               label = DIARIO2$ObitosAcumulados[round(seq(1, nrow(DIARIO2), length.out = 10))]) +
      scale_x_continuous(name = "", breaks = round(seq(1, nrow(DIARIO2), length.out = 10)), 
                         labels = DIARIO2$datacor[round(seq(1, nrow(DIARIO2), length.out = 10))]) +
      scale_y_continuous(name = "Óbitos", limits = c(0, max(DIARIO2$ObitosAcumulados) + 
                                                       max(DIARIO2$ObitosAcumulados)/4)) +
      ggtitle("Evolução no número de óbitos na Bahia") + theme_classic()
  })
  
  output$value_1BR = renderValueBox({
    valueBox(value = scales::number(sum(MSESTADO$casos)), 
             subtitle = "Casos Confirmados", color = "yellow")
  })
  output$value_2BR = renderValueBox({
    valueBox(value = scales::number(sum(MSESTADO$obitos)), 
             subtitle = "Óbitos Confirmados", color = "yellow")
  })
  output$value_3BR = renderValueBox({
    valueBox(value = paste0(round((sum(MSESTADO$obitos)/sum(MSESTADO$casos))*100,2),"%"), 
             subtitle = "Taxa de Letalidade", color = "yellow")
  })
  output$value_4BR = renderValueBox({
    valueBox(value = scales::number(MSDIARIO$obitos[nrow(MSDIARIO)]), 
             subtitle = paste0("Óbitos confirmados hoje ", MSDIARIO$datacor[nrow(MSDIARIO)]), 
             color = "yellow")
  })
  output$value_5BR = renderValueBox({
    valueBox(value = scales::number(MSDIARIO$casos[nrow(MSDIARIO)]), 
             subtitle = paste0("Casos confirmados hoje ", MSDIARIO$datacor[nrow(MSDIARIO)]), color = "yellow")
  })
  
  output$MAPABR = renderPlot({
    if(input$BRMAP == "Óbitos"){
      ggplot(MSESTADO) + geom_sf(aes(fill = obitos)) + 
        scale_fill_distiller(type = "seq", palette = "Greys", direction = 1) +
        labs(title = NULL, fill = "Óbitos") + 
        theme(panel.background = element_blank(),
              panel.grid.major = element_line(color = "transparent"),
              axis.text = element_blank(), axis.ticks = element_blank())
    }
    else{
      if(input$BRMAP == "Casos"){
        ggplot(MSESTADO) + geom_sf(aes(fill = casos)) + 
          scale_fill_distiller(type = "seq", palette = "Reds", direction = 1) +
          labs(title = NULL, fill = "Casos") + 
          theme(panel.background = element_blank(),
                panel.grid.major = element_line(color = "transparent"),
                axis.text = element_blank(), axis.ticks = element_blank())
      }
      else{
        ggplot(MSESTADO) + geom_sf(aes(fill = as.numeric(IncidenciaCasos))) + 
          scale_fill_distiller(type = "seq", palette = "Oranges", direction = 1) +
          labs(title = NULL, fill = "Incidência de casos\npor 1 milhão de habitantes") + 
          theme(panel.background = element_blank(),
                panel.grid.major = element_line(color = "transparent"),
                axis.text = element_blank(), axis.ticks = element_blank())
      }
    }
  })
  output$tbl2 = DT::renderDataTable({
    coltopo = JS("function(settings, json) {","$(this.api().table().header()).css(
          {'background-color': 'orange', 'color': 'white'});", "}")
    
    DT::datatable(data.frame(MSESTADO$estado_nome, MSESTADO$casos, MSESTADO$obitos,
                             number(MSESTADO$Populacao), 
                             round(as.numeric(MSESTADO$IncidenciaCasos),2)), 
                  filter = 'none', options = list(autoWidth = T, initComplete = coltopo, 
                  pageLength = 9, lengthMenu = c(3, 9, 27),
                  columnDefs = list(list(className = 'dt-left', targets = 1),
                                    list(className = "dt-right", targets = 2:5))), 
                  colnames = c('Estados', 'Casos', 'Óbitos', 'População',
                               'Coeficiente de incidência de casos por 1 milhão de habitantes'))
  })
  output$LinhasBR1 = renderPlot({
    MSDIARIO2 = MSDIARIO[MSDIARIO$casosA > 0,]
    
    ggplot(MSDIARIO2) + geom_line(aes(x = 1:nrow(MSDIARIO2), y = casosA), size = 1.25) +
      geom_point(aes(x = 1:nrow(MSDIARIO2), y = casosA),
                                size = 3, color = "orange") +
      annotate("text", x = round(seq(1, nrow(MSDIARIO2), length.out = 10)),
               y = MSDIARIO2$casosA[round(seq(1, nrow(MSDIARIO2), 
                                                    length.out = 10))] + 
                 max(MSDIARIO2$casosA)/12, 
               label = MSDIARIO2$casosA[round(seq(1, nrow(MSDIARIO2), length.out = 10))]) +
      
      scale_x_continuous(name = "", breaks = round(seq(1, nrow(MSDIARIO2), length.out = 10)),
                         labels = MSDIARIO2$datacor[round(seq(1, nrow(MSDIARIO2), length.out = 10))]) +
      scale_y_continuous(name = "Casos", limits = c(0, max(MSDIARIO2$casosA) + 
                                                      max(MSDIARIO2$casosA)/8)) +
      ggtitle("Evolução dos Casos no Brasil") + theme_classic()
  })
  output$LinhasBR2 = renderPlot({
    MSDIARIO2 = MSDIARIO[MSDIARIO$obitosA > 0,]
    
    ggplot(MSDIARIO2) + geom_line(aes(x = 1:nrow(MSDIARIO2), y = obitosA), size = 1.25) +
      geom_point(aes(x = 1:nrow(MSDIARIO2), y = obitosA),
                                   size = 3, color = "orange") +
      annotate("text", x = round(seq(1, nrow(MSDIARIO2), length.out = 10)),
               y = MSDIARIO2$obitosA[round(seq(1, nrow(MSDIARIO2), 
                                              length.out = 10))] + 
                 max(MSDIARIO2$obitosA)/20, 
               label = MSDIARIO2$obitosA[round(seq(1, nrow(MSDIARIO2), length.out = 10))]) +
      
      scale_x_continuous(name = "", breaks = round(seq(1, nrow(MSDIARIO2), length.out = 10)),
                         labels = MSDIARIO2$datacor[round(seq(1, nrow(MSDIARIO2), length.out = 10))]) +
      scale_y_continuous(name = "Óbitos", limits = c(0, max(MSDIARIO2$obitosA) + 
                                                      max(MSDIARIO2$obitosA)/8)) +
      ggtitle("Evolução no número de óbitos no Brasil") + theme_classic()
  })
}

ui = dashboardPage(skin = "yellow", header, sidebar, body)

shinyApp(ui, server) # APP SHINY
