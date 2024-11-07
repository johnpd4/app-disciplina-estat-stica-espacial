library(pacman)
pacman::p_load(sf, shiny, bslib, leaflet, dplyr, purrr, knitr, htmltools, fontawesome, shinythemes, bsicons)

caminho = file.path(getwd(), "dados")

arquivos <- list.files(pattern = "\\.csv$", full.names = TRUE)
estados <- gsub("\\.csv$", "", basename(arquivos))

estados <- c(
  "Acre" = "Acre",
  "Alagoas" = "Alagoas",
  "Amapá" = "Amapa",
  "Amazonas" = "Amazonas",
  "Bahia" = "Bahia",
  "Ceará" = "Ceara",
  "Distrito Federal" = "DF",
  "Espírito Santo" = "Espirito",
  "Goiás" = "Goias",
  "Maranhão" = "Maranhao",
  "Mato Grosso" = "Mato",
  "Mato Grosso do Sul" = "Mato_Sul",
  "Minas Gerais" = "Minas",
  "Pará" = "Para",
  "Paraíba" = "Paraiba",
  "Paraná" = "Parana",
  "Pernambuco" = "Pernambuco",
  "Piauí" = "Piaui",
  "Rio de Janeiro" = "RJ",
  "Rio Grande do Norte" = "Rio_norte",
  "Rio Grande do Sul" = "Rio_Grande_do_Sul",
  "Rondônia" = "Rondonia",
  "Roraima" = "Roraima",
  "Santa Catarina" = "Catarina",
  "São Paulo" = "SP",
  "Sergipe" = "Sergipe",
  "Tocantins" = "Tocantins"
)

#superhero
#cyborg
#minty
#solar 
#sketchy
#morph

ui <- 
  page_sidebar(
    
    theme = bs_theme(version = 5, bootswatch = "lux"),
    title = "Seminário 1",
    
    sidebar = sidebar(
      
      width = "20%",
      open = "always",
      
      h1("Variáveis brutas:", align = "center", style = "text-transform: capitalize"),
      
      h2(textOutput("nome_estado"), align = "center", style = "text-transform: capitalize"),
      
      value_box(
        title = textOutput("nome_var"),
        value = textOutput("valor_var"),
        theme = "#666",
        showcase = bs_icon("triangle-fill")
      ),
      
      value_box(
        title = "Área (km^2)",
        value = textOutput("area_estado"),
        theme = "#666",
        showcase = bs_icon("globe2")
      ),
      
      value_box(
        title = "População",
        value = textOutput("pop_residente"),
        theme = "#666",
        showcase = bs_icon("people-fill")
      ),
      
      value_box(
        title = "PIB (R$)",
        value = textOutput("pib_estado"),
        theme = "#666",
        showcase = bs_icon("graph-up-arrow")
      ),
      
      selectInput(
        inputId = "estadoSelecionado",
        label = "Escolha o Estado",
        choices = estados
      ),
      
      selectInput(
        inputId = "varSelecionada",
        label = "Escolha a Variável",
        choices = c("Bovino", "Equino", "Suinos", "Galinaceos")
      ),
      
      p("Grupo: Gabriel Bertolini, Gabriel Nunes, Pedro John, Rodrigo Kato e Yuri Dessimon")
      
    ), # sidebar

    leafletOutput("mapa_woow")
)

server <- function(input, output, session) {
  
  carregar_estado <- function(estado) {
    # Para testes
    #estado = "Acre"
    
    estado_csv <- file.path(caminho, paste0(estado, ".csv"))
    df <- read.csv(estado_csv)
    return(df)
  }
  
  carregar_shapefile <- function(estado) {
    # Para testes
    #estado = "Acre"
    
    shapefile <- switch(estado,
                        "Acre" = "AC_Municipios_2022.shp",
                        "Alagoas" = "AL_Municipios_2022.shp",
                        "Amapa" = "AP_Municipios_2022.shp",
                        "Amazonas" = "AM_Municipios_2022.shp",
                        "Bahia" = "BA_Municipios_2022.shp",
                        "Ceara" = "CE_Municipios_2022.shp",
                        "DF" = "DF_Municipios_2022.shp",
                        "Espirito" = "ES_Municipios_2022.shp",
                        "Goias" = "GO_Municipios_2022.shp",
                        "Maranhao" = "MA_Municipios_2022.shp",
                        "Mato" = "MT_Municipios_2022.shp",
                        "Mato_Sul" = "MS_Municipios_2022.shp",
                        "Minas" = "MG_Municipios_2022.shp",
                        "Para" = "PA_Municipios_2022.shp",
                        "Paraiba" = "PB_Municipios_2022.shp",
                        "Parana" = "PR_Municipios_2022.shp",
                        "Pernambuco" = "PE_Municipios_2022.shp",
                        "Piaui" = "PI_Municipios_2022.shp",
                        "RJ" = "RJ_Municipios_2022.shp",
                        "Rio_norte" = "RN_Municipios_2022.shp",
                        "Rio_Grande_do_Sul" = "RS_Municipios_2022.shp",
                        "Rondonia" = "RO_Municipios_2022.shp",
                        "Roraima" = "RR_Municipios_2022.shp",
                        "Catarina" = "SC_Municipios_2022.shp",
                        "SP" = "SP_Municipios_2022.shp",
                        "Sergipe" = "SE_Municipios_2022.shp",
                        "Tocantins" = "TO_Municipios_2022.shp")
    
    shapefile_path <- file.path(caminho, shapefile)
    
    st_read(shapefile_path) %>%
      st_transform(crs = 4326)
  }
  
  observeEvent(input$estadoSelecionado, {
    req(input$estadoSelecionado)
    
    # Para testes
    #input = data.frame(estadoSelecionado = "Acre", varSelecionada = "Suinos")
    
    mun <- carregar_estado(input$estadoSelecionado)
    dados_shp <- carregar_shapefile(input$estadoSelecionado)
    
    dados_merged <- left_join(dados_shp, mun, by = c("NM_MUN"))
    
    dados_merged$PIB_cap <- as.numeric(dados_merged$PIB_cap)  
    dados_merged$Densi_demo <- as.numeric(dados_merged$Densi_demo)
    
    output$nome_estado <- renderText({
      
      nome_estado = input$estadoSelecionado
      
      nome_estado = switch(nome_estado,
                           "Acre" = "Acre",
                           "Alagoas" = "Alagoas",
                           "Amapa" = "Amapá",
                           "Amazonas" = "Amazonas",
                           "Bahia" = "Bahia",
                           "Ceara" = "Ceará",
                           "DF" = "Distrito Federal",
                           "Espirito" = "Espírito Santo",
                           "Goias" = "Goiás",
                           "Maranhao" = "Maranhão",
                           "Mato" = "Mato Grosso",
                           "Mato_Sul" = "Mato Grosso do Sul",
                           "Minas" = "Minas Gerais",
                           "Para" = "Pará",
                           "Paraiba" = "Paraíba",
                           "Parana" = "Paraná",
                           "Pernambuco" = "Pernambuco",
                           "Piaui" = "Piauí",
                           "RJ" = "Rio de Janeiro",
                           "Rio_norte" = "Rio Grande do Norte",
                           "Rio_Grande_do_Sul" = "Rio Grande do Sul",
                           "Rondonia" = "Rondônia",
                           "Roraima" = "Roraima",
                           "Catarina" = "Santa Catarina",
                           "SP" = "São Paulo",
                           "Sergipe" = "Sergipe",
                           "Tocantins" = "Tocantins")
      
      HTML(paste0(nome_estado))  
    })
    
    output$pib_estado <- renderText({
      pib <- sum(dados_merged$PIB_cap, na.rm = TRUE)  
      HTML(paste0(format(round(pib, 0), nsmall = 0)))  
    })
    
    output$pop_residente <- renderText({
      densi_demo <- sum(dados_merged$Pop_residente, na.rm = TRUE) 
      HTML(paste0(format(round(densi_demo, 0), nsmall = 0)))  
    })
    
    output$area_estado <- renderText({
      densi_demo <- sum(dados_merged$AREA_KM2, na.rm = TRUE) 
      HTML(paste0(format(round(densi_demo, 0), nsmall = 0)))  
    })
    
    output$mapa_woow <- renderLeaflet({
      req(input$varSelecionada)  
      
      var_selecionada <- input$varSelecionada

      output$nome_var <- renderText({
        var_nome = switch(var_selecionada,
                          "Bovino" = "Bovinos Totais",
                          "Equino" = "Equinos Totais",
                          "Suinos" = "Suínos Totais",
                          "Galinaceos" = "Galináceos Totais")
        
        HTML(paste0(var_nome))  
      })
      
      output$valor_var <- renderText({
        valor <- sum(dados_merged[[var_selecionada]], na.rm = TRUE)  
        HTML(paste0(format(round(valor, 0), nsmall = 0)))  
      })
      
      pal <- colorNumeric(
        palette = switch(var_selecionada,
                         "Bovino" = "YlOrRd",
                         "Equino" = "BuPu",
                         "Suinos" = "OrRd",
                         "Galinaceos" = "Greens"),
        domain = as.numeric(dados_merged[[var_selecionada]])
      )
      
      faz_tabela = function(municipio){
        
        # Para testes
        #municipio = "Acrelândia"
        
        dados_tabela = dados_merged |> subset(NM_MUN == municipio) |>
          select(Bovino, Equino, Suinos, Galinaceos, AREA_KM2, Pop_residente) |> st_drop_geometry()
        
        area = dados_tabela$AREA_KM2
        pop = dados_tabela$Pop_residente
        
        dados_tabela = dados_tabela |> select(-AREA_KM2, -Pop_residente)
        
        dados_tabela[2, ] = dados_tabela[1, ] / area
        dados_tabela[3, ] = dados_tabela[1, ] / pop
        
        dados_tabela$Vars = c("Var bruta", "Var / km2", "Var / pop")
        
        dados_tabela = dados_tabela[,c(5, 1, 2, 3, 4)]
        
        rownames(dados_tabela) = c(NULL, NULL, NULL)
        
        tabela = dados_tabela |> knitr::kable(format = "html", rownames = FALSE, digits = 2)
        
        tabela = paste0("<b>", municipio, "</b><br>", tabela)
        
        return(tabela)
        
      }
      
      dados_merged = dados_merged |> mutate(tabela = map_chr(NM_MUN, faz_tabela))
      
      dados_merged = st_as_sf(dados_merged)
      
      leaflet(dados_merged) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        addPolygons(
          fillColor = ~pal(get(var_selecionada)), 
          weight = 1.5,
          opacity = 1,
          fillOpacity = 0.7,
          color = "gray",
          
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =~ lapply(tabela, htmltools::HTML)) |>
        addLegend("bottomright", pal = pal, values = ~get(var_selecionada), title = var_selecionada, opacity = 1) |>
        setMaxBounds(-27.59, -35.93, -78.97, 8.29)
    })
  })
}

shinyApp(ui = ui, server = server)