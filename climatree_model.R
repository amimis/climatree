# Climatree model
# C3
# Code by Angelos Mimis
# 28.06.2018

library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(DT)
library(plyr)

load(url("https://www.dropbox.com/s/3ta77awuvm05fnq/shp_nuts_v4.rdata?dl=1"))

# all_cases <-data.frame(nut=character(), code=character(),name=character(),
#                        area=numeric(), tree=character(),debris=logical(), 
#                        age=numeric(), density=numeric() ) 

source('model_code.R', local = TRUE)

total_model<-function(df_selected){
  
  df.pools__<-data.frame(name=character(), code=character(),tree=character(),year=integer(), biomass_trunk=numeric(), biomass_roots=numeric(), debris=numeric(), soil=numeric())
  
  number_of_regions <- nrow( df_selected )
  for( i in 1:number_of_regions){
    regions_ <- df_selected$code[i]
    surface  <- df_selected$area[i]
    
    tree_    <- tree_name_to_number(df_selected$tree[i])
    
    yield_ <-  df_selected$yield[i]
    pdensity_ <-df_selected$density[i]
    keep_res <- df_selected$debris[i]
    
    #loop for all regions!
    #mydata[mydata$NUTS_ID=="EL301",]
    soil_veg<- df_selected$soil_veg[i]
    soil_vegetated <- rep(soil_veg,12)
    farmyard_manure <- rep(0,12)
    
    #reg_clim <- mydata[mydata$NUTS_ID==regions_,]
    reg_clim<-data.frame()
    reg_clim_f<-data.frame()
    soil_data<-data.frame()
    if(df_selected$nut[i]=="Nuts 1"){
      reg_clim <- nuts1_climatic[nuts1_climatic$NUTS_ID==regions_,]
      reg_clim_f <- nuts1_climatic_f[nuts1_climatic_f$NUTS_ID==regions_,]
      soil_data <- nuts1_soil[nuts1_soil$NUTS_ID==regions_,]
    }
    if(df_selected$nut[i]=="Nuts 2"){
      reg_clim <- nuts2_climatic[nuts2_climatic$NUTS_ID==regions_,]
      reg_clim_f <- nuts2_climatic_f[nuts2_climatic_f$NUTS_ID==regions_,]
      soil_data <- nuts2_soil[nuts2_soil$NUTS_ID==regions_,]
    }
    if(df_selected$nut[i]=="Nuts 3"){
      reg_clim <- nuts3_climatic[nuts3_climatic$NUTS_ID==regions_,]
      reg_clim_f <- nuts3_climatic_f[nuts3_climatic_f$NUTS_ID==regions_,]
      soil_data <- nuts3_soil[nuts3_soil$NUTS_ID==regions_,]
    }
    
    temp <- c(reg_clim$Avg_TE1-273.15, reg_clim$Avg_TE2-273.15,
            reg_clim$Avg_TE3-273.15, reg_clim$Avg_TE4-273.15,
            reg_clim$Avg_TE5-273.15, reg_clim$Avg_TE6-273.15,
            reg_clim$Avg_TE7-273.15, reg_clim$Avg_TE8-273.15,
            reg_clim$Avg_TE9-273.15, reg_clim$Avg_TE10-273.15, 
            reg_clim$Avg_TE11-273.15, reg_clim$Avg_TE12-273.15)
    
    temp_f <- c(reg_clim_f$Avg_FTE1-273.15, reg_clim_f$Avg_FTE2-273.15,
                reg_clim_f$Avg_FTE3-273.15, reg_clim_f$Avg_FTE4-273.15,
              reg_clim_f$Avg_FTE5-273.15, reg_clim_f$Avg_FTE6-273.15,
              reg_clim_f$Avg_FTE7-273.15, reg_clim_f$Avg_FTE8-273.15,
              reg_clim_f$Avg_FTE9-273.15, reg_clim_f$Avg_FTE10-273.15, 
              reg_clim_f$Avg_FTE11-273.15, reg_clim_f$Avg_FTE12-273.15)
     
    rainfall <- c(reg_clim$Avg_PR1,reg_clim$Avg_PR2,
              reg_clim$Avg_PR3,reg_clim$Avg_PR4,
              reg_clim$Avg_PR5,reg_clim$Avg_PR6,
              reg_clim$Avg_PR7,reg_clim$Avg_PR8,
              reg_clim$Avg_PR9,reg_clim$Avg_PR10,
              reg_clim$Avg_PR11,reg_clim$Avg_PR12 )
    
    rainfall_f <- c(reg_clim_f$Avg_FPR1,reg_clim_f$Avg_FPR2,
                    reg_clim_f$Avg_FPR3,reg_clim_f$Avg_FPR4,
                    reg_clim_f$Avg_FPR5,reg_clim_f$Avg_FPR6,
                    reg_clim_f$Avg_FPR7,reg_clim_f$Avg_FPR8,
                    reg_clim_f$Avg_FPR9,reg_clim_f$Avg_FPR10,
                    reg_clim_f$Avg_FPR11,reg_clim_f$Avg_FPR12 )
  
    pan_evap <- c(reg_clim$Avg_PE1,reg_clim$Avg_PE2,
              reg_clim$Avg_PE3,reg_clim$Avg_PE4,
              reg_clim$Avg_PE5,reg_clim$Avg_PE6,
              reg_clim$Avg_PE7,reg_clim$Avg_PE8,
              reg_clim$Avg_PE9,reg_clim$Avg_PE10,
              reg_clim$Avg_PE11,reg_clim$Avg_PE12 )
    
    pan_evap_f <- c(reg_clim_f$Avg_FPE1,reg_clim_f$Avg_FPE2,
                    reg_clim_f$Avg_FPE3,reg_clim_f$Avg_FPE4,
                    reg_clim_f$Avg_FPE5,reg_clim_f$Avg_FPE6,
                    reg_clim_f$Avg_FPE7,reg_clim_f$Avg_FPE8,
                    reg_clim_f$Avg_FPE9,reg_clim_f$Avg_FPE10,
                    reg_clim_f$Avg_FPE11,reg_clim_f$Avg_FPE12 )
    
    clay <- soil_data$clay     #Percent clay
    
    rothc_initial_pools <- c(0.0, soil_data$RPM, soil_data$BIO, soil_data$HUM, 0.0 )
    
    tree_name<- tree_species[tree_]
    #df.pools_<-climatree_model( 50, surface, tree_, temperature,  rainfall, evaporation, clay, soil_vegetated, farmyard_manure)
    df.pools_<-climatree_model( 50, surface, tree_, temp,  temp_f, temp_future_change=0.0, rainfall, rainfall_f, pan_evap, pan_evap_f, 
                                clay, rothc_initial_pools, soil_vegetated, yield_, pdensity_, keep_res, farmyard_manure)
    
    df.tmp<-data.frame( name=rep(df_selected$name[i],50), code=rep(df_selected$code[i],50),
                        tree=rep(tree_name,50), area=rep(df_selected$area[i],50), yield=rep(df_selected$yield[i],50), 
                        density=rep(df_selected$density[i],50), debris_left = rep(df_selected$debris[i],50), 
                        soil_veg=rep(df_selected$soil_veg[i],50) )
    df.pools_<-cbind(df.tmp, df.pools_)
    
    df.pools__<-rbind(df.pools__, df.pools_)
  }
  df.pools__
}

get_region_name <- function(nuts_, id_){
  
  text_ <-""  
  if (nuts_=="nuts1")
    text_ = nuts1_shp[nuts1_shp$NUTS_ID==id_,]$NAME_LATN
  if (nuts_=="nuts2")
    text_ = nuts2_shp[nuts2_shp$NUTS_ID==id_,]$NAME_LATN
  if (nuts_=="nuts3")
    text_ = nuts3_shp[nuts3_shp$NUTS_ID==id_,]$NAME_LATN

  text_
}



header <- dashboardHeader(title = "Climatree CO2 model")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Model description",tabName = "description"),
    menuItem("Tree crops characteristics",tabName = "input"),
    menuItem("Analysis",tabName = "results"),
    menuItem("Row data",tabName = "row_data")
  )
  
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "description",
            h2("CO2 sequestration in cropland"),
            tags$div(
              HTML("<strong>The model in this study, will consist by three pools. 
                   The first one will include biomass (BM), the second one, debris pool (DP) 
                   and the third one the soil organic matter (SOM).</strong>"),
              tags$br(),
              HTML("<strong>The 1st pool is connected to 2nd pool though pruning, 
                   and crops left to ground. It is also connected through the roots 
                   to the soil directly. The material in DP decomposes and feeds the Soil. 
                   The conceptual description of the model can be seen in the following 
                   figure.</strong>"),
              tags$br(),
              #tags$img(src="http://www.uehr.gr/images/climatree/co2_model.jpg", alt="co2 model"),
              tags$img(src="https://www.dropbox.com/s/hy564bs9sukmnrr/co2_model.jpg?dl=1", alt="co2 model"),
              tags$br(),
              HTML("<strong>The model is spatial, and can be run in NUTS 1, 2 or 3 level of detail. 
                   It is customized for the seven species of perennial trees most common 
                   in Mediterranean area and are the olive, orange, apple, almond, peach 
                   trees.</strong>")
            )
            
    ),
    tabItem(tabName="input",
            fluidRow(
              box(title = "Level of detail",width = 2,solidHeader = TRUE,  
                  selectInput("nuts_input", "NUTS",
                              choices = c("NUTS 1" = "nuts1",
                                          "NUTS 2" = "nuts2",
                                          "NUTS 3"="nuts3"),
                              selected = "nuts1"
                              )),
              box(title = "Location",width=10,"pick your region",
                  leafletOutput("myMap"))
                  #leaflet()%>% addTiles() %>% addPolygons(data=my_data(),weight=3,col = 'red'))
              ),
            
            fluidRow(
              box(title="Land characteristics",width=12,
                  column(4,
                      tags$p("Region"),
                      verbatimTextOutput("region_selected"),
                      
                      checkboxInput("litter", "Litter on the field",
                                    value = TRUE),
                      
                      checkboxInput("soil_veg", "Soil vegetated ",
                                    value = TRUE)
                  ),
                  column(4,
                      numericInput("area", "Area(ha)", value = 10),
                      numericInput("density", "Density(trees/ha)", value = 300)
                  ),
                  column(4,
                      selectInput("tree", "Tree crops",
                                  choices = c("Olive" = "option1",
                                              "Orange" = "option2",
                                              "Apple" = "option3",
                                              "Almond" = "option4",
                                              "Peach" = "option5"),
                                  selected = "option3"),
                      numericInput("yield", "Yield (tn/ha)", value = 8.0),
                      
                      actionButton("addButton", "Add")
                      )
                 )
              ),
            fluidRow(
              box(title="List of land registered for the estimation",width=12,
              
              tableOutput('my_table'),
              
              actionButton("delButton", "Delete last row")
              )
            )
    ),
    tabItem(tabName = "results",
            fluidRow(
              box(title = "Selected data", width = 6,
                verbatimTextOutput("nuts_text"),
                #verbatimTextOutput("total_results"),
                #verbatimTextOutput("region_selected"),
                tableOutput('selected_data_table'),
                actionButton("evalButton", "Run the model")
              )
            ),
          fluidRow(
            box(title="Results", graph = 6,
                verbatimTextOutput("total_results"),
                #plot if there is smt to plot
                plotOutput("soil_plot"),
                plotOutput("carbon_changes")
            )   
          )
    ),
    tabItem(tabName = "row_data",
            h2("Row data"),
            fluidRow(
              column(4,
                     selectInput("time",
                                 "Time:",
                                 c("All",
                                   unique( c("every 10 years","last year") )))
              ),
              column(4,
                     selectInput("tree_type",
                                 "Tree:",
                                 c("All",
                                   unique(as.character(tree_species))))
              )
              
            ),
            DT::dataTableOutput("table_row_results"),
            downloadButton('downloadresults', 'Download')
    )
    
  )
  
)

ui <- dashboardPage( header, sidebar, body )

server <- function(input, output) { 
    
    data_of_clicks <- reactiveValues(ids="")
    df <- reactiveValues(all_cases =data.frame(nut=character(), code=character(),name=character(),
                                                area=numeric(), tree=character(),debris=logical(), 
                                                yield=numeric(), density=numeric(), soil_veg=logical() ) )
    soil_results <- reactiveValues(df.pools=data.frame(
      name=character(), code=character(),tree=character(),
      area=numeric(), yield=numeric(), density=numeric(),
      debris_left = logical(), soil_veg=logical(), year=integer(),
      biomass_trunk=numeric(), biomass_roots=numeric(), debris=numeric(), soil=numeric()
    )) #numeric())
      
    observeEvent(input$evalButton, {
      soil_results$df.pools <- total_model( df$all_cases )
    })
    
    output$soil_plot <-renderPlot({
      
      data <- soil_results$df.pools
      data <- data.frame(year=data$year,trunk=data$biomass_trunk,roots=data$biomass_roots,
        debris=data$debris,soil=data$soil)
      
      agg_data <- ddply(data,.(year),numcolwise(sum))
      
      years=1:50
      if(nrow(soil_results$df.pools)>10){
        matplot(years, agg_data[1:50,2:5], type="l", lty=1, col=1:4,
                xlab="Time (years)", ylab="C stocks (T /ha)")
        legend("topleft", c("Trunk", "Roots", "Debris", "Soil"),
               lty=1, col=1:4, bty="n")
      }

    })
    
    output$carbon_changes <-renderPlot({
      
      data <- soil_results$df.pools
      
      t1_bt <- data$biomass_trunk
      t2_bt <- t1_bt[-1]
      t1_bt <- head(t1_bt,-1)
      t_bt <- t2_bt-t1_bt
      
      t1_s <- data$soil
      t2_s <-t1_s[-1]
      t1_s <- head(t1_s,-1)
      t_s <- t2_s-t1_s
      
      data <- data.frame(year=head(data$year,-1),trunk=t_bt, soil=t_s)
      
      agg_data <- ddply(data,.(year),numcolwise(sum))
      
      years=1:49
      if(nrow(soil_results$df.pools)>10){
        matplot(years, agg_data[1:49,2:3], type="l", lty=1, col=c(1,4),
                xlab="Time (years)", ylab="C stocks (T /ha)")
        legend("topleft", c("Trunk", "Soil"),
               lty=1, col=c(1,4), bty="n")
      }
      
    })
    
    #df_removed <- eventReactive(input$delButton, {
    observeEvent(input$delButton, {
      
      if (nrow(df$all_cases)>0){
        df$all_cases <- df$all_cases[-nrow(df$all_cases),]
      }
      df$all_cases
    })
    
    #df_captured <- eventReactive(input$addButton, {
    observeEvent(input$addButton, {
    
      if( nchar(data_of_clicks$ids)>0){
        text_ <- switch(input$nuts_input,"nuts1"= "Nuts 1", "nuts2"= "Nuts 2","nuts3"= "Nuts 3")
        
        tree_ <- switch( input$tree,"option1" = "Olive","option2"="Orange" ,"option3"="Apple","option4"="Almond",
                "option5" = "Peach" )
        
        current_case <-data.frame(nut=text_, code=data_of_clicks$ids,
                              name= get_region_name(input$nuts_input, data_of_clicks$ids),
                               area=input$area, tree= tree_,debris=input$litter, 
                               yield=input$yield, density=input$density, soil_veg=input$soil_veg )
        
        df$all_cases <- rbind(df$all_cases, current_case)
      }
      df$all_cases
    })
    
    output$my_table <- renderTable({
      #df_captured()
      df$all_cases
    })
    
    output$selected_data_table <- renderTable({
      
      sdt <- data.frame( code=df$all_cases$code,name=df$all_cases$name, 
                         area= df$all_cases$area, tree = df$all_cases$tree )
      sdt
    })
    
    output$nuts_text <- renderText({
      #data_of_clicks$ids<-""
      txt_<- paste("The level of detail is in ", input$nuts_input)
      txt_
    })
    
    output$total_results <- renderText({
      data <- soil_results$df.pools
      data <- data[data$year == 50,]
      
      n <- nrow(data)
      total_carbon<-0
      
      for(i in 1:n){
        total_carbon<-total_carbon+data$biomass_trunk[i]+data$biomass_roots[i]+
          data$debris[i]+data$soil[i]
      }
      
      text_<- paste("Total amount of carbon after 50 years (in T /ha) is:",as.character(total_carbon))
      text_
      })
    
    
    output$region_selected <- renderText({
      #data_of_clicks$ids = unique(data_of_clicks$ids)
      #data_of_clicks$ids
      #text_ <-""
    
      text_ <-get_region_name(input$nuts_input, data_of_clicks$ids)
      text_
      })
  
    output$table_row_results <- DT::renderDataTable(DT::datatable({
      data <- dplyr::select(soil_results$df.pools, name, code,tree, year, biomass_trunk, biomass_roots, debris, soil)
      
      if (input$time != "All") {
        if(input$time=="every 10 years"){
          data <- data[(data$year %% 10) == 0,]
        }
        if(input$time=="last year"){
          data <- data[data$year == 50,]
        }
      }
      if (input$tree_type != "All") {
        data <- data[data$tree == input$tree_type,]
      }
      data
    }))
    
    output$downloadresults <- downloadHandler(
      
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        paste("climatree_results_demo_",Sys.Date(),".csv",sep="")
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        write.csv2(soil_results$df.pools, file=file, row.names = FALSE)
      }
    )
    
    observeEvent(input$myMap_shape_click, { 
      data_of_clicks$ids <-input$myMap_shape_click$id
      #data_of_clicks$ids <- c( data_of_clicks$ids,input$myMap_shape_click$id)
    })
    
    output$myMap <- renderLeaflet({
      if (input$nuts_input=="nuts1")
        my_data = nuts1_shp
      if (input$nuts_input=="nuts2")
        my_data = nuts2_shp
      if (input$nuts_input=="nuts3")
        my_data = nuts3_shp
      
      map <- leaflet()%>%
        addTiles() %>%
        addPolygons(data = my_data, layerId=my_data$NUTS_ID, weight=3,col = 'red',label= my_data$NAME_LATN)
    })
    
  }

shinyApp(ui, server)
