library(ggplot2)
library(forcats)
library(readr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(plotly)
library(viridis)
library(viridisLite)
library(tigris)
library(geojsonio)
library(readxl)
library(janitor)
library(tibble)
library(leaflet.providers)
library(rgdal)
library(shinythemes)
library(reshape2)
library(sp)
library(RColorBrewer)
library(scales)
library(Hmisc)
library(data.table)
library(maptools)
library(maps)
library(USAboundaries)
library(sf)
library(formattable)

#Veteran Population Data
vetpop <- read_csv('vetandpop.csv')
vetpop1 <- vetpop %>%
  mutate(State = fct_reorder(State, desc(State)))
levels(vetpop1$State)

searchData <- read_csv("search_veterans_programs.csv")


#Veteran Expenditure Data
dat.VA <- read_xlsx("FY19StateExp.xlsx", sheet = 1) %>% clean_names()
dat.VA<-dat.VA[-c(52, 53, 54),]
# dat.VA<-Filter(function(x)!all(is.na(x)), dat.VA)

dat.VAround <- dat.VA%>%
  mutate_if(is.numeric, ~round(., 0))

density_VA <- dat.VAround%>%
  mutate(veteran_density = veteran_population/total_population)

states <- geojsonio::geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

#rename for popup purposes 
plotDat <- geo_join(states, density_VA, by_sp = "name", by_df = "state", how = "inner")
colnames(plotDat@data)[4] = ("State")
colnames(plotDat@data)[6] = ("Total Expenditure")
colnames(plotDat@data)[7] = ("Compensation & Pension")
colnames(plotDat@data)[8] = ("Construction")
colnames(plotDat@data)[9] = ("Ed, Vocational, Rehab, Employment")
colnames(plotDat@data)[11] = ("General Operating")
colnames(plotDat@data)[12] = ("Insurance") 
colnames(plotDat@data)[13] = ("Medical Care")
colnames(plotDat@data)[16] = ("Veteran Density") 

dat_CD<- read_xlsx("FY19StateExp.xlsx", sheet = 2) %>% clean_names()
dat_CDround <- dat_CD%>%
  mutate_if(is.numeric, ~round(., 0))
names(dat_CDround)[2] <- "GEOID"

congress_data<- rgdal::readOGR("tl_2018_us_cd116 copy.json")
#here is the original congressional district data


plotDistDat<-merge(congress_data,dat_CDround, by="GEOID")
#rename for popup purposes 


colnames(plotDistDat@data)[13] = ("State")
colnames(plotDistDat@data)[15] = ("Total Expenditure")
colnames(plotDistDat@data)[16] = ("Compensation & Pension")
colnames(plotDistDat@data)[17] = ("Construction")
colnames(plotDistDat@data)[18] = ("Ed, Vocational, Rehab, Employment")
colnames(plotDistDat@data)[21] = ("Insurance") 
colnames(plotDistDat@data)[22] = ("Medical Care")

ui <- dashboardPage(
  dashboardHeader(title = "RShiny Dashboard"),
  skin = "black",
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Veteran Demographics", tabName = "vetpop", icon = icon("user-friends")),
      menuItem(text = "Veteran Program Searches", tabName = "seo", icon = icon("laptop")),
      menuItem(text = "Expenditures", tabName = "exp", icon = icon("landmark"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "vetpop",
              fluidRow(
              titlePanel(textOutput('vetpop')),
              # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "VetPopSortBy",
                              label = 'Sort By',
                              choices = c('By State', 'By Population'),
                              selected = 'By State'),
                  radioButtons(inputId = "Region", 
                               label = "Region",
                               choices = unique(vetpop1$Region)),
                  sliderInput(inputId = "Year",
                              label = "Year",
                              min = min(vetpop1$Year),
                              max = max(vetpop1$Year),
                              value = '2020',
                              sep = '',
                              animate = TRUE),
                  actionLink("selectallgender", "Select All/Unselect All"),
                  checkboxGroupInput("Gender",
                                     label = "Gender",
                                     choices = unique(vetpop1$Gender),
                                     selected = unique(vetpop1$Gender)),
                  actionLink("selectallage", "Select All/Unselect All"),
                  checkboxGroupInput("Agegroup",
                                     label = "Age Group",
                                     choices = unique(vetpop1$Agegroup),
                                     selected = unique(vetpop1$Agegroup))
                ),
                # Show a plot of the generated distribution
                mainPanel(
                  plotlyOutput("distPlot"),
                  h6('Table 9L: VetPop2018 County-Level Veteran Population by STATE, AGE GROUP, GENDER, 2018-2048. National Center for Veterans Analysis and Statistics.')
                )
              )
              )
      ),
      # Second tab content
      tabItem(tabName = "seo",
              fixedRow(
                titlePanel("Veteran Program Search Volume by Category"),
                # Select Box 
                column(width = 5,
                       selectInput(inputId = "selectService",
                                   label = h4("Service Need Category"),
                                   choices = unique(searchData$Service_Needs_Category),
                                   selected = "Education")), 
              ),
              fixedRow(
                column(width = 10,
                       plotlyOutput(outputId="ServiceSearchPlot", width = 800, height = 550))
                , width = NULL)
              
              
              
      ),
      # Fourth tab Content
      tabItem(tabName = "exp",
              fluidRow(
                # Application title
                titlePanel("Veteran Population and Expenditures"),
                
                # Sidebar with a selectInput to select type of expenditure and radio buttons for color choice
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = 'expmap', label = 'State or District', choices = c('State Level', 'District Level')),
                    selectInput(inputId ="radio", label ="Data Choice",
                                choices = c ("Total Expenditure" =  "Total Expenditure", "Compensation & Pension" = "Compensation & Pension", "Ed, Vocational, Rehab, Employment" = "Ed, Vocational, Rehab, Employment", "Insurance" = "Insurance", "Medical Care" = "Medical Care"),
                                width = '100%',
                                selected = "veteran_population")
                    # radioButtons("colorinput",
                    #              label = h3("Color Selection"),
                    #              choices = list("Orange/Red" = "OrRd",
                    #                             "Blue" = "Blues",
                    #                             "Green" = "Greens", 
                    #                             "Red" = "Reds"),
                    #              selected = "OrRd")
                  ),
                  # Show a plot of the generated distribution
                  mainPanel(
                    div(style = "margin-top: 3em;", # to fix an alignment issue with leaflet (feel free to adjust)
                        leafletOutput(outputId = 'expmap', height = 400, width = 600)
                    )
                  )
                )
              ) 
              )
    )
  )
)


server <- function(input, output, session) {

  #SelectAll/Unselect All for vetpop graph tab
  observe({
    if(input$selectallgender == 0) return(NULL)
    else if(input$selectallgender %%2 ==0)
    {
      updateCheckboxGroupInput(session, "Gender","Gender",choices=unique(vetpop$Gender), selected = unique(vetpop$Gender))
    }
    else{
      updateCheckboxGroupInput(session, "Gender","Gender",choices=unique(vetpop$Gender))
    }
  })
  observe({
    if(input$selectallage == 0) return(NULL)
    else if(input$selectallage %%2 ==0)
    {
      updateCheckboxGroupInput(session, "Agegroup","Age Group",choices=unique(vetpop1$Agegroup), selected = unique(vetpop1$Agegroup))
    }
    else{
      updateCheckboxGroupInput(session, "Agegroup","Age Group",choices=unique(vetpop1$Agegroup))
    }
  })

  #Vetpop Tab
  output$vetpop <- renderText(paste("Veteran Population in the ",input$Region," in ",input$Year))

  observe({
    if(input$VetPopSortBy == 'By Population')
    {
      output$distPlot <- renderPlotly({
        
        
        JplotDat <- vetpop1 %>%
          filter(Year == input$Year) %>%
          filter(Region == input$Region) %>%
          filter(Gender %in% input$Gender) %>%
          filter(Agegroup %in% input$Agegroup) %>%
          group_by(State) %>%
          summarise(VetSum = sum(Veterans),
                    PopSum = sum(Projectedpop)) %>%
          mutate(Percent = VetSum/PopSum * 100, Population = VetSum/1000)
        
        
        vetpopplot <- ggplot(JplotDat, aes(y = Population, x = reorder(State, Population))) +
          scale_fill_gradient(low ="Skyblue", high = "Midnightblue", name = "% of Population") +
          geom_col() + coord_flip() +
          labs(x= "", y = "Population of Veterans (thousands)") +
          theme( 
            panel.grid = element_blank(),
            plot.title = element_text(face='bold'),
            axis.text.y = element_text(size=12, face='bold'),
            axis.ticks.x = element_blank(), 
            panel.background = element_blank()
          ) +
          geom_bar(stat = "Identity", aes(fill = Percent, text = paste0("<b> Population: </b>", round(Population,0),"<br>",round(Percent,2),"% of State Population")))
        ggplotly(vetpopplot, tooltip = 'text')
      })
    }
    else if (input$VetPopSortBy == 'By State')
    {
      output$distPlot <- renderPlotly({
        JplotDat <- vetpop1 %>%
          filter(Year == input$Year) %>%
          filter(Region == input$Region) %>%
          filter(Gender %in% input$Gender) %>%
          filter(Agegroup %in% input$Agegroup) %>%
          group_by(State) %>%
          summarise(VetSum = sum(Veterans),
                    PopSum = sum(Projectedpop)) %>%
          mutate(Percent = VetSum/PopSum * 100, Population = VetSum/1000)
        
        
        vetpopplot <- ggplot(JplotDat, aes(y = Population, x = State, fill = Percent)) +
          scale_fill_gradient(low ="Skyblue", high = "Midnightblue", name = "% of State Population") +
          geom_col() + coord_flip() +
          labs(x= "", y = "Population of Veterans (thousands)") +
          theme( 
            panel.grid = element_blank(),
            plot.title = element_text(face='bold'),
            axis.text.y = element_text(size=12, face='bold'),
            axis.ticks.x = element_blank(), 
            panel.background = element_blank()
          ) +
          geom_bar(stat = "Identity", aes(fill = Percent, text = paste0("<b> Population: </b>", round(Population,0),"<br>",round(Percent,2),"% of State Population")))
        ggplotly(vetpopplot, tooltip = 'text')
      })
    }
  })  
  
#SEO Tab
  output$ServiceSearchPlot <- renderPlotly({
    filtersearchData <- searchData %>% 
      filter(Service_Needs_Category==input$selectService) %>%
      group_by(Service_Needs_Category, Keyword) %>%
      summarise(sumSearchVol = sum(Search_Volume, na.rm = TRUE),.groups = "keep")
    
    ggplotService <- ggplot(data = filtersearchData, aes(x = reorder(Keyword, sumSearchVol), y = sumSearchVol)) +
      geom_bar(stat = "Identity",
               width = 0.5, 
               aes(fill = sumSearchVol, text = paste0("<b>Keyword: </b>",Keyword,"<br><b> Search Volume: </b>",sumSearchVol)),
               show.legend = FALSE,
               size = 4) +
      labs(x = paste0("Keyword Searches Related to ",input$selectService),
           y = "Search Volume (1 month)") +
      theme(axis.text.x = element_text(face = "bold", size = 9, hjust = 1),
            panel.background = element_blank(),
            axis.text.y = element_text(face = "bold", size = 9),
            axis.title.x = element_text(face = "bold", size = 10, hjust = 0.5),
            axis.title.y = element_text(face = "bold", size = 10, hjust = 0.5),
            plot.title = element_text(face = "bold", size = 12, vjust = 3)
            ) +
      coord_flip() +
      scale_fill_gradient(low = 'lightgreen', high = 'darkgreen')
    
    ggplotly(ggplotService, tooltip = 'text')
  })
  


  
#Exp Tab
observe({
  if(input$expmap == 'State Level')
    {
    output$expmap <- renderLeaflet({
      
      pal <- colorNumeric('OrRd', domain = NULL, n = 9)
      
      #popups for each state to show the relevant data regardless of what radio button is selected
      popup_user <-paste0("State: ", plotDat$State,"<br/>","Veteran Population: ", comma(plotDat$veteran_population, digits = 0),"<br/>",capitalize(input$radio),": $", comma(plotDat@data %>% pull(input$radio),digits = 0),"<br>Per Veteran Expenditure: $",round(plotDat@data %>% pull(input$radio)/plotDat$veteran_population,2))
      
      #create the map
      leaflet(plotDat) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 3) %>% 
        addPolygons(fillColor = ~pal(plotDat@data %>% pull(input$radio)), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2,
                    popup = ~popup_user,
                    popupOptions = popupOptions(closeOnClick = TRUE, autoPan = TRUE, autoPanPaddingTopLeft = 150,300))%>%
        addLegend(pal = pal,
                  values = plotDat@data %>% pull(input$radio),
                  position = "topleft",
                  opacity = 0.6,
                  title = NULL)
    })

  }
  else if(input$expmap == 'District Level')
    {
    output$expmap <- renderLeaflet({
      
      pal <- colorNumeric('OrRd', domain = NULL, n = 4)
      
      #popups for each state to show the relevant data regardless of what radio button is selected
      popup_user <-paste0("District: ", plotDistDat$GEOID,"<br/>","Veteran Population: ", comma(plotDistDat$veteran_population, digits = 0),"<br/>",capitalize(input$radio),": $", comma(plotDistDat@data %>% pull(input$radio),digits = 0))
      #create the map
      leaflet(plotDistDat) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-98.483330, 38.712046, zoom = 3) %>% 
        addPolygons(fillColor = ~pal(plotDistDat@data %>% pull(input$radio)), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2,
                    popup = ~popup_user,
                    popupOptions = popupOptions(closeOnClick = TRUE, autoPan = TRUE, autoPanPaddingTopLeft = 150,300))%>%
        addLegend(pal = pal,
                  values = plotDistDat@data %>% pull(input$radio),
                  position = "topleft",
                  opacity = 0.6,
                  title = NULL)
    })
    
    
  }
  
})

}

shinyApp(ui, server)
