library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(countrycode)
library(plotly)
library(googleVis)
library(DT)
library(ggcorrplot)

# Choices used for input
scatter_choices = list("Happiness Score" = 'Score',
                       "GDP per Capita"="GDP.per.capita",
                       "Healthy Life Expectancy"="Healthy.life.expectancy",
                       "Freedom to Make Life Choices"="Freedom.to.make.life.choices",
                       "Generosity"="Generosity",
                       "Perception of Corruption" = 'Perceptions.of.corruption',
                       'Social Support' = 'Social.support',
                       'Dystopia residual' = 'Dystopia.Residual')


df_2015 <- read.csv("./data/2015.csv")
df_2016 <- read.csv("./data/2016.csv")
df_2017 <- read.csv("./data/2017.csv")
df_2018 <- read.csv("./data/2018.csv")
df_2019 <- read.csv("./data/2019.csv")
df_2020 <- read.csv("./data/2020.csv")
df_2021 <- read.csv("./data/2021.csv")
df_2022 <- read.csv("./data/2022.csv")

country_conversion = read.csv("./data/country_conversion.csv")

df_2022$year = 2022
df_2021$year = 2021
df_2020$year = 2020
df_2019$year = 2019
df_2018$year = 2018
df_2017$year = 2017
df_2016$year = 2016
df_2015$year = 2015


df_2015 = df_2015 %>%
  rename(
    Rank = Happiness.Rank,
    Score = Happiness.Score, 
    GDP.per.capita = Economy..GDP.per.Capita.,
    Healthy.life.expectancy = Health..Life.Expectancy.,
    Freedom.to.make.life.choices = Freedom, 
    Perceptions.of.corruption = Trust..Government.Corruption.,
    Social.support = Family
  )

df_2016 = df_2016 %>%
  rename(
    Rank = Happiness.Rank,
    Score = Happiness.Score, 
    GDP.per.capita = Economy..GDP.per.Capita.,
    Healthy.life.expectancy = Health..Life.Expectancy.,
    Freedom.to.make.life.choices = Freedom, 
    Perceptions.of.corruption = Trust..Government.Corruption.,
    Social.support = Family
  )

df_2017 = df_2017 %>%
  rename(
    Rank = Happiness.Rank,
    Score = Happiness.Score, 
    GDP.per.capita = Economy..GDP.per.Capita.,
    Healthy.life.expectancy = Health..Life.Expectancy.,
    Freedom.to.make.life.choices = Freedom, 
    Perceptions.of.corruption = Trust..Government.Corruption.,
    Social.support = Family
  )

df_2018$Perceptions.of.corruption = as.numeric(df_2018$Perceptions.of.corruption)
df_2018 = df_2018 %>%
  rename(
    Rank = Overall.rank,
    Country = Country.or.region
  )%>%
  mutate(Dystopia.Residual = Score-rowSums(.[4:9], na.rm=TRUE))

df_2019 = df_2019 %>%
  rename(
    Rank = Overall.rank,
    Country = Country.or.region
  ) %>%
  mutate(Dystopia.Residual = Score-rowSums(.[4:9], na.rm=TRUE))

df_2020 <- cbind(Rank = rownames(df_2020), df_2020)
rownames(df_2020) <- 1:nrow(df_2020)
df_2020$Rank = as.numeric(df_2020$Rank)
df_2020 <- df_2020[-c(3,5,6,7,8,9,10,11,12,13,14)]
df_2020 = df_2020 %>%
  rename(
    Country = Country.name,
    Score = Ladder.score, 
    GDP.per.capita = Explained.by..Log.GDP.per.capita,
    Healthy.life.expectancy = Explained.by..Healthy.life.expectancy,
    Freedom.to.make.life.choices = Explained.by..Freedom.to.make.life.choices, 
    Perceptions.of.corruption = Explained.by..Perceptions.of.corruption,
    Social.support = Explained.by..Social.support,
    Generosity = Explained.by..Generosity
  )

df_2021 <- cbind(Rank = rownames(df_2021), df_2021)
rownames(df_2021) <- 1:nrow(df_2021)
df_2021$Rank = as.numeric(df_2021$Rank)
df_2021 <- df_2021[-c(3,5,6,7,8,9,10,11,12,13,14)]
df_2021 = df_2021 %>%
  rename(
    Country = Country.name,
    Score = Ladder.score, 
    GDP.per.capita = Explained.by..Log.GDP.per.capita,
    Healthy.life.expectancy = Explained.by..Healthy.life.expectancy,
    Freedom.to.make.life.choices = Explained.by..Freedom.to.make.life.choices, 
    Perceptions.of.corruption = Explained.by..Perceptions.of.corruption,
    Social.support = Explained.by..Social.support,
    Generosity = Explained.by..Generosity
  )

df_2015 <- df_2015[-c(2,5)]
df_2017 <-  df_2017[-c(4,5)]
df_2016 <-  df_2016[-c(2,5,6)]
total_df = bind_rows(df_2015,df_2016,df_2017,df_2018,df_2019,df_2020,df_2021)
total_df = total_df[-c(14,15,16,17,18,19)]
# create continent and country code columns
total_df$code = countrycode(total_df$Country, origin='country.name',destination='iso3c', custom_match = c(Kosovo = "KSV"))
country_conversion = country_conversion %>%
  select(code_3,continent,sub_region)
total_df = total_df %>%
  left_join(country_conversion,by = c('code'='code_3'))

column_order = c('year','sub_region','continent','Country','code','Rank','Score', 'GDP.per.capita', 'Healthy.life.expectancy' ,'Freedom.to.make.life.choices', 'Generosity', 'Perceptions.of.corruption','Social.support',"Dystopia.Residual" )
total_df = total_df[,column_order]
total_df <- filter(total_df, code != 'KSV')


ui <- fluidPage(
  theme=shinytheme("united"),
  tags$head(
    tags$style(HTML(".navbar .navbar-nav {float: right;
        font-size:20px }
                        "))
  ),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 }"
  )
  ),
  
  # Navigation bar that is always on the top of the screen
  navbarPage(
    title="Happiness Scores Around the World",
    id="nav",
    position="static-top",
    #World Map page
    tabPanel("WORLD MAP", icon=icon('globe'),
             fluidRow(
               h3('Happiness Scores Around the World'),
               h4('Chloropeth showing Happiness across all the regions'),
               h5('Grey area indiacates no data available'),
               h6('Globe Map can be rotated and zoomed'),
               br(),
               h4('Data is taken from:'),
               tags$a(href="https://worldhappiness.report", "World Happiness Report")
             ),
             fluidRow(
               column(3,
                      br(),
                      sliderInput(
                        inputId="worldmap_year",
                        label="Select Year:",
                        min=2015, max=2021,
                        value=2021,
                        sep="")
               ),

               column(6,
                      plotlyOutput('worldmap'))
             )
             
    ),
    
    
    tabPanel("Table",
             icon=icon('table'),
             sidebarPanel(width = 3,
                          br(),
                          span("This section depicts Most Happiest and Least Happiest country"),
                          br(), hr(),
                          helpText("Select year"),
                          actionButton("button2015", "2015"),
                          actionButton("button2016", "2016"),
                          actionButton("button2017", "2017"),
                          actionButton("button2018", "2018"),
                          br(),br(),
                          actionButton("button2019", "2019"),
                          actionButton("button2020", "2020"),
                          actionButton("button2021", "2021"),
                          br(), br(), 
                          span("Data source:", 
                               tags$a("Kaggle",
                                      href = "https://www.kaggle.com/datasets/mathurinache/world-happiness-report?datasetId=748584&searchQuery=R")),
                          br(), br()
    
                          ),
             mainPanel(
                            
                            tabPanel(textOutput("tab_name_rankings"),
                                     h1(textOutput("year_name")),
                                     h2("Most Happiest 10"),
                                     wellPanel( 
                                       dataTableOutput("rank_table_top")),
                                     br(), 
                                     h2("Least Happiest 10"),
                                     wellPanel(
                                       dataTableOutput("rank_table_bottom"))
                            )
             )

                          ),
    
    # Explore Page
    tabPanel("Corelation",icon = icon("poll"),
             tabsetPanel(type='tabs', id='chart_tabs',
                         
                         # Variables tab within Explore Page
                         tabPanel("VARIABLES",
                                  fluidRow(
                                    br(),
                                    h4("This section depicts correlation between attributes of the dataset."),
                                    column(3,
                                           sliderInput(
                                             inputId = 'variables_year',
                                             label = 'Select Years:',
                                             min=2015, max=2021,
                                             value=c(2015,2021),
                                             sep=''
                                           ),
                                           selectizeInput(
                                             inputId = "var1",
                                             label = h5(strong("Select a variable for x-axis:")),
                                             choices = scatter_choices
                                           ),
                                           selectizeInput(
                                             inputId = "var2",
                                             label = h5(strong("Select a variable for y-axis:")),
                                             choices = scatter_choices
                                           ),
                                           h4(strong(htmlOutput("correlation")))
                                    ),
                                    column(9,
                                           plotlyOutput('scatter'))
                                  )
                              
                         ))),
  
            
  
  tabPanel("About",
           icon = icon('tree'),
           theme=shinytheme("superhero"),
           fluidRow(
             h3(strong("Context"), style = "text-align: left"),
             h4("The World Happiness Report (WHR) has attracted international attention since its inception in 2012, as it can assist policymakers in evaluating their policy alternatives. The United Nations Sustainable Development Solutions Network publishes the World Happiness Report. It includes articles and rankings of national happiness, which are based on respondents' assessments of their personal lives, which the report also compares with numerous (quality of) life aspects.
"),
            h3(strong("Content"),style = "text-align: left"),
             h4("The happiness scores and rankings use data from the Gallup World 
            Poll. The scores are based on answers to the main life evaluation 
            question asked in the poll. This question, known as the Cantril 
            ladder, asks respondents to think of a ladder with the best possible 
            life for them being a 10 and the worst possible life being a 0 and 
            to rate their own current lives on that scale. 
            From Kaggle World Happiness Report of 2021 is being used.
            This dataset uses the data from the Gallup World Poll.
            Data is collected from people in over 156 countries. 
            Each variable measured reveals a populated weighted average score on a scale running from 0 to 10 that
            is tracked over time and compared against other countries.The rankings of national happiness are based on a Cantril ladder survey. 
            Nationally representative samples of respondents are asked to think of a ladder, with the best possible life for them being a 
            10, and the worst possible life being a 0. They are then asked to rate their own current lives on that 0 to 10 scale. Apart
            from happiness score which is called Ladder.score in dataset scoring is done based on the following features :"),
            h4("1. GPD per capita"),
            h4("2. Social support"),
            h4("3. Healthy life expectancy"),
            h4("4. Freedom to make life choices"),
            h4("5. Generosity"),
            h4("6. Perception of corruption."),
           
            h3(strong("Instructions"),style = "text-align: left"),
            h4("The App contains 4 sections :"),
            h4("1. World Map - This section contains an interactive choropleth map of the world which shows happiness scores . The map can be rotated and zoomed and by clicking on a specific country it hovers and shows the happiness score. Person can see the happiness score of various years by changing the slider for the specific year ."),
            h4("1. Table - This section shows the most Happiest and Least Happiest country"),
            h4("2. Correlation- This section contains a scatter plot that shows the relation between different attributes/quality of life factors and happiness . Users can select and see the relation between two attributes which are distributed along the region . Each dot can be hovered upon to see the country name and the score of two variables selected. The correlation is also calculated and shown on the right."),
            h4("3. About - This section gives the context and information about the dataset.")
           
           )
           
           )
  
  
  )
)


server <- function(input, output, session) {
  
 
    
    react_data <- reactive({
      map_happiness = total_df %>%
        select(c('year','Score','Country','code',input$happiness_features,'Dystopia.Residual')) %>%
        filter(year == input$worldmap_year)

    })
    g <- list(
      projection = list(
        type = 'orthographic'
      ),
      showland = TRUE,
      landcolor = toRGB("#e5ecf6")
    )
    # Create output plot of world map
    output$worldmap <- renderPlotly({
      react_data() %>%
        plot_geo() %>%
        add_trace(z = ~Score,
                  color = ~Score,
                  colors = 'Orange',
                  text = ~Country,
                  locations = ~code,
                  marker = list(line = l)
        ) %>%
        colorbar(title = 'Score') %>%
        layout(title = 'Happiness Score', xaxis = list(title = list(text ='Drag and zoom to observe Happiness of other regions')), geo = g )  
      
    })
    
    
    output$variable_1 <- renderUI({
      selectInput("variable_1", "Y-variable:", 
                  choices = c("Rank", "Score", "GDP.per.capita", 
                              "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption", 
                              "Social.support"), selected = "Score")
    })
    
    
    output$variable_2 <- renderUI({
      selectInput("variable_2", "X-variable:", 
                  choices = c("Rank", "Score", "GDP.per.capita", 
                              "Healthy.life.expectancy", "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption", 
                              "Social.support"), selected = "Social.support")
    })
    
    
    data_year <- reactiveValues(data = filter(total_df, year == 2015), year = "2015")
    
    
    observeEvent(input$button2015, {
      data_year$data <- filter(total_df, year == 2015)
      year <- "2015"
    })
    
    
    observeEvent(input$button2016, {
      data_year$data <- filter(total_df, year == 2016)
      data_year$year <- "2016"
    })
    
    observeEvent(input$button2017, {
      data_year$data <- filter(total_df, year == 2017)
      data_year$year <-  "2017"
    })
    
    observeEvent(input$button2018, {
      data_year$data <- filter(total_df, year == 2018)
      data_year$year <-  "2018"
    })
    
    observeEvent(input$button2019, {
      data_year$data <- filter(total_df, year == 2019)
      data_year$year <-  "2019"
    })
    
    observeEvent(input$button2020, {
      data_year$data <- filter(total_df, year == 2020)
      data_year$year <-  "2020"
    })
    
    observeEvent(input$button2021, {
      data_year$data <- filter(total_df, year == 2021)
      data_year$year <-  "2021"
    })
    
    
    filtered_data <- reactive({
      na.omit(data_year$data) 
        # filter(Country %in% c(input$country)) %>% 
        # arrange(Country)
    })
    
    output$scatterplot <- renderPlotly({
      if (is.null(data_year$data)) return()
      
      p <- ggplot(filtered_data()) +
        geom_point(aes_string(x = input$variable_2, y = input$variable_1, 
                              colour = "sub_region", label = "Country"), size = 3) +
        ggtitle(paste0(input$variable_1, " vs. ", input$variable_2)) +
        theme_bw() +
        theme(legend.position = "bottom")
      
      ggplotly(p, tooltip = c("label", "x", "y"), height = 500) 
    })
    
    # Create reactive data frame for scatter plots
    happiness_scatter = reactive({
      happiness_scatter = total_df %>%
        filter(!is.na(get(input$var1)),
               !is.na(get(input$var2)),
               year %in% as.numeric(input$variables_year)
        )
    })
    
    # output for correlation
    output$correlation = renderText({
      x=happiness_scatter() %>%
        select(input$var1)
      y=happiness_scatter() %>%
        select(input$var2)
      corr = round(cor(x, y), digits = 5)
      
      paste('Correlation:',as.character(corr))
    })
    
    output$scatter = renderPlotly(
      happiness_scatter() %>%
        plot_ly(x= ~get(input$var1), 
                y= ~get(input$var2),
                color= ~continent,
                text= ~paste("Country: ",Country, "\nYear: ",year),
                type='scatter',
                mode='markers'
        ) %>%
        add_trace(x= ~get(input$var1),
                  y=fitted(lm(get(input$var2)~get(input$var1), data=happiness_scatter())),
                  color='',
                  mode = "lines",
                  name = 'Predicted') %>%
        layout(
          title = paste(input$var1, 'vs', input$var2),
          xaxis = list(title = input$var1),
          yaxis = list(title = input$var2)
        )
    )
    

    
    output$tab_name_rankings <- renderText({ 
      paste("Happiness Ranking: ", data_year$year) 
    })
    
    
    output$year_name <- renderText({ 
      paste("Happiness Ranking in ", data_year$year) 
    })
    
    
    output$rank_table_top <- DT::renderDataTable({
      if (is.null(data_year$data)) {
        return()
      } else {
        data_year$data %>% 
          mutate(Rank = Rank) %>% 
          arrange(Rank) %>% 
          top_n(10, desc(Rank))
      }
    },
    options = list(lengthChange = FALSE,
                   scrollX = "100%")
    )
    
    
    output$rank_table_bottom <- DT::renderDataTable({
      if (is.null(data_year$data)) {
        return()
      } else {
        data_year$data %>% 
          mutate(Rank = Rank) %>% 
          arrange(Rank) %>% 
          top_n(10, Rank)
      }
    },
    
    options = list(lengthChange = FALSE, 
                   scrollX = "100%")
    )
  
}






# Run the application 
shinyApp(ui = ui, server = server)