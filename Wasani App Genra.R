


## All libraries added

library(shiny)
library(data.table)
library(tidyr)
library(tidyverse)
library(ggplot2)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(stringi)
theme_set(theme_bw())
library(shinyBS)
library(plotly)

##data contrains the map of the world.
world <- ne_countries(scale = "medium", returnclass = "sf")


###!! working directory must be set.
setwd("C:/Users/Acer/Desktop/DataVizProject")



##data of wasabi artists with selected columns are selected.

dat<- fread("wasabi_artists.csv",select =c("_id","name","genres","type",
                   "lifeSpan.begin","lifeSpan.ended","lifeSpan.end", "labels","locationInfo",
                   "location.city","location.country"))



#genres text is extracted:
## extract only letters
all_genres<-str_replace_all(dat$genres, "[^[:alnum:]]", " ")

## count number of artists in each genre.
all_genres<-table(unlist(str_split(all_genres, " ")))


##based on the list above this contains the list of chosen genres.
genreListDetail<-c("All","hip","rap","east coast",
                   "blues","jazz","christian", "religious",
                   "classical","ballet","Baroque",
                   "dance",
                   "electronic","electro","bass","house","disco","techno","garage",
                   "pop","chanson","synthpop",
                   "u0026b","soul",
                   "rock", "metal","punk",
                   "country")

## the above list is categorized in wider genre definition.
genreList<-c("All", rep("Hip-Hop/Rap",3),
             "Blues","Jazz",rep("Religious",2),
             rep("Classical",3),
             "Dance",
             rep("Electronic",7),
             rep("Pop",3),
             rep("R&B Soul",2),
             rep("Rock Metal Punk",3),
             "Country"
            )


## creating columns with genres that is coded:
##  1 if the artist has produced album in this genre,
##  0 otherwise.
for(jj in 1:length(genreList))
{
    dat[,genreList[jj]]<-stri_detect_fixed(tolower(dat$genres),genreListDetail[jj])*1
}


##convert in data.frame
dat.df<-as.data.frame(dat)

cntr1<-unlist(lapply(dat.df$locationInfo,function(x) {return(str_split(x,",")[[1]][1])}))
dat.df$cntr2<-str_replace_all(tolower(cntr1), "[^[:alnum:]]", "")

world$name2=str_replace_all(tolower(world$name), "[^[:alnum:]]", "")
dat.df$Country=ifelse(tolower(dat.df$location.country)%in%world$name2,tolower(dat.df$Country),dat.df$cntr2)
dat.df$artistType=ifelse(dat.df$type%in%c("Choir","Other","Orchestra","Character","Group"),"Group",dat.df$type)
typeList<-c("All", unique(dat.df$artistType))
# The function that produces map plot
plotMap <-function(dat.df1,inGenre, inType, plotHeight,plotWidth)
{
    
    # if all 
    if(inType!="All")
    {
        dat.df1 = dat.df1 %>% filter(artistType==inType)
    }
    #if inGenre=="All" then no filtering is done.
    if(inGenre=="All")
    {
        
        ## grouping data by country
        country.df= dat.df1 %>% group_by(country=Country) %>% 
            summarise(no_artists=n())
        
    }
    
    ## if inGenre not "All" then filter data by genre
    if(inGenre!="All")
    {
        
        dat.df1$genre=dat.df1[,inGenre]
        
        ## grouping data by country
        country.df= dat.df1 %>% group_by(country=Country) %>% 
            summarise(no_artists=sum(genre))
        
    }

    ##the countries with no artists in the categories is "Rest"
    country.df$country<-ifelse(country.df$no_artists>=1,country.df$country, "Rest")
    

    #merging world data and wasabi data
    world_artist<-merge(world,country.df, by.x="name2", by.y="country", all.x=T)
    
    ## creating column that contains no. of artist in mio. population.
    world_artist$`Number of artists per Milion population`<-round(world_artist$no_artist/world_artist$pop_est*10^6,2)
    
    #Country name that will be shown in the label.
    world_artist$Country=world_artist$name_long
    
    ##map plot
    p<-ggplot(data = world_artist) +
        geom_sf(aes(fill = `Number of artists per Milion population`,label = Country))+
        scale_fill_gradientn(colours = heat.colors(100,1)[60:1],na.value = "white") +
        guides(fill=guide_legend(title="No. artists/Mil pop.")) 
    
    ##interactive plot (ggplotly)
    p <- ggplotly(p,width=plotWidth,height =plotHeight)
    #p <- ggplotly(p)
    
    p
}




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("No. of artists per million of population"),

    
    fluidRow(
            # Sidebar with a slider input for genre
           column(4,selectInput(
                "genre",
                label="Genre",
                choices=genreList,
                selected = genreList[1],
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
           )),
        column(4,selectInput(
                "type",
                label="ArtistType",
                choices=typeList,
                selected = typeList[1],
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            )),
        column(4,numericInput(inputId = "HeightMap",
                    label = "Height of  Map",
                    min = 300,
                    max = 800,
                    value = c(600), step=5,width="50%")),
        column(4,numericInput(inputId = "WidthMap",
                             label = "Width of Map",
                             min = 400,
                             max = 2000,
                             value = c(600),step=5,width="50%")),
            ##tool tip for genre.
            bsTooltip("genre", "Select genre from the list",
                      "right", options = list(container = "body"))) ,
        
        # Show the map plot.
       fluidRow( mainPanel(
            plotlyOutput("Map")
        ))
    
)

# Define server logic required to draw a map plot.
server <- function(input, output, session) {

    
    output$Map <- renderPlotly({plotMap(
        dat.df,input$genre, input$type,
        input$HeightMap,input$WidthMap)
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
