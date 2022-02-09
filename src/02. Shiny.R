# R Project 
#Fernando Delgado, Nithesh Ramanna, Aazad Ghoslya

#===============================================================================
# Libraries
#===============================================================================

library(ggplot2)
library(tidyr)
library(tidyverse)
library(maps)
library(readr)
library(sf)
library(shinythemes)

#===============================================================================

#Load data
datamart <- read.csv(file = '~/Group Assignment - Team 13/data/processed/datamart.csv')

ui <- navbarPage("Betting apps analysis",
                 theme = shinytheme('cosmo'),
                 tabPanel("Users",
                          mainPanel(
                            plotOutput("appUsers")
                          )
                 ),
                 tabPanel("Application Usage",
                          mainPanel(
                            plotOutput("appUsage")
                          )
                 ),
                 tabPanel("Users Language",
                          mainPanel(
                            plotOutput("userLanguage")
                          )
                 ),
                 tabPanel("Users Countries",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plotCountry",choices=c('Map',
                                                                  'Bar'),
                                          selected = 'Bar',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("CountryPlot")
                            )
                          )
                 ),
                 tabPanel("Customer Levels",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("customerType",choices=c('Pie',
                                                                   'Bar'),
                                          selected = 'Pie',label = "Select the plot")
                            ),
                            mainPanel(
                              
                              textOutput('customerText'),
                              plotOutput("customerLevel")
                              
                            )
                          )
                 ),
                 
                 tabPanel("Sports book fixed-odd",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plottype1",choices=c('Total Winnings vs Total Stakes',
                                                                'Total Winnings vs Total bets', 
                                                                'Life time value by Customer Level',
                                                                'Average daily Stake by Customer Level',
                                                                'Average daily winnings by Customer Level',
                                                                'Average total bets by Customer Level'),
                                          selected = 'Total Winnings vs Total Stakes',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("product1Plot")
                            )
                          )
                 ),
                 tabPanel("Sports book live-action",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plottype2",choices=c('Total Winnings vs Total Stakes',
                                                                'Total Winnings vs Total bets', 
                                                                'Life time value by Customer Level',
                                                                'Average daily Stake by Customer Level',
                                                                'Average daily winnings by Customer Level',
                                                                'Average total bets by Customer Level'),
                                          selected = 'Total Winnings vs Total Stakes',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("product2Plot")
                            )
                          )
                 ),
                 tabPanel("Casino BossMedia",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plottype4",choices=c('Total Winnings vs Total Stakes',
                                                                'Total Winnings vs Total bets', 
                                                                'Life time value by Customer Level',
                                                                'Average daily Stake by Customer Level',
                                                                'Average daily winnings by Customer Level',
                                                                'Average total bets by Customer Level'),
                                          selected = 'Total Winnings vs Total Stakes',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("product4Plot")
                            )
                          )
                 ),
                 tabPanel("Supertoto",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plottype5",choices=c('Total Winnings vs Total Stakes',
                                                                'Total Winnings vs Total bets', 
                                                                'Life time value by Customer Level',
                                                                'Average daily Stake by Customer Level',
                                                                'Average daily winnings by Customer Level',
                                                                'Average total bets by Customer Level'),
                                          selected = 'Total Winnings vs Total Stakes',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("product5Plot")
                            )
                          )
                 ),
                 tabPanel("Games VS",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plottype6",choices=c('Total Winnings vs Total Stakes',
                                                                'Total Winnings vs Total bets', 
                                                                'Life time value by Customer Level',
                                                                'Average daily Stake by Customer Level',
                                                                'Average daily winnings by Customer Level',
                                                                'Average total bets by Customer Level'),
                                          selected = 'Total Winnings vs Total Stakes',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("product6Plot")
                            )
                          )
                 ),
                 tabPanel("Games bwin",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plottype7",choices=c('Total Winnings vs Total Stakes',
                                                                'Total Winnings vs Total bets', 
                                                                'Life time value by Customer Level',
                                                                'Average daily Stake by Customer Level',
                                                                'Average daily winnings by Customer Level',
                                                                'Average total bets by Customer Level'),
                                          selected = 'Total Winnings vs Total Stakes',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("product7Plot")
                            )
                          )
                 ),
                 tabPanel("Casino Chartwell",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("plottype8",choices=c('Total Winnings vs Total Stakes',
                                                                'Total Winnings vs Total bets', 
                                                                'Life time value by Customer Level',
                                                                'Average daily Stake by Customer Level',
                                                                'Average daily winnings by Customer Level',
                                                                'Average total bets by Customer Level'),
                                          selected = 'Total Winnings vs Total Stakes',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("product8Plot")
                            )
                          )
                 ),
                 tabPanel("Poker",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("pokerPlot",choices=c('Poker buy vs Poker Sell',
                                                                'Life time value by customer Level',
                                                                'Average frequency of play by customer level'),
                                          selected = 'Poker buy vs Poker Sell',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("plotPoker")
                            )
                          )
                 ),
                 
                 tabPanel("Total Aggregations",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("aggregations",choices=c('Overview of stakes, winnings and Bets',
                                                                   'Total cost V/s Total Revenue',
                                                                   'Life time value per customer Level',
                                                                   'Average frequency of play by customer level',
                                                                   'Profit margin'),
                                          selected = 'Overview of stakes, winnings and Bets',label = "Select the plot")
                            ),
                            mainPanel(
                              plotOutput("aggregationsPlot")
                            )
                          )
                 )
                 
)



# Define the server logic
server <- function(input, output) {
  
  output$appUsers <- renderPlot({
    
    #Gender Ratio
    ggplot(datamart,aes(x = Gender, color = Gender, fill = Gender))+
      geom_bar()+
      ggtitle("Gender Ratio")
    
  })
  output$appUsage <- renderPlot({
    
    #Application Usage
    ggplot(datamart, aes(x = Application))+
      geom_bar()+
      theme(axis.text.x=element_text(angle=90, hjust=1))+
      ggtitle("Applications used for bettings")
    
  })
  output$userLanguage <- renderPlot({
    
    #Languages
    ggplot(datamart,aes(x = Language))+
      geom_bar()+
      theme(axis.text.x=element_text(angle=90, hjust=1))+
      ggtitle('Languages spoken by users')
    
  })
  output$userLanguage <- renderPlot({
    
    #Languages
    ggplot(datamart,aes(x = Language))+
      geom_bar()+
      theme(axis.text.x=element_text(angle=90, hjust=1))+
      ggtitle('Languages spoken by users')
    
  })
  output$CountryPlot <- renderPlot({
    
    if(input$plotCountry == 'Map'){
      #Users from different parts of the word
      countries <- datamart %>% group_by(Country) %>% summarise(count= n()) %>% rename(region = Country)
      
      world_map <- map_data('world')
      
      world_map2 <- world.cities %>% filter(capital==1)%>%  rename(region = country.etc)
      users_in_world = merge(countries, world_map2, by = 'region')
      
      ggplot() +
        geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill="black", alpha=1)+
        geom_point( data=users_in_world, aes(x=long, y=lat, size=count), color = 'green')
    }
    else {
      #Countries with more than 1000 users
      countries <- datamart %>% group_by(Country) %>% summarise(count= n()) %>% rename(region = Country)
      
      ggplot(countries %>% filter(count > 1000), aes(x = region, y = count))+
        geom_col()+
        scale_x_discrete(name = 'Country')+
        scale_y_continuous(name = 'Number of Users')+
        ggtitle('Countries with more than 1000 users')
    }
    
  })
  
  
  output$product1Plot <- renderPlot({
    
    if(input$plottype1 == 'Total Winnings vs Total Stakes'){
      #Total winnings vs Total stakes for product2
      ggplot(datamart, aes(x = TotalStakes_Product1, y = TotalWinnings_Product1, color = Gender), size = 10)+
        geom_point()+
        ggtitle('Total winnings vs Total stakes for Sports book fixed-odd')+
        scale_x_continuous(name = 'Total Stakes')+
        scale_y_continuous(name = 'Total Winnings')
    } else if (input$plottype1 == 'Total Winnings vs Total bets'){
      #Total winnings vs Total bets for product2
      ggplot(datamart, aes(x = TotalBets_Product1, y = TotalWinnings_Product1, color = Gender), size = 10)+
        geom_point()+
        ggtitle('Total winnings vs Total Bets for Sports book fixed-odd')+
        scale_x_continuous(name = 'Total Bets')+
        scale_y_continuous(name = 'Total Winnings')
    }else if(input$plottype1 == 'Average daily Stake by Customer Level'){
      #Average Daily Stakes
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyStake_Product1)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyStake = mean(AverageDailyStake_Product1))%>% arrange(AverageDailyStake)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyStake))+
        geom_col()+
        ggtitle('Average Daily Stakes by Customer level for Sports book fixed-odd')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Stakes')
      
    }else if(input$plottype1 == 'Life time value by Customer Level'){
      #Average Total Cost per customer level
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(LifetimeValue_Product1)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(LifetimeValue_Product1))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level for Sports book fixed-odd')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if(input$plottype1 == 'Average daily winnings by Customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyWinning_Product1)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyWinnings = mean(AverageDailyWinning_Product1))%>% arrange(AverageDailyWinnings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyWinnings))+
        geom_col()+
        ggtitle('Average Daily Winnings by Customer level for Sports book fixed-odd')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Winnings')
      
    }else {
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyBets_Product1)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyBettings = mean(AverageDailyBets_Product1))%>% arrange(AverageDailyBettings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyBettings))+
        geom_col()+
        ggtitle('Average Daily bettings by Customer level for Sports book fixed-odd')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Bettings')
    }
  })  
  
  output$product2Plot <- renderPlot({
    
    if(input$plottype2 == 'Total Winnings vs Total Stakes'){
      #Total winnings vs Total stakes for product2
      ggplot(datamart, aes(x = TotalStakes_Product2, y = TotalWinnings_Product2, color = Gender), size = 20)+
        geom_point()+
        ggtitle('Total winnings vs Total stakes for Sports book live-action')+
        scale_x_continuous(name = 'Total Stakes')+
        scale_y_continuous(name = 'Total Winnings')
    } else if (input$plottype2 == 'Total Winnings vs Total bets'){
      #Total winnings vs Total bets for product2
      ggplot(datamart, aes(x = TotalBets_Product2, y = TotalWinnings_Product2, color = Gender), size = 20)+
        geom_point()+
        ggtitle('Total winnings vs Total Bets for Sports book live-action')+
        scale_x_continuous(name = 'Total Bets')+
        scale_y_continuous(name = 'Total Winnings')
    }else if(input$plottype2 == 'Average daily Stake by Customer Level'){
      #Average Daily Stakes
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyStake_Product2)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyStake = mean(AverageDailyStake_Product2))%>% arrange(AverageDailyStake)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyStake))+
        geom_col()+
        ggtitle('Average Daily Stakes by Customer level for Sports book live-action')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Stakes')
      
    }else if(input$plottype2 == 'Life time value by Customer Level'){
      #Average Total Cost per customer level
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(LifetimeValue_Product2)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(LifetimeValue_Product2))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level for Sports book live-action')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if(input$plottype2 == 'Average daily winnings by Customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyWinning_Product2)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyWinnings = mean(AverageDailyWinning_Product2))%>% arrange(AverageDailyWinnings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyWinnings))+
        geom_col()+
        ggtitle('Average Daily Winnings by Customer level for Sports book live-action')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Winnings')
      
    }else {
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyBets_Product2)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyBettings = mean(AverageDailyBets_Product2))%>% arrange(AverageDailyBettings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyBettings))+
        geom_col()+
        ggtitle('Average Daily bettings by Customer level for Sports book live-action')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Bettings')
    }
  })  
  
  output$product4Plot <- renderPlot({
    
    if(input$plottype4 == 'Total Winnings vs Total Stakes'){
      #Total winnings vs Total stakes for product4
      ggplot(datamart, aes(x = TotalStakes_Product4, y = TotalWinnings_Product4, color = Gender), size = 40)+
        geom_point()+
        ggtitle('Total winnings vs Total stakes for Casino BossMedia')+
        scale_x_continuous(name = 'Total Stakes')+
        scale_y_continuous(name = 'Total Winnings')
    } else if (input$plottype4 == 'Total Winnings vs Total bets'){
      #Total winnings vs Total bets for product4
      ggplot(datamart, aes(x = TotalBets_Product4, y = TotalWinnings_Product4, color = Gender), size = 40)+
        geom_point()+
        ggtitle('Total winnings vs Total Bets for Casino BossMedia')+
        scale_x_continuous(name = 'Total Bets')+
        scale_y_continuous(name = 'Total Winnings')
    }else if(input$plottype4 == 'Average daily Stake by Customer Level'){
      #Average Daily Stakes
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyStake_Product4)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyStake = mean(AverageDailyStake_Product4))%>% arrange(AverageDailyStake)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyStake))+
        geom_col()+
        ggtitle('Average Daily Stakes by Customer level for Casino BossMedia')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Stakes')
      
    }else if(input$plottype4 == 'Life time value by Customer Level'){
      #Average Total Cost per customer level
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(LifetimeValue_Product4)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(LifetimeValue_Product4))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level for Casino BossMedia')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if(input$plottype4 == 'Average daily winnings by Customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyWinning_Product4)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyWinnings = mean(AverageDailyWinning_Product4))%>% arrange(AverageDailyWinnings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyWinnings))+
        geom_col()+
        ggtitle('Average Daily Winnings by Customer level for Casino BossMedia')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Winnings')
      
    }else {
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyBets_Product4)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyBettings = mean(AverageDailyBets_Product4))%>% arrange(AverageDailyBettings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyBettings))+
        geom_col()+
        ggtitle('Average Daily bettings by Customer level for Casino BossMedia')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Bettings')
    }
  })  
  
  output$product5Plot <- renderPlot({
    
    if(input$plottype5 == 'Total Winnings vs Total Stakes'){
      #Total winnings vs Total stakes for product5
      ggplot(datamart, aes(x = TotalStakes_Product5, y = TotalWinnings_Product5, color = Gender), size = 50)+
        geom_point()+
        ggtitle('Total winnings vs Total stakes for Supertoto')+
        scale_x_continuous(name = 'Total Stakes')+
        scale_y_continuous(name = 'Total Winnings')
    } else if (input$plottype5 == 'Total Winnings vs Total bets'){
      #Total winnings vs Total bets for product5
      ggplot(datamart, aes(x = TotalBets_Product5, y = TotalWinnings_Product5, color = Gender), size = 50)+
        geom_point()+
        ggtitle('Total winnings vs Total Bets for Supertoto')+
        scale_x_continuous(name = 'Total Bets')+
        scale_y_continuous(name = 'Total Winnings')
    }else if(input$plottype5 == 'Average daily Stake by Customer Level'){
      #Average Daily Stakes
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyStake_Product5)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyStake = mean(AverageDailyStake_Product5))%>% arrange(AverageDailyStake)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyStake))+
        geom_col()+
        ggtitle('Average Daily Stakes by Customer level for Supertoto')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Stakes')
      
    }else if(input$plottype5 == 'Life time value by Customer Level'){
      #Average Total Cost per customer level
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(LifetimeValue_Product5)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(LifetimeValue_Product5))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level for Supertoto')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if(input$plottype5 == 'Average daily winnings by Customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyWinning_Product5)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyWinnings = mean(AverageDailyWinning_Product5))%>% arrange(AverageDailyWinnings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyWinnings))+
        geom_col()+
        ggtitle('Average Daily Winnings by Customer level for Supertoto')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Winnings')
      
    }else {
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyBets_Product5)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyBettings = mean(AverageDailyBets_Product5))%>% arrange(AverageDailyBettings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyBettings))+
        geom_col()+
        ggtitle('Average Daily bettings by Customer level for Supertoto')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Bettings')
    }
  })  
  
  output$product6Plot <- renderPlot({
    
    if(input$plottype6 == 'Total Winnings vs Total Stakes'){
      #Total winnings vs Total stakes for product6
      ggplot(datamart, aes(x = TotalStakes_Product6, y = TotalWinnings_Product6, color = Gender), size = 60)+
        geom_point()+
        ggtitle('Total winnings vs Total stakes for Games VS')+
        scale_x_continuous(name = 'Total Stakes')+
        scale_y_continuous(name = 'Total Winnings')
    } else if (input$plottype6 == 'Total Winnings vs Total bets'){
      #Total winnings vs Total bets for product6
      ggplot(datamart, aes(x = TotalBets_Product6, y = TotalWinnings_Product6, color = Gender), size = 60)+
        geom_point()+
        ggtitle('Total winnings vs Total Bets for Games VS')+
        scale_x_continuous(name = 'Total Bets')+
        scale_y_continuous(name = 'Total Winnings')
    }else if(input$plottype6 == 'Average daily Stake by Customer Level'){
      #Average Daily Stakes
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyStake_Product6)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyStake = mean(AverageDailyStake_Product6))%>% arrange(AverageDailyStake)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyStake))+
        geom_col()+
        ggtitle('Average Daily Stakes by Customer level for Games VS')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Stakes')
      
    }else if(input$plottype6 == 'Life time value by Customer Level'){
      #Average Total Cost per customer level
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(LifetimeValue_Product6)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(LifetimeValue_Product6))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level for Games VS')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if(input$plottype6 == 'Average daily winnings by Customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyWinning_Product6)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyWinnings = mean(AverageDailyWinning_Product6))%>% arrange(AverageDailyWinnings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyWinnings))+
        geom_col()+
        ggtitle('Average Daily Winnings by Customer level for Games VS')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Winnings')
      
    }else {
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyBets_Product6)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyBettings = mean(AverageDailyBets_Product6))%>% arrange(AverageDailyBettings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyBettings))+
        geom_col()+
        ggtitle('Average Daily bettings by Customer level for Games VS')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Bettings')
    }
  })  
  
  output$product7Plot <- renderPlot({
    
    if(input$plottype7 == 'Total Winnings vs Total Stakes'){
      #Total winnings vs Total stakes for product7
      ggplot(datamart, aes(x = TotalStakes_Product7, y = TotalWinnings_Product7, color = Gender), size = 70)+
        geom_point()+
        ggtitle('Total winnings vs Total stakes for Games bwin')+
        scale_x_continuous(name = 'Total Stakes')+
        scale_y_continuous(name = 'Total Winnings')
    } else if (input$plottype7 == 'Total Winnings vs Total bets'){
      #Total winnings vs Total bets for product7
      ggplot(datamart, aes(x = TotalBets_Product7, y = TotalWinnings_Product7, color = Gender), size = 70)+
        geom_point()+
        ggtitle('Total winnings vs Total Bets for Games bwin')+
        scale_x_continuous(name = 'Total Bets')+
        scale_y_continuous(name = 'Total Winnings')
    }else if(input$plottype7 == 'Average daily Stake by Customer Level'){
      #Average Daily Stakes
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyStake_Product7)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyStake = mean(AverageDailyStake_Product7))%>% arrange(AverageDailyStake)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyStake))+
        geom_col()+
        ggtitle('Average Daily Stakes by Customer level for Games bwin')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Stakes')
      
    }else if(input$plottype7 == 'Life time value by Customer Level'){
      #Average Total Cost per customer level
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(LifetimeValue_Product7)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(LifetimeValue_Product7))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level for Games bwin')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if(input$plottype7 == 'Average daily winnings by Customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyWinning_Product7)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyWinnings = mean(AverageDailyWinning_Product7))%>% arrange(AverageDailyWinnings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyWinnings))+
        geom_col()+
        ggtitle('Average Daily Winnings by Customer level for Games bwin')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Winnings')
      
    }else {
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyBets_Product7)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyBettings = mean(AverageDailyBets_Product7))%>% arrange(AverageDailyBettings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyBettings))+
        geom_col()+
        ggtitle('Average Daily bettings by Customer level for Games bwin')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Bettings')
    }
  })  
  
  output$product8Plot <- renderPlot({
    
    if(input$plottype8 == 'Total Winnings vs Total Stakes'){
      #Total winnings vs Total stakes for product8
      ggplot(datamart, aes(x = TotalStakes_Product8, y = TotalWinnings_Product8, color = Gender), size = 80)+
        geom_point()+
        ggtitle('Total winnings vs Total stakes for Casino Chartwell')+
        scale_x_continuous(name = 'Total Stakes')+
        scale_y_continuous(name = 'Total Winnings')
    } else if (input$plottype8 == 'Total Winnings vs Total bets'){
      #Total winnings vs Total bets for product8
      ggplot(datamart, aes(x = TotalBets_Product8, y = TotalWinnings_Product8, color = Gender), size = 80)+
        geom_point()+
        ggtitle('Total winnings vs Total Bets for Casino Chartwell')+
        scale_x_continuous(name = 'Total Bets')+
        scale_y_continuous(name = 'Total Winnings')
    }else if(input$plottype8 == 'Average daily Stake by Customer Level'){
      #Average Daily Stakes
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyStake_Product8)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyStake = mean(AverageDailyStake_Product8))%>% arrange(AverageDailyStake)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyStake))+
        geom_col()+
        ggtitle('Average Daily Stakes by Customer level for Casino Chartwell')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Stakes')
      
    }else if(input$plottype8 == 'Life time value by Customer Level'){
      #Average Total Cost per customer level
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(LifetimeValue_Product8)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(LifetimeValue_Product8))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level for Casino Chartwell')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if(input$plottype8 == 'Average daily winnings by Customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyWinning_Product8)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyWinnings = mean(AverageDailyWinning_Product8))%>% arrange(AverageDailyWinnings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyWinnings))+
        geom_col()+
        ggtitle('Average Daily Winnings by Customer level for Casino Chartwell')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Winnings')
      
    }else {
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(AverageDailyBets_Product8)==0) %>% group_by(customer_level)%>%
        summarise(AverageDailyBettings = mean(AverageDailyBets_Product8))%>% arrange(AverageDailyBettings)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageDailyBettings))+
        geom_col()+
        ggtitle('Average Daily bettings by Customer level for Casino Chartwell')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Daily Bettings')
    }
  })  
  output$customerText <- renderText({
    paste("Customers are assigned the levels based on the RFM model. The levels are Platinum, Gold, Silver, Bronze")
  })
  output$customerLevel <- renderPlot({
    
    if(input$customerType == 'Pie'){
      pie(sort(table(datamart$customer_level)))
      
    } else {
      ggplot(datamart %>% filter(is.na(customer_level)==0),aes(x = customer_level, fill = customer_level ))+
        geom_bar()+
        ggtitle("Customer Levels")
      
    }
    
  })
  output$costVSrevenue <- renderPlot({
    
    #Total Revenue  vs Total costs per user
    ggplot(datamart, aes(x = totalrevenue, y = totalcost, color = Gender), size = 10)+
      geom_point()+
      ggtitle('Total Revenue  vs Total costs per user')+
      scale_x_continuous(limits = c(0, 1150000))+
      scale_y_continuous(limits = c(0, 1100000))
    
  })
  output$aggregationsPlot <- renderPlot({
    
    if (input$aggregations == 'Overview of stakes, winnings and Bets'){
      #Total stakes per products
      x <- datamart %>% pivot_longer(cols = c(TotalStakes_Product1, TotalStakes_Product2,TotalStakes_Product4,
                                              TotalStakes_Product5, TotalStakes_Product6, TotalStakes_Product7, 
                                              TotalStakes_Product8, ), names_to = "var", values_to = "val")
      x$val[is.na(x$val)] <- 0
      x <- x %>% group_by(var) %>% summarise(s = sum(val)) %>% 
        replace(list = 'Number', values = c(1,2,3,4,5,6,7))
      
      
      
      #Total Bets per products
      y <- datamart %>% pivot_longer(cols = c(TotalBets_Product1, TotalBets_Product2,TotalBets_Product4,
                                              TotalBets_Product5, TotalBets_Product6, TotalBets_Product7, 
                                              TotalBets_Product8, ), names_to = "var", values_to = "val")
      
      y$val[is.na(y$val)] <- 0
      y <- y %>% group_by(var) %>% summarise(s = sum(val)) %>% 
        replace(list = 'Number', values = c(1,2,3,4,5,6,7))
      
      
      
      #Total Winnings per products
      z <- datamart %>% pivot_longer(cols = c(TotalWinnings_Product1, TotalWinnings_Product2,TotalWinnings_Product4,
                                              TotalWinnings_Product5, TotalWinnings_Product6, TotalWinnings_Product7, 
                                              TotalWinnings_Product8, ), names_to = "var", values_to = "val")
      
      z$val[is.na(z$val)] <- 0
      z <- z %>% group_by(var) %>% summarise(s = sum(val)) %>% 
        replace(list = 'Number', values = c(1,2,3,4,5,6,7))
      
      
      
      #Plot for aggregations based on products
      ggplot()+
        geom_line(data = x, aes(x= Number, y = s, color = 'green'), size = 1.5)+
        geom_line(data = y, aes(x= Number, y = s, color = 'blue'),size = 1.5)+
        geom_line(data = z, aes(x= Number, y = s, color = 'red'),size = 1.5)+
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7), name = 'Products', labels = c('Sports book fixed-odd',
                                                                                    'Sports book live-action',
                                                                                    'Casino BossMedia',
                                                                                    'Supertoto',
                                                                                    'Games VS',
                                                                                    'Games bwin',
                                                                                    'Casino Chartwell'))+
        scale_y_continuous(breaks = c(0,10000000,20000000,30000000), name = 'Monetary Value')+
        theme(axis.text.x=element_text(angle=90, hjust=1))+
        scale_fill_identity(guide = 'legend')+
        scale_colour_manual(name = 'Line', values =c('green'='green','blue'='blue', 'red'='red'), 
                            labels = c('Total Stakes','Total Bets', 'Total Winnings'))
    }else if (input$aggregations  == 'Life time value per customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(LifetimeValue_Product8)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(lifetimevalue))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if (input$aggregations == 'Average frequency of play by customer level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(totalrevenue)==0) %>% group_by(customer_level)%>%
        summarise(Averagefrequency = mean(poker_frequency))%>% arrange(Averagefrequency)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= Averagefrequency))+
        geom_col()+
        ggtitle('Average frequency of play by customer level')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average frequency')
    }else if (input$aggregations == 'Total cost V/s Total Revenue'){
      ggplot(datamart, aes(x = totalrevenue, y = totalcost, color = Gender), size = 10)+
        geom_point()+
        ggtitle('Total Revenue  vs Total costs per user')+
        scale_x_continuous(limits = c(0, 1150000))+
        scale_y_continuous(limits = c(0, 1100000))
    }else {
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(profitmargin)==0) %>% group_by(customer_level)%>%
        summarise(Averageprofit = mean(profitmargin))%>% arrange(Averageprofit)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= Averageprofit))+
        geom_line()+
        ggtitle('Average profit by customer level')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average profit')
    }
  })
  output$plotPoker <- renderPlot({
    if(input$pokerPlot == 'Poker buy vs Poker Sell'){
      ggplot(datamart, aes(x = totalpoker_buy, y = totalpoker_sell, color = Gender), size = 10)+
        geom_point()+
        ggtitle('Total Poker buy vs Total poker sell')
    }else if(input$pokerPlot == 'Life time value by customer Level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(poker_ltv)==0) %>% group_by(customer_level)%>%
        summarise(AverageLifetimevalue = mean(poker_ltv))%>% arrange(AverageLifetimevalue)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= AverageLifetimevalue))+
        geom_col()+
        ggtitle('Average life time value by Customer level for poker')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average Life time value')
    }else if(input$pokerPlot == 'Average frequency of play by customer level'){
      datamart %>% filter(is.na(customer_level)==0, 
                          is.na(poker_frequency)==0) %>% group_by(customer_level)%>%
        summarise(Averagefrequency = mean(poker_frequency))%>% arrange(Averagefrequency)%>%
        replace(list = 'Number', values = c(1,2,3,4))%>%
        ggplot(aes(x= Number, y= Averagefrequency))+
        geom_col()+
        ggtitle('Average frequency of play by customer level for poker game')+
        scale_x_continuous(name = 'Customer Level',breaks = c(1,2,3,4), labels = c('Bronze', 'Silver', 'Gold', 'Platinum'))+
        scale_y_continuous(name = 'Average frequency')
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)