library(shiny)
library(shinyIncubator)
library(rCharts)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title="Opinion Mining"),
  dashboardSidebar(
      textInput("entity1", "Handle 1: ","dhoni"),
      textInput ("entity2","Handle 2: ","virat"),
      sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=500,max=2500,value=800,step=100),
      #actionButton(inputId='go',icon =icon("twitter"), label="Hit it!"),
      hr(),
    sidebarMenu(
      menuItem("Sentiment Analysis", tabName = "dashboard", icon = icon("signal")),
      menuItem("Word Clouds", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("Score", tabName = "score", icon = icon("dashboard")),
      menuItem("Handle 1 Tweets", tabName = "handle1t", icon = icon("th")),
      menuItem("Handle 2 Tweets", tabName = "handle2t", icon = icon("th"))
    )
    ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              HTML("<div style='color:#fff; font-size: 25px;text-align: center; background-color:#1E282C; box-shadow: 0 12px 6px -6px #777;border-radius: 10px;padding: 9px 15px ; margin-bottom:8px;' >No of tweets By Hour </div>"),
              chartOutput("plot2", "highcharts"),
              HTML("<div style='color:#fff; font-size: 25px;text-align: center; background-color:#1E282C; box-shadow: 0 12px 6px -6px #777;border-radius: 10px;padding: 9px 15px ; margin-bottom:8px;' >Score By Hour </div>"),
              chartOutput("plot1", "highcharts")
      ),
      
      # Second tab content
      tabItem(tabName = "wordcloud",
              HTML("<div style='color:#fff; font-size: 25px;text-align: center; background-color:#1E282C; box-shadow: 0 12px 6px -6px #777;border-radius: 10px;padding: 9px 15px ; margin-bottom:8px;' >WordCloud for handle 1 </div>"),
              plotOutput("entity1wcplot"),
              HTML("<div style='color:#fff; font-size: 25px;text-align: center; background-color:#1E282C; box-shadow: 0 12px 6px -6px #777;border-radius: 10px;padding: 9px 15px ; margin-bottom:8px;' >WordCloud for handle 2 </div>"),
              plotOutput("entity2wcplot")
              
      ),
      tabItem(tabName = "score",
              HTML("<div style='color:#fff; font-size: 25px;text-align: center; background-color:#1E282C; box-shadow: 0 12px 6px -6px #777;border-radius: 10px;padding: 9px 15px ; margin-bottom:8px;' >Score for handle 1</div>"),
              chartOutput("plot3", "highcharts"),
              HTML("<div style='color:#fff; font-size: 25px;text-align: center; background-color:#1E282C; box-shadow: 0 12px 6px -6px #777;border-radius: 10px;padding: 9px 15px ; margin-bottom:8px;' >Score for handle 2</div>"),
              chartOutput("plot4","highcharts")
              
      ),
      tabItem(tabName = "handle1t",
             tableOutput("tableentity1")
              
      ),
      tabItem(tabName = "handle2t",
              tableOutput("tableentity2")
              
      )
    )
  )
)