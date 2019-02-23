install.packages("shiny")
install.packages("DT")
install.packages("ggplot2")
install.packages("shinythemes")
install.packages("dplyr")
install.packages("rsconnect")

library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(rsconnect)


mydata = read.csv("/Users/zehrasubas/desktop/r/FinalProjectProd/hfi_cc_2018.csv", header = TRUE, sep = ",", na = "0")
mycolumns <- c("year", "region", "countries","pf_rol_civil","pf_rol_criminal", "pf_rol","ef_trade_movement_foreign", "ef_trade_movement_visit","ef_trade")
mydata <- mydata[mycolumns]
mycolumnNames <- c( "Year", "Region", "Country", "Civil Justice", "Criminal Justice", "Rule of Law", "Foreign ownership/investment restrictions", "Freedom of Foreigners to Visit", "Freedom to trade internationally","Weighted Data")

attach(mydata)

server = function(input, output, session){
  
  # table for the Data table tab
  

  
  output$tableDT <- DT::renderDataTable(DT::datatable(mydata)) 
  
  weighted.mydata = reactive(
    cbind(mydata[which(mydata$year==input$year),] , points= (input$w1 * mydata[which(mydata$year==input$year),]$pf_rol_civil + input$w2 * mydata[which(mydata$year==input$year),]$pf_rol_criminal + input$w3 * mydata[which(mydata$year==input$year),]
                                                             $pf_rol  ))
  )
  
  
  output$justice_chart = renderPlot({
    #  myreacdata <- mydata[which(mydata$year==input$year),]
    # myreacdata <- cbind(myreacdata,points = input$w1 * myreacdata$pf_rol_civil + input$w2 * myreacdata$pf_rol_criminal + input$w3 * myreacdata$pf_rol)
    data <- weighted.mydata()
    ggplot(data, aes(region,points, color=region)) +
      geom_point() + geom_smooth(method = "lm") +
      xlab("Countries") + ylab("Justice ranks") +
      ggtitle("Civil-Criminal Justice & Rule of law (select points to see more detail in the table)")
      
  })
  

  output$trade_chart1 = renderPlot({
    myreacdata <- mydata[which(mydata$year==input$year2),]
    ggplot(myreacdata, aes(x=ef_trade, fill=region)) +
      geom_area(stat ="bin", alpha=0.6) +
      xlab("Trade ranks (1 to 10)")+
      ylab("Count") +
      ggtitle("Freedom to Trade Internationally")
    
  })
  
  
  output$trade_chart2 = renderPlot({
    myreacdata <- mydata[which(mydata$year==input$year2),]
    ggplot(myreacdata, aes(x=region, y=ef_trade_movement_visit)) +
      geom_jitter(alpha=0.6, aes(color = region)) +
      xlab("Regions") +
      ylab("Visit fredoom ranks (1 to 10)")+
      ggtitle("Freedom of foreigners to visit")
    
  })
  
  output$trade_chart3 = renderPlot({
    myreacdata <- mydata[which(mydata$year==input$year2),]
    ggplot(myreacdata, aes(x=region, y=ef_trade_movement_foreign, fill=region)) +
      geom_bar(stat="identity") +
      xlab("Regions") +
      ylab("Restriction ranks (1 to 10)")+
      ggtitle("Foreign ownership/investment restrictions")
    
  })
  
  
  
#    theme(axis.text.x=element_blank(),  axis.ticks.x=element_blank())
  
  mydata.new = reactive({
    user_brush <- input$user_brush
    mysel <- brushedPoints(na.omit(weighted.mydata()), user_brush)
    return(mysel)
  })
  
  
  output$table = DT::renderDataTable(DT::datatable(mydata.new(), colnames=mycolumnNames, rownames=NULL))
  
  output$mydownload = downloadHandler(
    filename = "Selected-Data.csv",
    content = function(file) {
      data <- mydata.new()
      names(data) <- mycolumnNames
      write.csv(data, file)})
  
  
}

ui = navbarPage(theme = shinytheme("sandstone"), title = h3("Human Freedom Around the World"),
                tabPanel(
                  ("Justice Index"),
                  wellPanel(
                    sliderInput(inputId = "w1",
                                label = "Civil Justice",
                                value = 7, min = 0, max = 10),
                    sliderInput(inputId = "w2",
                                label = "Criminal Justice",
                                value = 2, min = 0, max = 10),
                    sliderInput(inputId = "w3",
                                label = "Rule of Law",
                                value = 0.6, min = 0, max = 10),
                    selectInput("year", "Year:",
                                unique(c(mydata$year)))
                    
                  ),
                  plotOutput("justice_chart", brush = "user_brush"),
                  DT::dataTableOutput("table"),
                  downloadButton(outputId = "mydownload", label = "Download Table")
                ),
                
                tabPanel("Foreign Trade Index",
                         wellPanel(selectInput("year2", "Year:",
                                               unique(c(mydata$year)))
                         ),
                         plotOutput("trade_chart1"),
                         plotOutput("trade_chart2"),
                         plotOutput("trade_chart3")
                ), 
                
                
                tabPanel("Documentation",
                         tags$div(
                           HTML(" <html>
                                <head>
                                <style type='text/css'>
                                .mainTitle{
                                color:#a64d79;
                                font-weight:400;
                                font-size:16pt;
                                font-family:'Calibri';
                                }
                                .subTitle{
                                color:#6aa84f;
                                font-family:'Calibri';
                                font-size:14pt;
                                margin-top:10px;
                                }
                                .margins{
                                margin-right: 15%;
                                margin-left: 15%;
                                margin-bottom: 10%;
                                margin-top: 3%;
                                }
                                </style>
                                </head>
                                <body>
                                <div class='margins'>
                                <div class='mainTitle'>JUSTICE INDEX EXPLANATION</div>
                                <div class='subTitle'>Widgets</div>
                                <div>
                                There are 4 different widgets defined for justice
                                index explanation widgets. The first one is
                                Civil Justice, it’s a scale 1 to 10 to and used to change
                                point’s position based on civil justice ranks. The second
                                one is Criminal Justice, it’s a scale 1 to 10 to and used to
                                change point’s position based on criminal justice ranks.
                                The third one is Rule of Law, it’s a scale 1 to 10 to and
                                used to change point’s position based on rule of law ranks.
                                When the slider is changed for each of them,
                                the points value will be calculated again.
                                Calculation of the Points data is all these three data set are multiplied by slider
                                values (whichever field is matching). Points are used for y-axis of the dot chart. The last widget is year
                                widget. It filters the data set based on the selection.
                                By default it brings data belongs to 2016.
                                </div>
                                <div class='subTitle'>Civil - Criminal Justice and Rule of Law Chart</div>
                                <div>
                                This chart is a ggplot geom_point. Its x-axis created by using region
                                and its y-axis is created by using points data (this data type is explained above).
                                The chart is colored based on regions. Each country has a color based on its region.
                                Moreover, this char is interactive with the data table below.
                                The user needs to select the points they want to see detailed data for.
                                Data will appear on data table interactively.
                                </div>
                                <div class='subTitle'>Data Table</div>
                                <div>
                                Data table works interactively with the data range that’s selected on the point chart.
                                It’s also filtered by year widget.
                                It has raw data that has been used for developing these charts and it has a download option.
                                </div>
                                <br>
                                <div class='mainTitle'>FOREIGN TRADE INDEX EXPLANATION</div>
                                <div class='subTitle'>Widgets</div>
                                <div>
                                There’s only year widget is used on this page. When the user selects the year,
                                it filters the data based on the selected year. This filtered data is used to create the charts below.
                                </div>
                                <div class='subTitle'>Freedom to Trade Internationally Chart</div>
                                <div>
                                This chart is a ggplot geom_area. The chart’s x-axis is created based on international
                                trade ranks and y-axis is automatically
                                created by R because of used chart type. It’s filled and colored based on the regions.
                                </div>
                                <div class='subTitle'>Fredom of Foreigners to Visit Chart</div>
                                <div>
                                This chart is a ggplot geom_jitter. The chart’s x-axis is created
                                based on regions and y-axis is created based on freedom of foreigners to visit ranks.
                                It’s filled and colored based on the regions.
                                </div>
                                <div class='subTitle'>Foreign Ownership/Investment Restrictions Chart</div>
                                <div>
                                This chart is a ggplot geom_bar.
                                The chart’s x-axis is created based on regions and y-axis
                                is created based on ownership and investment restrictions
                                for foreigners ranks. It’s filled and colored based on the regions.
                                </div>
                                </div>
                                </body>
                                </html>
                                ")
                         )
                )
                
                
)

shinyApp(ui = ui, server = server)
