library(shiny)
library(munsell)
library(MASS)
# library(ggforce)
library(ggplot2)
library(ggbeeswarm)
library(dplyr)
library(ggiraph)
library(stringr)
library(shinyBS)
library(shinyjqui)
# library(shinyjs)


ui <- fixedPage(
  tags$head(
    tags$style(HTML("
    
      .regression-table {
      border-collapse: collapse;
      }
                    
      .regression-table td, .regression-table th {
      border: 1px solid #ddd;
      padding: 6px;
      }
                    
      .regression-table th {
      padding-top: 6px;
      padding-bottom: 6px;
      text-align: left;
      background-color: #808080;
      color: white;
      }

      .regression-table tr {
      padding-top: 6px;
      background-color: black;
      color: white
      }
      
      .navigation-table table {
      border-collapse: collapse;
      }
      
      .navigation-table tr {
      height: 15px;
      }
      
      .navigation-table td {
      width: 15px;
      }
      
    table.WhichRegToRun-table td {
      border: 1px solid #ccc;
      padding: 5px;
    }
    table.WhichRegToRun-table th {
      border: 1px solid #ccc;
      padding: 5px;
    }
    
    table.WhichRegToRun-table th.centered {
      text-align: center;
    }
    
    table.WhichRegToRun-table th.no-border {
      border: none;
    }
    
    table.WhichRegToRun-table {
      border-collapse: collapse;
      padding: 10px;
    }

      
      
      
      .navigation-btn {
      opacity: 0.75;
      width: 20px !important;
      height: 20px !important;
      padding: 0 !important;
      display: flex;
      align-items: center;
      justify-content: center;
      }
      
      .same-row {
      display: inline-block;
      vertical-align: middle;
      margin-left: 10px;
      }
      
      .edge-label {
        cursor: pointer;
        # fill: black;
        font-size: 16px;
      }
      
    #infoText {
      cursor: pointer;
      color: #337ab7;
    }
    #infoText:hover {
      color: #23527c;
    }

      

    ")),
 
    # console.log('Clicked edge:', edgeId);
    
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
    var infoText = document.getElementById('infoText');
    if (infoText) {
      infoText.addEventListener('click', function() {
        Shiny.setInputValue('text_clicked', true, {priority: 'event'});
      });
    }
  });
  ")),
  tags$script(HTML("
      $(document).on('click', '.edge-label', function() {
        const edgeId = $(this).data('edge-id');
        Shiny.setInputValue('clicked_edge', edgeId, {priority: 'event'});
      });
    "))
  ),   
  # Application title
    fixedRow(
      column(width=10, 
             titlePanel("Causality and Regression"),
             HTML("See the <span id='infoText'>introduction</span> explaining how to use this app."),
      ),
      column(width=2,
             tags$img(id = "nsf_logo",
                      src = "images/NSF.png",
                      height = "80")
             ),
      bsPopover(
        id = "nsf_logo",
        title= "NSF",
        trigger = "hover",
        content="This material is based upon work supported by the National Science Foundation under Grant Number 1651330. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation."
      )
    ),

  hr(),
  
  fixedRow(
    
    column(width=4,
           wellPanel(
             selectInput("N", "Number of Observations", choices=c(10,20,50,100,500,1000), selected=100),
             selectInput("RoleOfZ", "Causal Structure", choices=c('Alternative Cause', 'Confound / Common Cause', 'Alternative Effect', 'Mediator / Mechanism', 'Interaction / Moderator', 'Common Effect')),
             htmlOutput("CSGraph")
           ),
           
           
           
           wellPanel(
             tags$details(
               tags$summary(HTML("<b>Additional Options</b>"), style = "cursor: pointer;"),
               tags$p(
                 selectInput("DataGeneration", "Data Generation", choices=c("Perfect" = TRUE, "Approximate" = FALSE), selected="Perfect"),
                 selectInput("Domain", "Names of Variables", choices=c('Smoking - Heart Disease', 'Perseverence - LSAT', 'Anxiety - Depression')),
                 checkboxInput('ChooseNames', 'Edit the variable names'),
                 conditionalPanel(
                   condition = "input.ChooseNames == true",
                   
                   textInput("XLabel", "Name of X", value = "Smoking", width = NULL),
                   textInput("YLabel", "Name of Y", value = "Heart Disease", width = NULL, placeholder = NULL),
                   textInput("ZLabel", "Name of Z", value = "Diet", width = NULL, placeholder = NULL),
                   textInput("ZLabelZ0", HTML('<span style="color:red">Name of Group Z0</span>'), value = "Healthy Diet", width = NULL, placeholder = NULL),
                   textInput("ZLabelZ1", HTML('<span style="color:orange">Name of Group Z1</span>'), value = "Unhealthy Diet", width = NULL, placeholder = NULL),
                   actionButton("SubmitNames", "Submit Names")
                 ),
                 actionButton("ResampleData", "Resample Data"),
                 conditionalPanel(
                   condition = '"a" == "b"',
                   numericInput("hiddeninput", "hiddeninput", 10, min = 1, max = 1000000)
                 )
               )
             )
           )
    ),
    
    
    # Show a plot of the generated distribution
    column(width=8,
           wellPanel(
             selectInput( 
               "WATR", #'Which Analysis to Run" 
               "Select which regression you want to run:", 
               choices = c("select one" = "","[ X~Z ]","[ Y~Z ]","[ Y~X ]","[ Y~X+Z ]","[ Y~X×Z ]")
             ),
             conditionalPanel(condition = "input.WATR == '[ X~Z ]'", uiOutput("simple_relation_Z_X_HTML")),
             conditionalPanel(condition = "input.WATR == '[ Y~Z ]'", uiOutput("simple_relation_Z_Y_HTML")),
             conditionalPanel(condition = "input.WATR == '[ Y~X ]'", uiOutput("current_regression_unconditional_HTML")),
             conditionalPanel(condition = "input.WATR == '[ Y~X+Z ]'", uiOutput("current_regression_conditional_HTML")),
             conditionalPanel(condition = "input.WATR == '[ Y~X×Z ]'", uiOutput("current_regression_interaction_HTML"))
           ),
           hr(),
           
           
           conditionalPanel(
             condition = "input.WATR == '[ X~Z ]'",
             tags$table(
               style = "border-collapse: collapse; table-layout: fixed; width: 650px; border: none;",  # fixed table width = sum of column widths
               tags$tr(
                 tags$td(
                   style = "padding: 0; width: 500px; border: none;",
                   div(style = "width:500px; height:175px; overflow: hidden; position: relative; margin: 0 0 0 0;",
                       div(style = "position: absolute; top: -325px; margin: 0 0 0 0;",
                             plotOutput("MarginalXPlot", width = "500px", height="500px")
                       )
                   )
                 ),
                 tags$td(
                   style = "padding: 0; width: 175px; border: none; background-color: transparent !important;",
                 )
               )
               )
           ),
             
           
           tags$div(style = "position: relative;",
           tags$table(
             style = "border-collapse: collapse; table-layout: fixed; width: 650px; border: none;",  # fixed table width = sum of column widths
             
             # Row 1

             
             # Row 2
             tags$tr(
               style = "height: 500px;",  # fixed height for bottom row
               tags$td(
                 style = "padding: 0; width: 500px; border: none;",
                 div(style = "width:500px; height:500px; margin: 0 0 0 0; z-index: 1; position: relative",
                     girafeOutput("distPlot"),
                     tags$div(style="position: absolute; bottom: 45px; right: 5px; z-index: 10;",
                              tags$table(class = "navigation-table",
                                         tags$tr(
                                           tags$td(),
                                           tags$td(actionButton(class = "navigation-btn", inputId = "Up", label = icon("arrow-up"), style = "padding: 10; cursor: pointer;")),
                                           tags$td()
                                         ),
                                         tags$tr(
                                           tags$td(actionButton(class = "navigation-btn", inputId = "Left", label = icon("arrow-left"), style = "padding: 10; cursor: pointer;" )),
                                           tags$td(actionButton(class = "navigation-btn", inputId = "Center", label = icon("location-crosshairs"), style = "padding: 10; cursor: pointer;")),
                                           tags$td(actionButton(class = "navigation-btn", inputId = "Right", label = icon("arrow-right"), style = "padding: 10; cursor: pointer;"))
                                         ),
                                         tags$tr(
                                           tags$td(),
                                           tags$td(actionButton(class = "navigation-btn", inputId = "Down", label = icon("arrow-down"), style = "padding: 10; cursor: pointer;")),
                                           tags$td()
                                         )
                                         )
                              # tags$td(actionButton(inputId = "Down", label = icon("arrow-down"), style = "position: absolute; bottom: 0px; left: 50%; transform: translateX(-50%); padding: 10; cursor: pointer;")),
                              )
                     )
                 ),
               tags$td(
                 style = "padding: 0; width: 175px; border: none;",
                 div(style = "width:175px; height:500px; overflow: hidden; position: relative; margin: 0 0 0 0;",
                     div(style = "position: absolute; right: -325px; margin: 0 0 0 0;",
                         conditionalPanel(
                           "input.WATR == '[ Y~Z ]'",
                           plotOutput("MarginalYPlot", width = "500px", height="500px"))
                         )#it generates the plot this size initially, and then crops it in the div above
                     )
                 )
               )
             )
           ),
           uiOutput("KeyText")
    )
  ),
  hr(),
  fixedRow(
    column(
      width=12,
      wellPanel(
        tags$details(
          tags$summary(HTML("<b>How to decide which regression to run</b>"), style = "cursor: pointer;"),
          tags$p(HTML("The main question addressed in this app is when you should or should not control for a third variable Z. Stated another way, which regression should you run?
          <ul>
          <li>Y ~ X : this does not control for Z</li>
          <li>Y ~ X + Z : this does control for Z</li>
          </ul>")),
          tags$p(HTML("<b>Rule 1:</b> control for Z if it is plausibly a direct cause of Y (alternative cause, confound). This table explains why:")),

          tags$p(
            tags$table(
              class = "WhichRegToRun-table",
              tags$tr(
                tags$th(class = "no-border", ""),
                tags$th(colspan = 2, class="centered", "Why")
              ),
              tags$tr(
                tags$th(class = "no-border", ""),
                tags$th(class = "centered", "Because of the regression weight b"),
                tags$th(class = "centered", "Because of significance p and power")
              ),
              tags$tr(
                tags$th("When to control for Z"),
                tags$td(HTML("If you suspect that Z is a <b>confound</b>, you should control for it, because if you don’t, and it really is a confound, you will get the wrong regression weight. For example, you might conclude that the regression weight is positive when in reality it is negative or zero, or you might conclude it is negative when in reality it is positive, etc.")),
                tags$td(HTML("If you suspect that Z is an <b>alternative cause</b>, you should control for it, because if it really is an alternative cause, controlling increases your power to detect an effect (a lower p-value)."))
              ),
              tags$tr(
                tags$th(HTML("When <b>not</b> to control for Z")),
                tags$td(HTML("If you suspect that Z is a <b>common effect</b>, you should <b>not</b> control for it, because if you do, and it is a common effect, you will get the wrong regression weight.")),
                tags$td(HTML("If you suspect that Z is an <b>alternative effect</b>, you should <b>not</b> control for it, because if you do, and it really is an alternative effect, controlling decreases your power to detect an effect (a higher p-value). NB: Though it is not simulated here, the same is true if the true causal structure is X➝Y➝Z."))
              )
            )
          ),
          tags$p(HTML("<b>Rule 2 about mediators / mechanisms:</b> If you think that Z is plausibly a mediator, then controlling for it tells you whether X is still directly predictive of Y aside from any path through Z. If instead you run [Y ~ X], it will only tell you if X predicts Y, but it could be that X only predicts Y through Z. NB: Testing whether Z actually is a mediator involves, essentially, comparing [ Y ~ X + Z ] and [ Y ~ X ], to see if when controling for Z, the predictiveness of X gets weaker, compared to when not controlling for Z. However, there are more sophisticated analyses than just running these regressions for mediation analysis.")),
          tags$p(HTML("<b>Rule 3 about moderation / interaction:</b> If you want to test for moderation / interaction, which means testing whether the slopes of the two groups are different from each other, then you need to run [Y ~ X x Z]. If you run [Y ~ X + Z], the computer will not tell you if there is a significant interaction or not. That said, if you do not think it is plausible that there is an interaction or are not interested in it, you should not by default always test for an interaction, because doing so will decrease your power to find a significant effect of X."))
          
          
          
        )
      

        
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  values <- reactiveValues(
    N_SlopeXonY = 1,
    N_SlopeZonY = 1,
    CC_SlopeXonY = 1,
    CC_SlopeZonX = 3,
    CC_SlopeZonY = 3,
    AE_SlopeXonY = 1,
    AE_SlopeXonZ = 1,
    M_SlopeXonY = 1,
    M_SlopeXonZ = 1,
    M_SlopeZonY = 3,
    CE_SlopeXonY = 0,
    CE_SlopeXonZ = 3,
    CE_SlopeYonZ = 3,
    I_SlopeXonY = -1,
    I_SlopeZonY = 0,
    I_Interaction = 2,
    NumXY = 0,
    NumZY = 0,
    NumXZ = 0,
    Num.Interaction = 0,
    Num.Intercept = 0,
    ColorXY = "white",
    ColorZY = "white",
    ColorXZ = "white",
    ColorInteraction = "white",
    OP.XY = 0,
    OP.ZY = 0,
    OP.YZ = 0,
    OP.ZX = 0,
    OP.XZ = 0,
    OP.Int = 0
  )
  
  clickHandlersRegistered <- reactiveVal(FALSE)
  




  
  XNAME<-   eventReactive(c(input$SubmitNames, input$hiddeninput), {input$XLabel}, ignoreNULL = FALSE)
  YNAME<-   eventReactive(c(input$SubmitNames, input$hiddeninput), {input$YLabel}, ignoreNULL = FALSE)
  ZNAME<-   eventReactive(c(input$SubmitNames, input$hiddeninput), {input$ZLabel}, ignoreNULL = FALSE)
  ZNAMEZ0<- eventReactive(c(input$SubmitNames, input$hiddeninput), {input$ZLabelZ0}, ignoreNULL = FALSE)
  ZNAMEZ1<- eventReactive(c(input$SubmitNames, input$hiddeninput), {input$ZLabelZ1}, ignoreNULL = FALSE)
  
  observe({
    r <- input$RoleOfZ
    s <- input$Domain
    updateNumericInput(session, "hiddeninput", value = runif(1, 0, 1000000))
  })
  
  observeEvent(input$text_clicked, {
    showModal(modalDialog(
      title = "Introduction and How to Use this App",
      HTML("This app is designed to help understand the relationships between causality and regression, and decide which regression to run based on which causal structures you think are plausible. Normally as a researcher you don't know the causal structure for certain. In this app, you get to play the role of nature/'God', and select the true causal structure. Then, you get to see what the data look like when plotted and the output of different regressions. <br><br>
      In the left hand column you get to decide the role of the third variable called Z which determines the causal structure. Z is a binary (two group) variable. The two groups are referred as Z0 and Z1. <br><br>
      When you select a different role for Z, you will see a different causal structure. You also get to decide the strengths of the causal relations - if you click on the numbers in the causal structure you can change the strengths. You want to be able to uncover these true strengths by running the right regression.<br><br>
      In the right hand column, when you choose to run a regression, the regression table is displayed, and the scatterplot is modified to show you what the regression means. <br><br>
      At the bottom of the screen you can get help about deciding which regression to run.<br><br>
      Pay attention to colors. The colors of the variables in the regression table, the different components in the scatterplot, and the causal diagram line up. When you pick the right regression to run, you will see the colors and numbers match.<br><br>
      <b>More Info:</b> <a href='https://canvas.pitt.edu/courses/124970' target='_blank'> osRMss </a> teaches these rules in more detail. I have a particular, exercise that shows how to see each of these rules play out in this app. You can access the exercise in multiple formats: <a href='https://pitt.h5p.com/content/1292296889820880358' target='_blank'>H5P</a>, <a href='https://canvas.pitt.edu/courses/124970/pages/1-dot-07-causality-and-regression?module_item_id=3037852' target='_blank'>Videos and MSWord</a>.<br><br>
      <b>Contributions:</b> This app was made by <a href='http://www.lrdc.pitt.edu/rottman/' target='_blank'>Ben Rottman</a>. View source, report issues, or suggest changes on <a href='https://github.com/BenRottman/Causality_and_Regression_shinylive/' target='_blank'>GitHub</a>, or contact me directly."),
      easyClose = TRUE
    ))
  })
  

  observe({
    if (input$RoleOfZ == 'Alternative Cause'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Diet", "Healthy Diet", "Unhealthy Diet")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Sleep", "More Sleep", "Less Sleep")
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelZ0", value = L[4])
      updateTextInput(session, "ZLabelZ1", value = L[5])
      
      
    } else if (input$RoleOfZ == 'Confound / Common Cause'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Stress", "Not Stressed", "Stressed")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Stress", "More Stress", "Less Stress")
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelZ0", value = L[4])
      updateTextInput(session, "ZLabelZ1", value = L[5])
      
    } else if (input$RoleOfZ == 'Mediator / Mechanism'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Endothelium Damage", "Endothelium Not Damaged", "Endothelium Damaged")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Time Studying", "Little Studying", "Lots of Studying")
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelZ0", value = L[4])
      updateTextInput(session, "ZLabelZ1", value = L[5])
      
    } else if (input$RoleOfZ == 'Alternative Effect'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Teeth Color", "Normal Teeth", "Yellow Teeth")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Excercise Frequency", "Infrequent", "Frequent") #stress could affect perseverance through procrastination
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelZ0", value = L[4])
      updateTextInput(session, "ZLabelZ1", value = L[5])
      
    } else if (input$RoleOfZ == 'Common Effect'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Death", "Normal Death", "Early Death")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Law School Admission", "Not Admitted", "Admitted") #stress could affect perseverance through procrastination
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelZ0", value = L[4])
      updateTextInput(session, "ZLabelZ1", value = L[5])
      
    } else if (input$RoleOfZ == 'Interaction / Moderator'){
      if(input$Domain == 'Smoking - Heart Disease'){
        L <- c("Smoking", "Heart Disease", "Activity", "Active", "Sedentary")
      } else if (input$Domain == 'Perseverence - LSAT'){
        L <- c("Perseverence", "LSAT Score", "Practice Tests", "Do Not Take Practice Tests", "Take Practice Tests")
      } else if (input$Domain == 'Anxiety - Depression'){
        L <- c("Anxiety", "Depression", "Insomnia", "Sleep Well", "Sleep Poorly")
      }
      
      updateTextInput(session, "XLabel", value = L[1])
      updateTextInput(session, "YLabel", value = L[2])
      updateTextInput(session, "ZLabel", value = L[3])
      updateTextInput(session, "ZLabelZ0", value = L[4])
      updateTextInput(session, "ZLabelZ1", value = L[5])
      
    }
  })
  
  
  currentDF <- reactive({
    print("currentDF reactive triggered")
    input$ResampleData
    N <- as.integer(input$N)

    
    if(input$RoleOfZ == 'Alternative Cause'){
      SlopeXonY<-values$NumXY
      SlopeZonY<-values$NumZY
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical= input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      NoiseX<-c(d[,1],d[,2]) +3 #+3 makes the min of the noise be around 0 since with sd=1, this is 3 sd below the mean
      NoiseY<-c(d[,3],d[,4]) +0
      
      
      # Z<-c(rep(-.5,N/2),rep(.5,N/2))*VarNoiseZ
      Z<-c(rep(0,N/2),rep(1,N/2))*VarNoiseZ
      X<-NoiseX
      Y<-SlopeXonY*X + SlopeZonY*Z + NoiseY + values$Num.Intercept
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 0"
      Z[Z==TRUE]<-"Group 1"
    }
    
    if(input$RoleOfZ == 'Confound / Common Cause'){
      SlopeXonY<-values$NumXY
      SlopeZonX<-values$NumXZ
      SlopeZonY<-values$NumZY
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical=input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      NoiseX<-c(d[,1],d[,2]) +3 #+3 makes the min of the noise be around 0 since with sd=1, this is 3 sd below the mean
      NoiseY<-c(d[,3],d[,4]) +0
      
      # Z<-c(rep(-.5,N/2),rep(.5,N/2))*VarNoiseZ
      Z<-c(rep(0,N/2),rep(1,N/2))*VarNoiseZ
      X<-SlopeZonX*Z + NoiseX
      Y<-SlopeXonY*X + SlopeZonY*Z + NoiseY + values$Num.Intercept
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 0"
      Z[Z==TRUE]<-"Group 1"
    }
    
    #I used the same model to generate the mechanism as the confound, even though technically it is not generated from causes to effects
    #The reason is that these two models are markov equivalent so the data should work either way 
    #Also, it is confusing to generate a binary variable (Z) when it is not exogenous - would have to designate a cutoff, which essentially adds noise into the system and messes with the parameters
    #Possibley come back to this and instead of having a binary variable as 1/0, actually make it a multiple of X, so it could be something like +2-/2 if X->Z link is 2, or +3/-3 if X->Z link is 3. I think this would fix the mediation problem.
    if(input$RoleOfZ == 'Mediator / Mechanism'){
      SlopeXonY<-values$NumXY
      SlopeZonX<-values$NumXZ 
      SlopeZonY<-values$NumZY
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical=input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      NoiseX<-c(d[,1],d[,2])
      NoiseY<-c(d[,3],d[,4])
      
      # Z<-c(rep(-.5,N/2),rep(.5,N/2))
      Z<-c(rep(0,N/2),rep(1,N/2))
      X<-SlopeZonX*Z + NoiseX
      Y<-SlopeXonY*X + SlopeZonY*Z + NoiseY + values$Num.Intercept
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 0"
      Z[Z==TRUE]<-"Group 1"
    }
    
    if(input$RoleOfZ == 'Alternative Effect'){
      #technically I am modeling this like Z->X->Y because if I do Y<-X->Z, then have to deal with Z being binary and the simple relation between X and Z gets messed up; see the note above for mediation model. The same note applies here as well
      #However, because Z->X->Y and Z<-X->Y are Markov equivalent, the parameters will work out ok.
      SlopeXonY<-values$NumXY
      SlopeZonX<-values$NumXZ #see note above - doing a similar thing for the mechanism / mediator case
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical=input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      NoiseX<-c(d[,1],d[,2])
      NoiseY<-c(d[,3],d[,4])
      
      # Z<-c(rep(-.5,N/2),rep(.5,N/2))
      Z<-c(rep(0,N/2),rep(1,N/2))
      X<-SlopeZonX*Z + NoiseX
      Y<-SlopeXonY*X + NoiseY + + values$Num.Intercept
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 0"
      Z[Z==TRUE]<-"Group 1"
    }
    
    if(input$RoleOfZ == 'Common Effect'){
      SlopeXonY<-values$NumXY
      SlopeXonZ<-values$NumXZ
      SlopeYonZ<-values$NumZY
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,9),3,3)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseZ
      sigma[3,3]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N), mu=rep(0,3), sigma, empirical=input$DataGeneration) #using empirical here makes almost perfectly independent variables
      
      X<-d[,1]
      NoiseZ<-d[,2]
      NoiseY<-d[,3]
      
      Y<-SlopeXonY*X + NoiseY + values$Num.Intercept
      Z<-SlopeXonZ*X + SlopeYonZ*Y + NoiseZ
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 0"
      Z[Z==TRUE]<-"Group 1"
    }
    
    if(input$RoleOfZ == 'Interaction / Moderator'){
      SlopeXonY<-values$NumXY
      SlopeZonY<-values$NumZY
      Interaction<-values$Num.Interaction
      VarNoiseX<-1
      VarNoiseZ<-1
      VarNoiseY<-1
      
      sigma<-matrix(rep(0,16),4,4)
      sigma[1,1]<-VarNoiseX
      sigma[2,2]<-VarNoiseX
      sigma[3,3]<-VarNoiseY
      sigma[4,4]<-VarNoiseY
      d<-mvrnorm(n=as.integer(N/2), mu=rep(0,4), sigma, empirical= input$DataGeneration)
      
      NoiseX<-c(d[,1],d[,2])
      NoiseY<-c(d[,3],d[,4])
      
      Z<-c(rep(0,N/2),rep(1,N/2))*VarNoiseZ
      X<-NoiseX
      Y<-SlopeXonY*X + SlopeZonY*Z + Interaction*X*Z + NoiseY + values$Num.Intercept
      
      Z<-Z>mean(Z)
      Z[Z==FALSE]<-"Group 0"
      Z[Z==TRUE]<-"Group 1"
    }
    
    df<-data.frame(X,Y,Z)
    return(df)
  })
  
  simple_relation_Z_X <- reactive({ summary(lm(X~Z, data=currentDF() )) })
  simple_relation_Z_Y <- reactive({ summary(lm(Y~Z, data=currentDF() )) })
  current_regression_unconditional <- reactive({ summary(lm(Y~X, data=currentDF() )) })  
  current_regression_conditional <- reactive({ summary(lm(Y~X+Z, data=currentDF() )) })
  current_regression_interaction <- reactive({ summary(lm(Y~X*Z, data=currentDF() )) })
  
  
  defaultLimits <- reactive({
    df <- currentDF()
    list(
      xmin = round(mean(df$X),0)-6.01,
      xmax = round(mean(df$X),0)+6.01,
      ymin = round(mean(df$Y),0)-6.01,
      ymax = round(mean(df$Y),0)+6.01
    )
  })
  
  axisLimits <- reactiveVal()
  
  observeEvent(currentDF(), {
    axisLimits(defaultLimits())
  })

  
  observeEvent(input$Up, {
    limits <- axisLimits()
    axisLimits(modifyList(limits, list(
      ymin = limits$ymin+1,
      ymax = limits$ymax+1
    )))
  })
               
  observeEvent(input$Down, {
    limits <- axisLimits()
    axisLimits(modifyList(limits, list(
      ymin = limits$ymin-1,
      ymax = limits$ymax-1
    )))
  })
  
  observeEvent(input$Left, {
    limits <- axisLimits()
    axisLimits(modifyList(limits, list(
      xmin = limits$xmin-1,
      xmax = limits$xmax-1
    )))
  })
  
  observeEvent(input$Right, {
    limits <- axisLimits()
    axisLimits(modifyList(limits, list(
      xmin = limits$xmin+1,
      xmax = limits$xmax+1
    )))
  })
  
  observeEvent(input$Center, {
    axisLimits(defaultLimits())
  })
  

  
  output$distPlot <- renderGirafe({
    print("current distplot reactive triggered")
    df<-currentDF()
    
    #unconditional
    regression_unconditional<-current_regression_unconditional()
    intercept_unconditional<-coefficients(regression_unconditional)[1,1]
    XSlope_unconditional<-coefficients(regression_unconditional)[2,1]
    
    #conditional
    regression_conditional<-current_regression_conditional()
    intercept_conditional<-coefficients(regression_conditional)[1,1]
    XSlope_conditional<-coefficients(regression_conditional)[2,1]
    ZTrueEffect_conditional<-coefficients(regression_conditional)[3,1]
    
    #interaction
    regression_interaction<-current_regression_interaction()
    intercept_I_Z0<-        coefficients(regression_interaction)[1,1]
    XSlope_I_Z0<-           coefficients(regression_interaction)[2,1]
    intercept_diff_I_Z1<-   coefficients(regression_interaction)[3,1]
    interaction_I<-         coefficients(regression_interaction)[4,1]
    
    # Intercepts
    df_intercept_unconditional <-data.frame(X=c(0), Y=c(intercept_unconditional))
    df_intercept_conditional   <-data.frame(X=c(0), Y=c(intercept_conditional))
    df_intercept_interaction   <-data.frame(X=c(0), Y=c(intercept_I_Z0))
    
    # CI and Prediction intervals
    # http://www2.stat.duke.edu/~tjl13/s101/slides/unit6lec3H.pdf
    # https://rstudio-pubs-static.s3.amazonaws.com/71339_d0b8346f41314979bc394448c5d60d86.html
    
    # ConfidenceIntervals for Unconditional
    tX<-seq(min(df$X), max(df$X), 0.05)
    CI_Unconditional_Temp <- data.frame(X = tX)
    CI_Unconditional <- predict(lm(Y~X, data=df), CI_Unconditional_Temp, interval="confidence",level = 0.95)
    CI_Unconditional <- cbind(CI_Unconditional, CI_Unconditional_Temp)
    
    # Confidence Intervals for Conditional
    temp1<-subset(df, Z=='Group 0')
    tX<-seq(min(temp1$X), max(temp1$X), 0.05)
    CI_Conditional_Temp_ZFalse <- data.frame(X = tX, Z = rep('Group 0',length(tX)))
    CI_Conditional_ZFalse <- predict(lm(Y~X+Z, data=df), CI_Conditional_Temp_ZFalse, interval="confidence",level = 0.95)
    CI_Conditional_ZFalse <- cbind(CI_Conditional_ZFalse, CI_Conditional_Temp_ZFalse)
    
    temp2<-subset(df, Z=='Group 1')
    tX<-seq(min(temp2$X), max(temp2$X), 0.05)
    CI_Conditional_Temp_ZTrue <- data.frame(X = tX, Z = rep('Group 1',length(tX)))
    CI_Conditional_ZTrue <- predict(lm(Y~X+Z, data=df), CI_Conditional_Temp_ZTrue, interval="confidence",level = 0.95)
    CI_Conditional_ZTrue <- cbind(CI_Conditional_ZTrue, CI_Conditional_Temp_ZTrue)
    
    # Confidence Intervals for Interaction
    temp3<-subset(df, Z=='Group 0')
    tX<-seq(min(temp3$X), max(temp3$X), 0.05)
    CI_Interaction_Temp_ZFalse <- data.frame(X = tX, Z = rep('Group 0',length(tX)))
    CI_Interaction_ZFalse <- predict(lm(Y~X, data=temp3), CI_Interaction_Temp_ZFalse, interval="confidence",level = 0.95)
    CI_Interaction_ZFalse <- cbind(CI_Interaction_ZFalse, CI_Interaction_Temp_ZFalse)
    
    temp4<-subset(df, Z=='Group 1')
    tX<-seq(min(temp4$X), max(temp4$X), 0.05)
    CI_Interaction_Temp_ZTrue <- data.frame(X = tX, Z = rep('Group 1',length(tX)))
    CI_Interaction_ZTrue <- predict(lm(Y~X, data=temp4), CI_Interaction_Temp_ZTrue, interval="confidence",level = 0.95)
    CI_Interaction_ZTrue <- cbind(CI_Interaction_ZTrue, CI_Interaction_Temp_ZTrue)
    
    # dots for the curve for the interaction
    dot_Interaction_Z0_X <- 1
    dot_Interaction_Z0_Y <- intercept_I_Z0 + 1*XSlope_I_Z0
    dot_Interaction_Z1_X <- 1
    dot_Interaction_Z1_Y <- intercept_I_Z0 + intercept_diff_I_Z1 + 1*(XSlope_I_Z0+interaction_I) + .01 #.01 added so points are never exactly the same
    
    df_int_curve<-data.frame(x1=dot_Interaction_Z0_X, x2=dot_Interaction_Z1_X, y1=dot_Interaction_Z0_Y, y2=dot_Interaction_Z1_Y)
    
    # otherwise curve for interaction switches on the left vs. right sided curve
    if (dot_Interaction_Z0_Y < dot_Interaction_Z1_Y) {
      curvature_int <- .3
      show_interaction_curve <- TRUE
    } else if (dot_Interaction_Z0_Y > dot_Interaction_Z1_Y){
      curvature_int <- -.3
      show_interaction_curve <- TRUE
    } else if (dot_Interaction_Z0_Y == dot_Interaction_Z1_Y){
      show_interaction_curve <- FALSE
    }
    
    # Conditional Effect of Z in Interaction Model
    dfvert_int <- data.frame(x1 = 0, x2 = 0, Z1 = intercept_I_Z0, Z2 = intercept_I_Z0 + intercept_diff_I_Z1)
    # data frame for two dots for vertical line
    df_vertical_int<-data.frame(X=c(0,0), Y=c(intercept_I_Z0, intercept_I_Z0 + intercept_diff_I_Z1))
    
    
    # Conditional Effect of Z
    # data frame for the vertical line
    dfvert <- data.frame(x1 = 0, x2 = 0, Z1 = intercept_conditional, Z2 = intercept_conditional + ZTrueEffect_conditional)
    # data frame for two dots for vertical line
    df_vertical<-data.frame(X=c(0,0), Y=c(intercept_conditional, intercept_conditional + ZTrueEffect_conditional))
    
    # help with naming groups
    # https://stackoverflow.com/questions/18060116/adding-legend-to-ggplot-when-lines-were-added-manually
    # note - color needs to be inside aes call for this to work
    
    # data frame for two dots for AnalyzeXRegressZ
    df1AnalyzeXRegressZ<-data.frame(Y=c(6,6), X=c(simple_relation_Z_X()$coefficients[1,1]-.02, simple_relation_Z_X()$coefficients[1,1] + simple_relation_Z_X()$coefficients[2,1]))
    # data frame for the vertical line for AnalyzeYRegressZ
    df2AnalyzeXRegressZ <- data.frame(y1 = axisLimits()$ymax, y2 = axisLimits()$ymax, x1 = simple_relation_Z_X()$coefficients[1,1]-.02, x2 = simple_relation_Z_X()$coefficients[1,1] + simple_relation_Z_X()$coefficients[2,1])
    # data frame for two dots for AnalyzeYRegressZ
    # df1AnalyzeYRegressZ<-data.frame(X=c(4,4), Y=c(1, 2))
    df1AnalyzeYRegressZ<-data.frame(X=c(6,6), Y=c(simple_relation_Z_Y()$coefficients[1,1]-.02, simple_relation_Z_Y()$coefficients[1,1] + simple_relation_Z_Y()$coefficients[2,1]))
    # data frame for the vertical line for AnalyzeYRegressZ
    df2AnalyzeYRegressZ <- data.frame(x1 = axisLimits()$xmax, x2 = axisLimits()$xmax, y1 = simple_relation_Z_Y()$coefficients[1,1]-.02, y2 = simple_relation_Z_Y()$coefficients[1,1] + simple_relation_Z_Y()$coefficients[2,1]) 
    

    
    # first plot
    p <- ggplot(df, aes(X, Y))
    p <- p + labs(x = paste('X (', XNAME(), ')', sep=''), y=paste('Y (', YNAME(), ')', sep='') )
    if (as.numeric(input$N) <=100){size = 2}
    else if (input$N == 500){size = 1}
    else if (input$N == 1000){size = .5}
    # p <- p + geom_point(aes(color = Z), size=size)
    p <- p + scale_color_manual(values=c("Group 0"="red", "Group 1"="orange", "Overall"="purple", "Effect of Z"="green"),
                                labels=c('Group 0'=ZNAMEZ0(),
                                         'Group 1'=ZNAMEZ1(),
                                         "Overall"=paste( "Simple effect of X (", XNAME(), ")", sep=""),
                                         "Effect of Z"=paste("Effect of Z (", ZNAME(), ")", sep="")
                                ),
                                name='')

    
    p <- p + theme(axis.title=element_text(size=14),
                   base_size = 14,
                   legend.position="none",
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   plot.margin = unit(c(0,0,0,0), "cm"),
                   aspect.ratio = 1)
    
    
    
    
    # axes inside renderplot
    p <- p + scale_x_continuous(breaks = round(axisLimits()$xmin,0):round(axisLimits()$xmax,0), 
                                limits = c(axisLimits()$xmin,axisLimits()$xmax))
    p <- p + scale_y_continuous(breaks = round(axisLimits()$ymin,0):round(axisLimits()$ymax,0), 
                                limits = c(axisLimits()$ymin,axisLimits()$ymax))
    
    if (input$WATR == ""){ #on first load when it says "select one"
      p <- p + geom_point(aes(color = Z), size=size)
    }
    
    
    #Simple X~Z
    if (input$WATR == "[ X~Z ]"){
      p <- p + geom_point(aes(color = Z), size=size)
      p <- p + geom_vline_interactive(aes(data_id="Z0Fit", tooltip = "Z0 Mean", xintercept= simple_relation_Z_X()$coefficients[1,1]-.02,), color="red", linewidth=1.5)
      p <- p + geom_vline_interactive(aes(data_id="Z1Fit", tooltip = "Z1 Mean", xintercept= simple_relation_Z_X()$coefficients[1,1] + simple_relation_Z_X()$coefficients[2,1]), color="orange", linewidth=1.5)
      # p <- p + geom_point(aes(x=X, y=Y), size=3, color = "black", data=df1AnalyzeXRegressZ)
      p <- p + geom_segment_interactive(aes(data_id="DifferenceBetweenMeans", tooltip = "Difference Between Group Means", x = x1, y = y1, xend = x2, yend = y2), size=1.5, data = df2AnalyzeXRegressZ, color='black')
      # p <- p + geom_rug(aes(color = Z), sides='t')
    }
    
    #Simple Y~Z
    else if (input$WATR == "[ Y~Z ]"){
      p <- p + geom_point(aes(color = Z), size=size)
      p <- p + geom_hline_interactive(aes(data_id="Z0Fit", tooltip = "Z0 Mean", yintercept= simple_relation_Z_Y()$coefficients[1,1]-.02), color="red", linewidth=1.5)
      p <- p + geom_hline_interactive(aes(data_id="Z1Fit", tooltip = "Z1 Mean", yintercept= simple_relation_Z_Y()$coefficients[1,1] + simple_relation_Z_Y()$coefficients[2,1]), color="orange", linewidth=1.5)
      # p <- p + geom_point(aes(x=X, y=Y), size=3, color = "black", data=df1AnalyzeYRegressZ)
      p <- p + geom_segment_interactive(aes(data_id="DifferenceBetweenMeans", tooltip = "Difference Between Group Means", x = x1, y = y1, xend = x2, yend = y2), size=1.5, data = df2AnalyzeYRegressZ, color='black')
      # p <- p + geom_rug(aes(color = Z), sides='r') 
    }
    
    #Unconditional Regression Y~X
    if (input$WATR == "[ Y~X ]"){
      p <- p + geom_point(data=df, aes(X, Y), color="purple", alpha=1) #do I want to keep this or not?
      p <- p + geom_abline_interactive(aes(data_id="Fit", tooltip = "Line of best fit for all the data ignoring groups.", intercept = intercept_unconditional, slope = XSlope_unconditional, color="Overall"))
      p <- p + geom_ribbon_interactive(aes(data_id="ConfidenceBand", tooltip = "Confidence band: represents uncertainty about the regression line", x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "purple", alpha = 0.4, data=CI_Unconditional)
      p <- p + geom_point_interactive(aes(tooltip = "Intercept: level of Y where line crosses X=0", x=X, y=Y), shape=18, size=5, stroke=1.5, color="purple", data=df_intercept_unconditional)
      triangle <- data.frame(x= c(0,1,1), y=c(intercept_unconditional,intercept_unconditional,intercept_unconditional+XSlope_unconditional))
      p <- p + geom_polygon_interactive(aes(x=x, y=y, tooltip="Slope: ΔY/ΔX"), data = triangle, color="purple", fill=NA)
    }
    
    #note why I am using na.rm=TRUE. If I don't and if part of the shape falls outside the plotted graph, will generate an error and the responsiveness will break
    
    #Conditional Regression Y~X+Z
    if (input$WATR == "[ Y~X+Z ]"){
      p <- p + geom_point(aes(color = Z), size=size)
      #Group1
      p <- p + geom_abline_interactive(aes(data_id="Z0Fit", tooltip = "Line of best fit for Z0.", intercept = intercept_conditional, slope = XSlope_conditional), color="red", linetype = "solid")
      p <- p + geom_ribbon_interactive(data_id="Z0Band", aes(tooltip = "Confidence band aroud line of best fit for Z0", x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "red", alpha = 0.4, data=CI_Conditional_ZFalse, na.rm=TRUE)
      p <- p + geom_point_interactive(aes(data_id="Z0int", tooltip = "Z0 Intercept: level of Y where line crosses X=0", x=0, y=intercept_conditional), shape=18, size=5, stroke=1.5, color="red", na.rm=TRUE)
      triangleZ0 <- data.frame(x= c(0,1,1), y=c(intercept_conditional,intercept_conditional,intercept_conditional+XSlope_conditional))
      p <- p + geom_polygon_interactive(aes(data_id="Z0Triangle", tooltip="Z0 Slope: ΔY/ΔX", x=x, y=y), data = triangleZ0, color="magenta", fill=NA, na.rm=TRUE)
      
      #Group2
      p <- p + geom_abline_interactive(aes(data_id="Z1Fit", tooltip = "Line of best fit for Z1.", intercept = intercept_conditional+ZTrueEffect_conditional, slope = XSlope_conditional), color="orange", linetype = "solid")
      p <- p + geom_ribbon_interactive(aes(data_id="Z1Band", tooltip = "Confidence band aroud line of best fit for Z1", x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "orange", alpha = 0.4, data=CI_Conditional_ZTrue, na.rm=TRUE)
      p <- p + geom_point_interactive(aes(data_id="Z1int", tooltip = "Z1 Intercept: level of Y where line crosses X=0", x=0, y=intercept_conditional+ZTrueEffect_conditional), shape=18, size=5, stroke=1.5, color="orange", na.rm=TRUE)
      triangleZ1 <- data.frame(x= c(0,1,1), y=c(intercept_conditional+ZTrueEffect_conditional,intercept_conditional+ZTrueEffect_conditional,intercept_conditional+ZTrueEffect_conditional+XSlope_conditional), na.rm=TRUE)
      p <- p + geom_polygon_interactive(aes(data_id="Z1Triangle", tooltip="Z1 Slope: ΔY/ΔX", x=x, y=y), data = triangleZ1, color="magenta", fill=NA, na.rm=TRUE)
      
      #Vertical Line Effect of Z
      p <- p + geom_segment_interactive(aes(data_id="GroupDifferenceBetweenIntercepts", tooltip = "Regression weight for Z; the difference between the intercepts for Z1-Z0.", x = x1, y = Z1, xend = x2, yend = Z2), size=2, data = dfvert, color='green')
      
    }
    
    #Interaction Regression Y~X*Z
    if (input$WATR == "[ Y~X×Z ]"){
      p <- p + geom_point(aes(color = Z), size=size)
      #Group1
      p <- p + geom_abline_interactive(data_id="Z0Fit", tooltip = "Line of best fit for Z1.", intercept = intercept_I_Z0, slope = XSlope_I_Z0, color='red', linetype = "solid")
      p <- p + geom_ribbon_interactive(aes(data_id="Z11Band", tooltip = "Confidence band aroud line of best fit for Z11", x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "red", alpha = 0.4, data=CI_Interaction_ZFalse)
      p <- p + geom_point_interactive(aes(data_id="Z0int", tooltip = "Z0 Intercept: level of Y where line crosses X=0", x=0, y=intercept_I_Z0), shape=18, size=5, stroke=1.5, color="red", na.rm=TRUE)
      triangleZ0 <- data.frame(x= c(0,1,1), y=c(intercept_I_Z0,intercept_I_Z0,intercept_I_Z0+XSlope_I_Z0))
      p <- p + geom_polygon_interactive(aes(data_id="Z0Triangle", tooltip="Z0 Slope: ΔY/ΔX", x=x, y=y), data = triangleZ0, color="red", fill=NA, na.rm=TRUE)
      #Group2
      p <- p + geom_abline_interactive(data_id="Z1Fit", intercept = (intercept_I_Z0+intercept_diff_I_Z1), slope = (XSlope_I_Z0+interaction_I), color='orange', linetype = "solid")
      p <- p + geom_ribbon_interactive(aes(data_id="Z11Band", tooltip = "Confidence band aroud line of best fit for Z11", x=X, ymin=lwr, ymax=upr), inherit.aes = FALSE, fill = "orange", alpha = 0.4, data=CI_Interaction_ZTrue)
      p <- p + geom_point_interactive(aes(data_id="Z1int", tooltip = "Z1 Intercept: level of Y where line crosses X=0", x=0, y=intercept_I_Z0+intercept_diff_I_Z1), shape=18, size=5, stroke=1.5, color="orange", na.rm=TRUE)
      triangleZ1 <- data.frame(x= c(0,1,1), y=c(intercept_I_Z0+intercept_diff_I_Z1,intercept_I_Z0+intercept_diff_I_Z1,intercept_I_Z0+intercept_diff_I_Z1+XSlope_I_Z0+interaction_I), na.rm=TRUE)
      p <- p + geom_polygon_interactive(aes(data_id="Z1Triangle", tooltip="Z1 Slope: ΔY/ΔX", x=x, y=y), data = triangleZ1, color="orange", fill=NA, na.rm=TRUE)
      
      #Interaction Curve
      if (show_interaction_curve == TRUE){
        p <- p + geom_curve_interactive(aes(data_id="GroupDifferenceBetweenSlopes", tooltip = "Interaction term; the difference between the slopes Z1-Z0.", x = x1, y = y1, xend = x2, yend = y2), data = df_int_curve, curvature = curvature_int, color="dodgerblue", linetype = "solid", size=1.1)
      }
      
      #Vertical Line Effect of Z
      # p <- p + geom_point(aes(x=X, y=Y), size=3, data=df_vertical_int, color='yellow')
      p <- p + geom_segment_interactive(aes(data_id="GroupDifferenceBetweenIntercepts", tooltip = "Regression weight for Z; the difference between the intercepts for Z1-Z0.", x = x1, y = Z1, xend = x2, yend = Z2), size=2, data = dfvert_int, color='yellow')
      
      #Intercept
      # p <- p + geom_point(aes(x=X, y=Y), shape=5, size=3, stroke=1.5, color="black", data=df_intercept_interaction)
    }
    
    
    
    #for ggiraph
    girafe(ggobj = p, 
           width_svg = 500 / 96,
           height_svg = 500 / 96,
           options = list(
             opts_sizing(rescale=FALSE),
             opts_toolbar(saveaspng = FALSE),
             opts_selection(type="none"),
             opts_hover(css = "stroke:black;stroke-width:2px;"))
           )
  })

  
  output$MarginalXPlot <- renderPlot({
      df<-currentDF()
      p <- ggplot(df, aes(x=X, y="12", color=Z))
      p <- p + labs(x = '', y='')
      if (as.numeric(input$N) <=100){size = 2; cex = 1.6}
      else if (input$N == 500){size = 1; cex = 1}
      else if (input$N == 1000){size = .5; cex = .75}
      p <- p + geom_beeswarm(side = 1, cex=cex, method='swarm', size=size)
      p <- p + scale_color_manual(values=c("Group 0"="red", "Group 1"="orange"),
                                  labels=c('Group 0'=ZNAMEZ0(),'Group 1'=ZNAMEZ1()),name='')
      p <- p + theme(axis.text.y = element_text(color = "transparent"),
                     axis.ticks.y = element_line(color = "transparent"),
                     panel.grid.major.y = element_blank(),
                     axis.title=element_text(size=14),
                     axis.title.x = element_blank(),
                     axis.text.x  = element_blank(),
                     axis.ticks.x = element_blank(),
                     legend.position="none",
                     panel.grid.minor.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     plot.margin = unit(c(0,0,0,0), "cm"))
      p <- p + scale_x_continuous(breaks = axisLimits()$xmin:axisLimits()$xmax, limits = c(axisLimits()$xmin,axisLimits()$xmax))
      p <- p + scale_y_discrete(expand = expansion(mult = c(.01, 1)))
      p
    }, width = 500, height = 500, res = 96
    )
  
  
  output$MarginalYPlot <- renderPlot({
    df<-currentDF()
    p <- ggplot(df, aes(x="12", y=Y, color=Z))
    p <- p + labs(x = '', y='')
    if (as.numeric(input$N) <=100){size = 2; cex = 1.6}
    else if (input$N == 500){size = 1; cex = 1}
    else if (input$N == 1000){size = .5; cex = .75}
    p <- p + geom_beeswarm(side = 1, cex=cex, method='swarm', size=size)
    p <- p + scale_color_manual(values=c("Group 0"="red", "Group 1"="orange"),
                                labels=c('Group 0'=ZNAMEZ0(),'Group 1'=ZNAMEZ1()),name='')
    p <- p + theme(axis.text.x = element_text(color = "transparent"),
                   axis.ticks.x = element_line(color = "transparent"),
                   panel.grid.major.x = element_blank(),
                   axis.title=element_text(size=14),
                   axis.title.y = element_blank(),
                   axis.text.y  = element_blank(),
                   axis.ticks.y = element_blank(),
                   legend.position="none",
                   panel.grid.minor.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   plot.margin = unit(c(0,0,0,0), "cm"))
    p <- p + scale_y_continuous(breaks = axisLimits()$ymin:axisLimits()$ymax, limits = c(axisLimits()$ymin,axisLimits()$ymax))
    p <- p + scale_x_discrete(expand = expansion(mult = c(.01, 1)))
    p
  }, width = 500, height = 500, res = 96
  )
  

  
  #### Below is the html for how all the regression tables are produced.
  ## First an html table is made
  ## Then certain key words are replaced dynamically based on regression results and variable names
  
  ## Z~X
  
  simple_relation_Z_X_HTML.Model<-'
This regression result is essentially the same thing as running a t-test comparing the two different groups of Z on X.
<table class="regression-table">
  <tr>
    <th style="text-align:left">Predictor</th>
    <th>B</th>
    <th>p-value</th>
    <th>Meaning</th>
    </tr>
  <tr>
    <td style="text-align:left">Z (ZNAME)</td>
    <td style="color:white">Z.B</td>
    <td>Z.p</td>
    <td>difference between group means on the X axis</td>
    </tr>
</table>'
  
  
  output$simple_relation_Z_X_HTML <- 
    renderText({str_replace_all(simple_relation_Z_X_HTML.Model,
                                c("Z.B" = toString(formatC(simple_relation_Z_X()$coefficients[2,1], 
                                                           format = 'f', digits=3)),
                                  "Z.p" = toString(formatC(simple_relation_Z_X()$coefficients[2,4], 
                                                           format = 'f', digits=3)),
                                  "ZNAME" = ZNAME()
                                ))
    })
  
  
  ## Z~Y
  
  simple_relation_Z_Y_HTML.Model<-'
This regression result is essentially the same thing as running a t-test comparing the two different groups of Z on Y.
<table class="regression-table">
  <tr>
    <th style="text-align:left">Predictor</th>
    <th>B</th>
    <th>p-value</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td style="text-align:left">Z (ZNAME)</td>
    <td style="color:white" bgcolor="black">Z.B</td>
    <td>Z.p</td>
    <td>difference between group means on the Y axis</td>
  </tr>
</table>'
  
  output$simple_relation_Z_Y_HTML <- 
    renderText({str_replace_all(simple_relation_Z_Y_HTML.Model,
                                c("Z.B" = toString(formatC(simple_relation_Z_Y()$coefficients[2,1], 
                                                           format = 'f', digits=3)),
                                  "Z.p" = toString(formatC(simple_relation_Z_Y()$coefficients[2,4], 
                                                           format = 'f', digits=3)),
                                  "ZNAME" = ZNAME()
                                ))
    })
  
  ## Y~X
  
  current_regression_unconditional_HTML.Model<-'
This regression result is essentially the same thing as running a correlation between X and Y and ignoring the two groups of Z.
<table class="regression-table">
  <tr>
    <th style="text-align:left">Predictor</th>
    <th>B</th>
    <th>p-value</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td style="text-align:left">Intercept</td>
    <td style="color:#9e37eb">I.B</td>
    <td>I.p</td>
    <td>&#x25C6where the line crosses X=0</td>
  </tr>
  <tr>
    <td style="text-align:left">X (XNAME)</td>
    <td style="color:#9e37eb" bgcolor="black">X.B</td>
    <td>X.p</td>
    <td>&#x25FF slope (rise/run)</td>
  </tr>
</table>'
  
  output$current_regression_unconditional_HTML <- 
    renderText({str_replace_all(current_regression_unconditional_HTML.Model,
                                c("I.B" = toString(formatC(current_regression_unconditional()$coefficients[1,1], 
                                                           format = 'f', digits=3)),
                                  "I.p" = toString(formatC(current_regression_unconditional()$coefficients[1,4], 
                                                           format = 'f', digits=3)),
                                  "X.B" = toString(formatC(current_regression_unconditional()$coefficients[2,1], 
                                                           format = 'f', digits=3)),
                                  "X.p" = toString(formatC(current_regression_unconditional()$coefficients[2,4], 
                                                           format = 'f', digits=3)),
                                  "XNAME" = XNAME()
                                ))
    })
  
  
  ## Y~X+Z
  
  current_regression_conditional_HTML.Model<-'
This regression result is similar to running two regressions, one for each of the groups of Z, but with the caveat that that the lines of best fit must have the same slopes. 
<table  class="regression-table">
<tr>
  <th style="text-align:left">Predictor</th>
  <th>B</th>
  <th>p-value</th>
  <th>Meaning</th>
</tr>
<tr>
  <td style="text-align:left">Z0 Intercept</td>
  <td>I.B</td>
  <td>I.p</td>
  <td>&#x25C6 where line for Z0 crosses X=0</td>
</tr>
<tr>
  <td style="text-align:left">X (XNAME)</td>
  <td style="color:#FF00FF" bgcolor="black">X.B</td>
  <td>X.p</td>
  <td>&#x25FF slope (same for both groups)</td>
</tr>
<tr>
  <td style="text-align:left">Z (ZNAME)</td>
  <td style="color:#00FF00" bgcolor="black">Z.B</td>
  <td>Z.p</td>
  <td>difference in intercepts (Z1-Z0)</td>
</tr>
</table>'
  
  output$current_regression_conditional_HTML <- 
    renderText({str_replace_all(current_regression_conditional_HTML.Model,
                                c("I.B" = toString(formatC(current_regression_conditional()$coefficients[1,1], 
                                                           format = 'f', digits=3)),
                                  "I.p" = toString(formatC(current_regression_conditional()$coefficients[1,4], 
                                                           format = 'f', digits=3)),
                                  "X.B" = toString(formatC(current_regression_conditional()$coefficients[2,1], 
                                                           format = 'f', digits=3)),
                                  "X.p" = toString(formatC(current_regression_conditional()$coefficients[2,4], 
                                                           format = 'f', digits=3)),
                                  "Z.B" = toString(formatC(current_regression_conditional()$coefficients[3,1], 
                                                           format = 'f', digits=3)),
                                  "Z.p" = toString(formatC(current_regression_conditional()$coefficients[3,4], 
                                                           format = 'f', digits=3)),
                                  "XNAME" = XNAME(),
                                  "ZNAME" = ZNAME()
                                ))
    })
  
  ## Y~X*Z
  
  current_regression_interaction_HTML.Model<-'
By including the interaction (X×Z), this regression now fits two different lines for the two groups of Z, and tests whether the slopes are significantly different.<br><br>
Note that the symbol × is sometimes replaced with *. Also, note that [ Y~X×Z ] is technically an abbreviation for [ Y~X+Y+X×Z ]. This is why in the table below you will see regression outputs for the three predictors X, Z, and X×Z.<br><br>
<table  class="regression-table">
  <tr>
    <th style="text-align:left">Predictor</th>
    <th>B</th>
    <th>p-value</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td style="text-align:left">Intercept</td>
    <td>I.B </td>
    <td>I.p</td>
    <td>&#x25C6;where line for Z0 crosses X=0</td>
  </tr>
  <tr>
    <td style="text-align:left">X (XNAME)</td>
    <td style="color:red" bgcolor="black">X.B</td>
    <td>X.p</td>
    <td>&#x25FF slope (rise/run) for Z0</td>
  </tr>
  <tr>
    <td style="text-align:left">Z (ZNAME)</td>
    <td style="color:yellow" bgcolor="black">Z.B</td>
    <td>Z.p</td>
    <td>difference in intercepts for Z1-Z0</td>
  </tr>
  <tr>
    <td style="text-align:left">X×Z</td>
    <td style="color:dodgerblue" bgcolor="black">Int.B</td>
    <td>Int.p</td>
    <td>the interaction (difference in slopes) for the two groups</td>
  </tr>
</table>'
  
  output$current_regression_interaction_HTML <- 
    renderText({str_replace_all(current_regression_interaction_HTML.Model,
                                c("I.B" = toString(formatC(current_regression_interaction()$coefficients[1,1], 
                                                           format = 'f', digits=3)),
                                  "I.p" = toString(formatC(current_regression_interaction()$coefficients[1,4], 
                                                           format = 'f', digits=3)),
                                  "X.B" = toString(formatC(current_regression_interaction()$coefficients[2,1], 
                                                           format = 'f', digits=3)),
                                  "X.p" = toString(formatC(current_regression_interaction()$coefficients[2,4], 
                                                           format = 'f', digits=3)),
                                  "Z.B" = toString(formatC(current_regression_interaction()$coefficients[3,1], 
                                                           format = 'f', digits=3)),
                                  "Z.p" = toString(formatC(current_regression_interaction()$coefficients[3,4], 
                                                           format = 'f', digits=3)),
                                  "Int.B" = toString(formatC(current_regression_interaction()$coefficients[4,1], 
                                                             format = 'f', digits=3)),
                                  "Int.p" = toString(formatC(current_regression_interaction()$coefficients[4,4], 
                                                             format = 'f', digits=3)),
                                  "XNAME" = XNAME(),
                                  "ZNAME" = ZNAME()
                                ))
    })
  
  
  
  ## The Key/Legend for the graph is made with text, not an image, and changes based on whether one or two colors are chosen
  
  output$KeyText <-renderText({
      KT<-('<b>Key:</b>&nbsp;&nbsp;&nbsp;<span style="color:red">&#9679 Group Z0 (ZNAMEZ0)</span>&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:orange">&#9679 Group Z1 (ZNAMEZ1)')
    return(str_replace_all(KT,c("ZNAMEZ0" = ZNAMEZ0(), "ZNAMEZ1" = ZNAMEZ1())))
  })
  
  
  observe({
    req(input$RoleOfZ)
    
    # clear everything by default
    values$NumXY <- 0
    values$NumZY <- 0
    values$NumXZ <- 0
    values$Num.Interaction <- 0
    # values$ColorXY <- "#FF00FF"
    # values$ColorZY <- "#00FF00"
    # values$ColorXZ <- "black"
    # values$ColorInteraction <- "black"
    #Opacity of arrows
    values$OP.XY <- 0
    values$OP.ZY <- 0
    values$OP.YZ <- 0
    values$OP.ZX <- 0
    values$OP.XZ <- 0
    values$OP.Int <- 0
    #visibility of strengths. Has to be done with visibility because opaque elements can still be clicked on
    values$V.XY <- "hidden"
    values$V.ZY <- "hidden"
    values$V.XZ <- "hidden"
    values$V.Int <- "hidden"
    
    # apply logic
    if (input$RoleOfZ == "Alternative Cause") {
      if (values$N_SlopeXonY == 0) {
        values$OP.ZY <- 1
      } else {
        values$OP.XY <- 1
        values$OP.ZY <- 1
      }
      values$NumXY <- values$N_SlopeXonY
      values$NumZY <- values$N_SlopeZonY
      values$V.XY <- "visible"
      values$V.ZY <- "visible"
      values$ColorXY <- "#FF00FF"
      values$ColorZY <- "#00FF00"
    }
    
    else if (input$RoleOfZ == "Confound / Common Cause") {
      if (values$CC_SlopeXonY == 0) {
        values$OP.ZY <- 1
        values$OP.ZX <- 1
      } else {
        values$OP.XY <- 1
        values$OP.ZY <- 1
        values$OP.ZX <- 1
      }
      values$NumXY <- values$CC_SlopeXonY
      values$NumZY <- values$CC_SlopeZonY
      values$NumXZ <- values$CC_SlopeZonX
      values$V.XY <- "visible"
      values$V.ZY <- "visible"
      values$V.XZ <- "visible"
      values$ColorXY <- "#FF00FF"
      values$ColorZY <- "#00FF00"
      values$ColorXZ <- "white"
    }
    
    else if (input$RoleOfZ == "Alternative Effect") {
      if (values$AE_SlopeXonY == 0) {
        values$OP.XZ <- 1
      } else {
        values$OP.XY <- 1
        values$OP.XZ <- 1
      }
      values$NumXY <- values$AE_SlopeXonY
      values$NumXZ <- values$AE_SlopeXonZ
      values$V.XY <- "visible"
      values$V.XZ <- "visible"
      values$ColorXY <- "#9e37eb"
      values$ColorXZ <- "white"
    }
    
    else if (input$RoleOfZ == "Mediator / Mechanism") {
      if (values$M_SlopeXonY == 0) {
        values$OP.ZY <- 1
        values$OP.XZ <- 1
      } else {
        values$OP.XY <- 1
        values$OP.ZY <- 1
        values$OP.XZ <- 1
      }
      values$ColorXZ <- "white"
      values$NumXY <- values$M_SlopeXonY
      values$NumZY <- values$M_SlopeZonY
      values$NumXZ <- values$M_SlopeXonZ
      values$V.XY <- "visible"
      values$V.ZY <- "visible"
      values$V.XZ <- "visible"
      values$ColorXY <- "#FF00FF"
      values$ColorZY <- "#00FF00"
      values$ColorXZ <- "white"
    }
    else if (input$RoleOfZ == "Interaction / Moderator") {
      values$OP.Int <- 1
      values$OP.XY <- 1
      values$OP.ZY <- 1
      values$NumXY <- values$I_SlopeXonY
      values$NumZY <- values$I_SlopeZonY
      values$Num.Interaction <- values$I_Interaction
      values$V.XY <- "visible"
      values$V.ZY <- "visible"
      values$V.Int <- "visible" 
      values$ColorXY <- "Red"
      values$ColorZY <- "Yellow"
      values$ColorInteraction <- "dodgerblue"
    }
    else if (input$RoleOfZ == "Common Effect") {
      if (values$CE_SlopeXonY == 0) {
        values$OP.YZ <- 1
        values$OP.XZ <- 1
      } else {
        values$OP.XY <- 1
        values$OP.YZ <- 1
        values$OP.XZ <- 1
      }
      values$NumXY <- values$CE_SlopeXonY
      values$NumZY <- values$CE_SlopeYonZ
      values$NumXZ <- values$CE_SlopeXonZ
      values$V.ZY <- "visible"
      values$V.XZ <- "visible"
      values$ColorZY <- "white"
      values$ColorXZ <- "white"
    }
    

  })
  
  
  output$CSGraph <- renderUI({
    tags$svg(width = "210px", height = "250px",
             # defs
             tags$defs(
               tags$marker(
                 id = "arrow", markerWidth = 10, markerHeight = 10, refX = 0, refY = 3,
                 orient = "auto", markerUnits = "strokeWidth",
                 tags$path(d = "M0,0 L0,6 L9,3 z", fill = "white")
               ),
               tags$marker(
                 id = "arrow-grey", markerWidth = 10, markerHeight = 10, refX = 0, refY = 3,
                 orient = "auto", markerUnits = "strokeWidth",
                 tags$path(d = "M0,0 L0,6 L9,3 z", fill = "grey")
               )
             ),
               
               
             
             # background
             tags$rect(width = "100%", height = "100%", fill = "black", stroke = "#000", "stroke-width" = 1),
             
             # arrows
             tags$line(x1 = 30,  y1 = 75,  x2 = 30,  y2 = 148, stroke = "white", "stroke-width" = 2, opacity = values$OP.XY, "marker-end" = "url(#arrow)"),
             tags$line(x1 = 150, y1 = 80, x2 = 58,  y2 = 158, stroke = "white", "stroke-width" = 2, opacity = values$OP.ZY, "marker-end" = "url(#arrow)"),
             tags$line(x1 = 30,  y1 = 180, x2 = 123, y2 = 103, stroke = "white", "stroke-width" = 2, opacity = values$OP.YZ, "marker-end" = "url(#arrow)"),
             tags$line(x1 = 30,  y1 = 60,  x2 = 115, y2 = 73, stroke = "white", "stroke-width" = 2, opacity = values$OP.XZ, "marker-end" = "url(#arrow)"),
             tags$line(x1 = 150, y1 = 80, x2 = 65,  y2 = 65,  stroke = "white", "stroke-width" = 2, opacity = values$OP.ZX, "marker-end" = "url(#arrow)"),
             
             tags$line(x1 = 180, y1 = 180, x2 = 67,  y2 = 180,  stroke = "grey", "stroke-width" = 2, "marker-end" = "url(#arrow-grey)"),
             
             # interaction arc
             tags$path(d = "M30 135 Q 65 100 72 146", stroke = values$ColorInteraction, "stroke-width" = 2, opacity = values$OP.Int, fill = "none"),
             
             # nodes
             tags$circle(cx = 30,  cy = 60,  r = 15, stroke = "black", "stroke-width" = 2, fill = "white"),
             tags$text("X", x = 30, y = 65.5, fill = "black", "text-anchor" = "middle", "font-family" = "sans-serif"),
             
             tags$circle(cx = 30,  cy = 180, r = 15, stroke = "black", "stroke-width" = 2, fill = "white"),
             tags$text("Y", x = 30, y = 185.5, fill = "black", "text-anchor" = "middle", "font-family" = "sans-serif"),
             
             tags$circle(cx = 150, cy = 80, r = 15, stroke = "black", "stroke-width" = 2, fill = "white"),
             tags$text("Z", x = 150, y = 85.5, fill = "black", "text-anchor" = "middle", "font-family" = "sans-serif"),
             
             # === CLICKABLE NUMBERS ===
             # XY
             tags$circle(cx = 30, cy = 115, r = 12, fill = "black", class = "edge-label", `data-edge-id` = "NumXY", visibility = values$V.XY),
             tags$text(values$NumXY, x = 30, y = 120.5, fill = values$ColorXY, "text-anchor" = "middle", class = "edge-label", `data-edge-id` = "NumXY", visibility = values$V.XY),
             # ZY
             tags$circle(cx = 95, cy = 127, r = 12, fill = "black", class = "edge-label", `data-edge-id` = "NumZY", visibility = values$V.ZY),
             tags$text(values$NumZY, x = 95, y = 134, fill = values$ColorZY, "text-anchor" = "middle", class = "edge-label", `data-edge-id` = "NumZY", visibility = values$V.ZY),
             # XZ
             tags$circle(cx = 95, cy = 70, r = 12, fill = "black", class = "edge-label", `data-edge-id` = "NumXZ", visibility = values$V.XZ),
             tags$text(values$NumXZ, x = 95, y = 77, fill = values$ColorXZ, "text-anchor" = "middle", class = "edge-label", `data-edge-id` = "NumXZ", visibility = values$V.XZ),
             # Interaction
             tags$circle(cx = 60, cy = 120, r = 12, fill = "black", class = "edge-label", `data-edge-id` = "Num.Interaction", visibility = values$V.Int),
             tags$text(values$Num.Interaction, x = 60, y = 122, fill = values$ColorInteraction, "text-anchor" = "middle", class = "edge-label", `data-edge-id` = "Num.Interaction", visibility = values$V.Int),
             # Intercept
             tags$circle(cx = 150, cy = 180, r = 12, fill = "black", class = "edge-label", `data-edge-id` = "Num.Intercept", visibility = "visible"),
             tags$text(values$Num.Intercept, x = 150, y = 185.5, fill = "grey", "text-anchor" = "middle", class = "edge-label", `data-edge-id` = "Num.Intercept", visibility = "visible"),
                          
             # descriptors (unchanged)
             tags$text("Independent Variable:", x = 10, y = 20,  fill = "grey", "text-anchor" = "left",  "font-style" = "italic", "font-family" = "sans-serif"),
             tags$text(XNAME(),                   x = 10, y = 35,  fill = "grey", "text-anchor" = "left",  "font-style" = "italic", "font-family" = "sans-serif"),
             tags$text("Dependent Variable:",     x = 10, y = 220, fill = "grey", "text-anchor" = "left",  "font-style" = "italic", "font-family" = "sans-serif"),
             tags$text(YNAME(),                   x = 10, y = 235, fill = "grey", "text-anchor" = "left",  "font-style" = "italic", "font-family" = "sans-serif"),
             tags$text("Group", x = 174, y = 80, fill = "grey", "text-anchor" = "middle", "font-style" = "italic",
                       transform = "rotate(90, 174, 80)", "font-family" = "sans-serif"),
             tags$text(ZNAME(), x = 190, y = 80, fill = "grey", "text-anchor" = "middle", "font-style" = "italic",
                       transform = "rotate(90, 190, 80)", "font-family" = "sans-serif"),
             tags$text("Intercept", x = 190, y = 180, fill = "grey", "text-anchor" = "middle", "font-style" = "italic",
                       transform = "rotate(90, 190,180)", "font-family" = "sans-serif")
    )
  })
  
  
  # Only send registration message if not already registered
  observe({
    req(!clickHandlersRegistered())
    invalidateLater(500, session)
    session$sendCustomMessage("registerClickHandlers", list())
  })
  
  # Receive acknowledgement from JS that handlers are registered
  observeEvent(input$clickHandlersReady, {
    clickHandlersRegistered(TRUE)
  })
  
  observeEvent(input$testRegister, {
    session$sendCustomMessage("registerClickHandlers", list())
    cat("Manual trigger of registerClickHandlers\n")
  })
  
  
  observeEvent(input$clicked_edge, {
    showModal(
      jqui_draggable(
        modalDialog(
          # title = paste("Adjust:", input$clicked_edge),
          sliderInput("edge_strength", NULL,
                      min = -3, max = 3, step = 0.5,
                      value = isolate(values[[input$clicked_edge]])),
          footer = NULL,
          easyClose = TRUE,
        )
      )
    )
  })
  
  
  
  observeEvent(input$edge_strength, {
    req(input$clicked_edge)
    values[[input$clicked_edge]] <- input$edge_strength
    # cat("Updated", input$clicked_edge, "to", input$edge_strength, "\n")
  })
  
  # observe({
  #   print(paste("Current NumZY is", values$NumZY))
  # })
  
# observe({
#   vals <- reactiveValuesToList(values)
#   print("Reactive values changed")
# })
#   
#   observe({
#     currentDF()
#     print("currentDF has been updated")
#   })

  
} #end of server

# Run the application 
shinyApp(ui = ui, server = server)

# Colors being used in various places
# #9e37eb Purple
# #00FF00 Green
# #FF00FF Magenta
# #2993fc Blue or "dodgerblue"
# #fffa40 Yellow

