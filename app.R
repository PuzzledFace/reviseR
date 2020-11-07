library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinyjs)

source("functions.R")
sourceFolder("modules")
incorrectAnswerStrings <- c("Bad luck.", "That's not quite right.", "Better luck next time.", "Oops!")

ui <- dashboardPage(
   dashboardHeader(title="ReviseR: a revision aid "),
   dashboardSidebar(
     sidebarMenu(
       menuItem("Questions", tabName="questions", icon=icon("question")),
       menuItem("Settings", tabName="settings", icon=icon("cog")),
       menuItem("Help", tabName="help", icon=icon("info"))
     )
   ),
   dashboardBody(
     useShinyjs(),
     tabItems(
       tabItem(
         tabName="questions",
         title="Questions",
         box(
           title="Question filter",
           collapsible=TRUE,
           collapsed=FALSE,
           solidHeader=TRUE,
           status="info",
           selectInput("categories", label="Categories", choices=c(), selected=NA, multiple=TRUE),
           selectInput("subCategories", label="Sub-categories", choices=c(), selected=NA, multiple=TRUE),
           textOutput("questionCount")
         ),
         box(
           title="Test options",
           collapsible=TRUE,
           collapsed=FALSE,
           solidHeader=TRUE,
           status="info",
           numericInput("nQuestions", "Number of questions:", min=0, value=3),
           checkboxInput("timedTest", "Use timer?"),
           numericInput("timeLimit", "Time limit (minutes):", value=NA),
           actionButton("startButton", "Start"),
           actionButton("quitButton", "Quit")
         ),
         box(
           title="Questions",
           collapsible=TRUE,
           collapsed=FALSE,
           solidHeader=TRUE,
           status="primary",
           width=12,
           uiOutput("timer"),
           uiOutput("questionPanel")
         )
       ),
       tabItem(
         tabName="settings",
         title="Settings",
         fluidRow(
           box(
             title="Question files",
             collapsible=TRUE,
             collapsed=FALSE,
             solidHeader=TRUE,
             status="info",
             questionFilesUI("questionFiles")
           )
         )
       ),
       tabItem(
         tabName="help",
         title="Help",
         fluidRow(includeMarkdown("README.md"))
       )
     )
   )
)

server <- function(input, output, session) {
  # getFilePath <- function(fileName) {
  #   x <- paste0(session$userData$filePath, fileName)
  #   print(session$userData$filePath)
  #   print(fileName)
  #   print(x)
  #   print(list.files(include.dirs=TRUE, recursive=TRUE))
  #   return(x)
  # }
  
  # Settings UI
  qaFiles <- questionFilesController("questionFiles")
  # End settings UI

  setGUIState <- function() {
    if (session$userData$test$started) {
      disable("nQuestions")
      disable("categories")
      disable("subCategories")
      disable("timedTest")
      disable("timeLimit")
      disable("startButton")
      enable("quitButton")
      print(session$userData$test$currentQuestion)
      print(input$nQuestions)
      print(session$userData$test$currentQuestion == input$nQuestions)
      if (session$userData$test$currentQuestion == input$nQuestions) {
        disable("nextButton")
        print("Disabled")
      }
      else { 
        enable("nextButton")
        print("Enabled")
      }
      if (session$userData$test$currentQuestion == 1) disable("prevButton")
      else enable("prevButton")
      if (session$userData$test$currentQuestion %in% names(session$userData$test$answers)) disable("SubmitButton")
      else enable("submitButton")
    } else {
      enable("nQuestions")
      enable("categories")
      enable("subCategories")
      enable("timedTest")
      enable("timeLimit")
      enable("startButton")
      disable("quitButton")
    }
  }
  
  resetTest <- function() {
    session$userData$test <- reactiveValues(
      currentQuestion=NA, 
      questions=NA,
      answers=list(),
      startTime=NA,
      elapsedTime=NA,
      started=FALSE
    )
    setGUIState()
  }
  
  # Initialise
  session$userData$initialised <- FALSE
  timer <- reactiveTimer()
  
  observe({
    # printDebug("Entry")
    timer()
    if (input$timedTest) session$userData$test$elapsedTime <- dseconds(session$userData$test$startTime %--% lubridate::now())
  },
  label="timer")
  
  observe({
    printDebug("Entry")
    if (!session$userData$initialised) {
      session$userData$filePath <- ifelse(session$clientData$url_hostname == "puzzledface.shinyapps.io", paste0(session$clientData$url_pathname, "www/"), paste0("www", session$clientData$url_pathname))
      resetTest()
      session$userData$initialised <- TRUE
    }
  },
  label="userData-initialised"
  )
  
  questions <- reactive({
    printDebug("Entry")
    bind_rows(lapply(qaFiles(), function(x) x()$questions))
  },
  label="questions"
  )
  
  availableQuestions <- reactive({
    printDebug("Entry")
    
    x <- questions()
    if (is.null(input$categories)) return(questions())
    x <- questions() %>% filter(Category %in% input$categories)
    if (is.null(input$subCategories)) return(x)
    x <- x %>% filter(SubCategory %in% input$subCategories)
  },
  label="availableQuestions"
  )
  
  observeEvent(availableQuestions(), {
    n <- availableQuestions() %>% nrow()
    m <- input$nQuestions
    updateNumericInput(session, "nQuestions", max=n ,value=min(n, m))
  })
  
  answers <- reactive({
    req(session$userData$filePath)
    printDebug("Entry")
    a <- bind_rows(lapply(qaFiles(), function(x) x()$answers))
    a <- a %>% group_by(QuestionID) 
    fixedIndices <- a %>% filter(!is.na(AnswerIndex)) %>% rename(PossibleIndex=AnswerIndex)
    possibleIndices <- a %>% 
                         summarise(
                           GroupSize=n(),
                           PossibleIndex=list(1:GroupSize),
                           .groups="drop"
                         ) %>% 
                         unnest(c(PossibleIndex)) %>%
                         anti_join(fixedIndices, by=c("QuestionID", "PossibleIndex")) %>%
                         mutate(
                           RandomOrder=runif(nrow(.)),
                           FixedIndex=FALSE
                         ) %>%
                         arrange(QuestionID, FixedIndex) %>% 
                         group_by(QuestionID, FixedIndex) %>% 
                         mutate(GroupIndex=row_number()) %>% 
                         rename(DisplayIndex=PossibleIndex) %>%
                         select(QuestionID, FixedIndex, GroupIndex, DisplayIndex) %>%
                         ungroup()
    a %>%
           mutate(FixedIndex=!is.na(AnswerIndex)) %>%
           group_by(QuestionID, FixedIndex) %>%
           mutate(GroupIndex=row_number()) %>%
           left_join(possibleIndices, by=c("QuestionID", "FixedIndex", "GroupIndex")) %>% 
           mutate(
             DisplayIndex=coalesce(AnswerIndex, DisplayIndex),
             IndexedAnswerText=paste0(DisplayIndex, ": ", AnswerText),
             Icon=ifelse(is.na(Correct), "times-circle", "check-circle")
           ) %>% 
    ungroup() %>% 
    select(-FixedIndex, -GroupIndex) 
  },
  label="answers"
  )
  
  currentQuestion <- reactive({
    req(session$userData$test$currentQuestion)
    
    session$userData$test$questions %>% slice(session$userData$test$currentQuestion)
  },
  label="currentQuestion"
  )
  
  currentAnswers <- reactive({
    req(currentQuestion())
    
    printDebug("Entry")
    answers() %>% filter(QuestionID == currentQuestion()$QuestionID)
  },
  label="currentAnswers"
  )
  
  observeEvent(input$startButton, {
    printDebug("Entry")
    q <- availableQuestions() %>% 
           sample_n(input$nQuestions) %>% 
           add_column(
             Answered=FALSE,
             AnsweredCorrectly=NA
           )
    session$userData$test$questions <- q
    session$userData$test$currentQuestion <- 1
    session$userData$test$startTime <- lubridate::now()
    session$userData$test$started <- TRUE
    setGUIState()
  },
  label="startButton"
  )
  
  observeEvent(input$submitButton, {
    printDebug("Entry")
    # Make a note of the user's answers
    correctAnswers <- currentAnswers() %>% filter(!is.na(Correct)) %>% pull(DisplayIndex)
    session$userData$test$answers[[session$userData$test$currentQuestion]] <- input$answerButtons
    session$userData$test$answerSubmitted <- TRUE
    session$userData$test$questions <- session$userData$test$questions %>% 
                                         mutate(
                                           Answered=ifelse(QuestionID == currentQuestion()$QuestionID, TRUE, Answered),
                                           AnsweredCorrectly=ifelse(
                                                               QuestionID == currentQuestion()$QuestionID, 
                                                               identical(as.character(correctAnswers), session$userData$test$answers[[session$userData$test$currentQuestion]]),
                                                               AnsweredCorrectly
                                                             )
                                         )
    setGUIState()
  },
  label="submitButton"
  )

  observeEvent(input$nextButton, {
    session$userData$test$currentQuestion <- session$userData$test$currentQuestion + 1
    setGUIState()
  },
  label="nextButton"
  )
  
  observeEvent(input$prevButton, {
    session$userData$test$currentQuestion <- session$userData$test$currentQuestion - 1
    setGUIState()
  },
  label="prevButton"
  )
  
  observeEvent(questions(), {
    printDebug("Entry")
    selected <- input$categories
    values <- c("- All -"="", questions() %>% pull(Category) %>% unique())
    updateSelectInput(session, "categories", choices=values, selected=selected)
  },
  label="questionsReactive"
  )
  
  observeEvent(input$categories, {
    printDebug("Entry")
    selected <- input$subCategories
    values <- questions() %>% select(Category, SubCategory)
    if (length(input$categories) > 0) values <- values %>% filter(Category %in% input$categories)
    values <- values %>% pull(SubCategory) %>% unique()
    updateSelectInput(session, "subCategories", choices=values, selected=selected)
  },
  label="observeEvent-input-categories"
  )
  
  output$questionCount <- renderText({
    printDebug("questionCount: Entry")
    sprintf(
      "%i questions in %i categories are available.",
      availableQuestions() %>% nrow(),
      length(availableQuestions() %>% pull(Category) %>% unique())
    )
  })
  
  output$currentQuestion <- renderText({
    printDebug("currentQuestion: Entry")
    if (!is.na(session$userData$test$currentQuestion)) paste0("\n", session$userData$test$currentQuestion, ": ", currentQuestion()$QuestionText)
  })
  
  output$currentAnswers <- renderUI({
    printDebug("currentAnswers: Entry")
    input$submitButton
    # Already answered
    if (currentQuestion() %>% pull(Answered)) {
      t <- currentAnswers() %>% arrange(DisplayIndex)
      x <- lapply(
             1:(t %>% nrow()),
             function(i) {
               c1 <- t %>% slice(i) %>% pull(IndexedAnswerText)
               c2 <- icon(t %>% slice(i) %>% pull(Icon))
               c3 <- ifelse(
                       i %in% session$userData$test$answers[[session$userData$test$currentQuestion]], 
                       ifelse(
                         currentQuestion() %>% pull(AnsweredCorrectly),
                         "tick.png",
                         "cross.png"
                       ), 
                       ""
                     )
               tags$span(c2, c1, tags$img(src=c3), tags$br())
             }
           )
      ui <- tagList(tags$br(), x)
    } else {
      # Not yet answered
      if (currentAnswers() %>% nrow() > 0) {
        if (currentAnswers() %>% filter(Correct == "Y") %>% nrow() > 1) {
          # Checkboxes
          ui <- tagList(
                  checkboxGroupInput(
                    "answerButtons",
                    label="",
                    choiceNames=currentAnswers() %>% pull(IndexedAnswerText),
                    choiceValues=currentAnswers() %>% pull(DisplayIndex),
                    selected=character(0)
                  )
                )
        } else {
          # Radio buttons
          ui <- tagList(
                  radioButtons(
                    "answerButtons",
                    label="",
                    choiceNames=currentAnswers() %>% pull(IndexedAnswerText),
                    choiceValues=currentAnswers() %>% pull(DisplayIndex),
                    selected=character(0)
                  )
                )
        }
      } else {
        ui <- tagList(radioButtons("answerButtons", label="", choiceNames=c(), choiceValues=c(), selected=character(0)))
      }
    }
    ui
  })
  
  output$currentHelpText <- renderUI({
    printDebug("currentHelpText: Entry")
    if (currentQuestion() %>% pull(Answered)) {
      correctAnswers <- currentAnswers() %>% filter(!is.na(Correct)) %>% pull(DisplayIndex)
      result <- ifelse(identical(as.character(correctAnswers), session$userData$test$answers[[session$userData$test$currentQuestion]]), "Correct!", sample(incorrectAnswerStrings, 1))
      t <- currentQuestion() %>% pull(HelpText)
      if (!is.na(t)) tagList(tags$br(), result, tags$br(), tags$br(), t)
      else tagList(tags$br(), result)
    }
  })
  
  output$progress <- renderUI({
    req(currentQuestion())
    
    printDebug("output$progress: Entry")
    h4(
      sprintf(
        "Question %i of %i [Score: %i/%i]", 
        session$userData$test$currentQuestion, 
        input$nQuestions,
        session$userData$test$questions %>% filter(!is.na(AnsweredCorrectly)) %>% summarise(R=n()) %>% pull(R),
        session$userData$test$questions %>% filter(Answered) %>% summarise(N=n()) %>% pull(N)
      )
    )
  })
  
  output$timer <- renderUI({
    printDebug("output$timer: Entry")
    dur <- lubridate::as.period(session$userData$test$elapsedTime)
    durString <- sprintf("Elapsed time: %02i:%02i:%02i", hour(dur), minute(dur), floor(second(dur)))
    if (!is.na(input$timeLimit)) {
      remain <- lubridate::as.period(dminutes(input$timeLimit) - session$userData$test$elapsedTime)
      remainString <- sprintf(" [Time remaining: %02i:%02i:%02i]", hour(remain), minute(remain), ceiling(second(remain)))
      durString <- paste0(durString, remainString)
    }
    if(input$timedTest & session$userData$test$started) wellPanel(durString)
  })
  
  output$navBar <- renderUI({
    printDebug("output$navBar: Entry")
    if (is.na(session$userData$test$currentQuestion)) return()
    tagList(
      actionButton("prevButton", "< Prev"),
      actionButton("submitButton", "Submit"),
      actionButton("nextButton", "Next >")
    )
  })
  
  output$questionPanel <- renderUI({
    printDebug("output$questionPanel: Entry")
    if (!session$userData$test$started) return()
    tagList(
      htmlOutput("progress"),
      uiOutput("currentQuestion"),
      uiOutput("currentAnswers"),
      uiOutput("currentHelpText"),
      uiOutput("navBar")
    )
  })
  
  observeEvent(input$quitButton, {
    resetTest()
  })
}

shinyApp(ui, server)