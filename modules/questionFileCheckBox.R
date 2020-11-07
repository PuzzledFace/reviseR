source("functions.R")

validateFile <- function(f, verbose=FALSE, title=NA) {
  rv <- TRUE
  msg <- c()
  qCols <- c("Category", "SubCategory", "QuestionID", "QuestionText", "HelpText", "Image", "HelpImage")
  aCols <- c("Category", "SubCategory", "QuestionID", "AnswerIndex", "AnswerText", "Correct", "Image")
  
  if (!file.exists(f)) {
    msg <- append(msg, paste0(f, " does not exist"))
    rv <- FALSE
  }
  if (!(tools::file_ext(f) %in% c("xls", "xlsx"))) {
    msg <- append(msg, paste0(f, " is not an Excel workbook [extension is ", tools::file_ext(f), "]"))
    rv <- FALSE
  } 
  sheets <- excel_sheets(f)
  if ("Questions" %in% sheets) {
    x <- read_excel(f, sheet="Questions")
    if (!identical(names(x), qCols)) {
      msg <- append(
        msg, 
        paste0(
          "The Questions tab does not contain the required columns: expected ", 
          paste0(qCols, collapse=", "),
          " but got ", 
          paste0(names(x), collpase=", ")
        )
      )
      rv <- FALSE
    }
  } else {
    rv <- FALSE
    msg <- append(msg, "The Questions worksheet does not exist.")
  }
  if ("Answers" %in% sheets) {
    y <- read_excel(f, sheet="Answers")
    if (exists("y")) if (!identical(names(y), aCols)) {
      msg <- append(
        msg, 
        paste0(
          "The Answers tab does not contain the required columns: expected ", 
          paste0(aCols, collapse=", "),
          " but got ", 
          paste0(names(y), collpase=", ")
        )
      )
      if (verbose & !rv) showModal(modalDialog(title=paste0(f, " is invalid!"), tags$ul(lapply(msg, tags$li))))
      rv <- FALSE
    }
  } else {
    rv <- FALSE
    msg <- append(msg, "The Answers worksheet does not exist.")
  }
  if (verbose & !rv) {
    showModal(
      modalDialog(
        title=ifelse(is.na(title), paste0(basename(f), " is invalid!"), title),
        tagList(lapply(msg, function(x) tags$li(x)))
      )
    )
  }
  return(rv)
}

questionFileCheckBoxUI <- function(id, file) {
  ns <- NS(id)
  
  printDebug("Entry")
  valid <- validateFile(file)
  if (valid) {
    label <- basename(file)
    checkboxInput(ns("file"), label=label, value=valid)
  } else {
    label <- paste0(basename(file), " [Invalid]")
    disabled(checkboxInput(ns("file"), label=label, value=valid))
  }
}

questionFileCheckBoxController <- function(id, file) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      v <- reactiveValues(file=file, valid=validateFile(file))
      
      observeEvent(input$file, {
        printDebug(ns("Entry"))
      },
      label=ns("file"))
      
      data <- reactive({
        if (v$valid) {
          q <- read_excel(v$file, sheet="Questions")
          a <- read_excel(v$file, sheet="Answers")
          list(questions=q, answers=a)
        }
        else list()
      },
      label=ns("data"))
      
      return({data})
    }
  )
}
