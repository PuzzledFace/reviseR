source("functions.R")

pluralise <- function(s, n, defaultIrregular=list("is"="are", "has"="have"), irregular=list()) {
  if (n == 1) return(s)
  irregular <- c(irregular, defaultIrregular)
  if (str_trim(s) %in% names(irregular)) return(stringi::stri_replace_first_fixed(s, str_trim(s), irregular[[str_trim(s)]]))
  else return(paste0(s, "s"))
}

prettyList <- function(x, prefix="row", suffix=NA, pluraliseSuffix=TRUE) {
  n <- length(x)
  if (n == 0) return("")
  if (n > 1) prefix <- pluralise(prefix, n)
  s <- paste0(x, collapse=", ")
  s <- paste0(prefix, " ", s)
  s <- stringi::stri_replace_last_fixed(s, ",", " and")
  if (!is.na(suffix)) {
    if (pluraliseSuffix) suffix <- pluralise(suffix, n)
    s <- paste0(s, suffix)
  }
  return(s)
}

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
  if (rv) {
    # Validate questions
    x <- x %>% mutate(RowNumber=row_number())
    t <- x %>% filter(is.na(QuestionID))
    if (t %>% nrow() > 0) {
      rv <- FALSE
      msg <- append(msg, sprintf("QuestionID is missing in rows %s of the Questions worksheet", prettyList(t %>% pull(RowNumber))))
    }
    t <- x %>% filter(!is.na(QuestionID)) %>% group_by(QuestionID) %>% summarise(N=n(), .groups="drop") %>% filter(N > 1)
    if (t %>% nrow() > 0) {
      rv <- FALSE
      msg <- append(msg, sprintf("%s duplicated in the Questions worksheet", prettyList(t %>% pull(QuestionID), prefix="QuestionID", suffix=" is")))
    }
    t <- x %>% filter(trimws(QuestionText) == "")
    if (t %>% nrow() > 0) {
      rv <- FALSE
      msg <- append(msg, sprintf("QuestionText is blank in rows %s of the Questions worksheet", prettyList(t %>% pull(RowNumber))))
    }
    y <- y %>% mutate(RowNumber=row_number())
    t <- y %>% filter(is.na(QuestionID))
    if (t %>% nrow() > 0) {
      rv <- FALSE
      msg <- append(msg, sprintf("QuestionID is missing in rows %s of the Answer worksheet", prettyList(t %>% pull(RowNumber))))
    }
    t <- y %>% group_by(QuestionID) %>%  summarise(N=sum(isTruthy(Correct)), .groups="drop")
    if (t %>% nrow() == 0) {
      rv <- FALSE
      msg <- append(msg, sprintf("%s no correct answer in the Answers worksheet", prettyList(t %>% pull(QuestionID), prefix="QuestionID", suffix=" has")))
    }
    t <- y %>% filter(trimws(AnswerText) == "")
    if (t %>% nrow() > 0) {
      rv <- FALSE
      msg <- append(msg, sprintf("AnswerText is blank in rows %s of the Answers worksheet", prettyList(t %>% pull(RowNumber))))
    }
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
