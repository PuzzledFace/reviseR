questionFilesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(
      ns("fileUpload"),
      label="Select one or more files:",
      multiple=TRUE,
      accept=c(".xlsx", ".xls")
    ),
    uiOutput(ns("boxes"))
  )
}

questionFilesController <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    
      # Initialise
      v <- reactiveValues(files=list())
      observe({
        v$files <- list.files("./www", "*\\.[xlsx|xls]", full.names=TRUE)
      })

      observeEvent(input$fileUpload, {
        if (validateFile(input$fileUpload$datapath, TRUE, title=paste0(input$fileUpload$name, " is invalid!"))) {
          x <- file.copy(input$fileUpload$datapath, paste0("./www/", input$fileUpload$name), overwrite=TRUE)
          v$files <- append(v$files, paste0("./www/", input$fileUpload$name))
          if (x) showModal(modalDialog(title="Information", paste0(input$fileUpload$name), " successfully uploaded."))
          else showModal(modalDialog(title="Error!", paste0("Unable to upload ", input$fileUpload$name)))
        }
      })
      
      controllers <- reactive({
        lapply(v$files, function(x) questionFileCheckBoxController(id=ns(tools::file_path_sans_ext(basename(x))), file=x))
      })
      
      observeEvent(controllers(), {
        q <- bind_rows(lapply(controllers(), function(x) x()$questions))
        a <- bind_rows(lapply(controllers(), function(x) x()$answers))
        list(questions=q, answers=a)
      },
      label="controllers")

      output$boxes <- renderUI({
        input$fileUpload
        ui <- wellPanel("Select question files to use:")
        lapply(
          v$files, 
          function(z) {
            ui <<- tagAppendChild(ui, questionFileCheckBoxUI(id=ns(tools::file_path_sans_ext(basename(z))), file=z))
          }
        )
        ui
      })
      
      return({controllers})
    }
  )
}