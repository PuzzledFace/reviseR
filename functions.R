# Based on https://stackoverflow.com/questions/7307987/logging-current-function-name
printDebug <- function(..., callstack=sys.calls(), debug=FALSE) {
  if (getOption("reviseR.debug", FALSE) != TRUE) return()
  cs <- clean_cs(callstack)
  if (debug) print(cs)
  x <- stringr::str_match(cs, pattern="\\|([\\w\\s\\d:\\-\\[\\]\\<\\> \\$]+)$")
  if (is.matrix(x)) {
    d <- dim(x)
    if (d[1] == 1 && d[2] == 2) {
      if (is.na(x[1, 2])) {
        print("")
        print(x)
        print(d)
        print("")
        t <- cs
      }
      else t <- x[1, 2]
    }
    else t <- x
  }
  else t <- "* Unknown *"
  print(paste0(t, ": ", ...))
}

# Based on https://stackoverflow.com/questions/7307987/logging-current-function-name
clean_cs <- function(x){
  val <- sapply(x, function(xt){
    z <- strsplit(paste(xt, collapse="\t"), "\t")[[1]]
    switch(z[1],
           "lapply" = z[3], 
           "sapply" = z[3],
           "do.call" = z[2], 
           "function" = "FUN",
           "source" = "###",
           "eval.with.vis" = "###",
           z[1]
    )
  })
  val[grepl("\\<function\\>", val)] <- "FUN"
  val <- val[!grepl("(###|FUN)", val)]
  val <- head(val, -1)
  paste(val, collapse="|")
}

#Source all files in a given folder
sourceFolder <- function(folder) {
  printDebug("Entry")
  sapply(
    list.files(folder, full.names=TRUE),
    function(f) {
      print(paste0("Loading ", f, "..."))
      source(f)
    }
  )  
}
