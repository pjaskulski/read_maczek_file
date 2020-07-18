# Diagram Czekanowskiego w R!
# Dzięki bibliotece RMaCzek (Albin Vasterlund, Krzysztof Bartoszek).
# https://cran.r-project.org/package=RMaCzek

library(RMaCzek)

# read_mdf_file - function to load data from an mdt file (MaCzek 3.3 - 
# http://www.antropologia.uw.edu.pl/MaCzek/maczek.html)
# Parameters: filepath - path to file *.mdt
# Output: data.frame

read_mdf_file <- function(filepath) {
  con <- file(filepath, "r")
  num_line <- 1
  start_rl <- FALSE
  start_vl <- FALSE
  start_data <- FALSE
  rlabel <- c()
  vlabel <- c()
  tmp <- tempfile()
  
  while ( TRUE ) {
    line <- readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    line <- trimws(line)
    if (num_line == 1 && line != 'MaCzek DATA 3.3') {
      close(con)
      stop("Error: This is not MaCzek DATA ver. 3.3 file!")
    }
    num_line <- num_line + 1
    if (line == '[RECORD_LABEL]') {
      start_rl <- TRUE
    }
    else if (line == '[VARIABLE_LABEL]') {
      start_vl <- TRUE
      start_rl <- FALSE
    }
    else if (line == '[DATA]') {
      start_data <- TRUE
      start_vl <- FALSE
    }
    else {
      if (start_rl && line != '') {
        rlabel <- append(rlabel, line)      
      }
      else if (start_vl && line != '') {
        vlabel <- append(vlabel, line)
      }
      else if (start_data && line != '') {
        cat(line, file = tmp, sep = '\n', append = TRUE)      
      }
    }
  }
  close(con)
  
  mdt <- read.csv(tmp, header = FALSE, sep = ';')
  file.remove(tmp)
  rownames(mdt) <- rlabel
  colnames(mdt) <- vlabel
  
  return(mdt)
}

test <- read_mdf_file('slabosz.mdt')
res <- czek_matrix(test)
plot.czek_matrix(res)
