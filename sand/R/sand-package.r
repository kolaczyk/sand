
.sand_cursor <- new.env()
.sand_cursor$chapter <- 2
.sand_cursor$chunk <- 1

.onAttach <- function(library, pkg) {
    unlockBinding(".sand_cursor", asNamespace("sand"))
    packageStartupMessage("\nStatistical Analysis of Network Data with R, 2nd Edition\n",
                          "Type in C2 (+ENTER) to start with Chapter 2.")
    invisible()
}

C2 <- structure(2, class="sand_chapter")
C3 <- structure(3, class="sand_chapter")
C4 <- structure(4, class="sand_chapter")
C5 <- structure(5, class="sand_chapter")
C6 <- structure(6, class="sand_chapter")
C7 <- structure(7, class="sand_chapter")
C8 <- structure(8, class="sand_chapter")
C9 <- structure(9, class="sand_chapter")
C10 <- structure(10, class="sand_chapter")
C11 <- structure(11, class="sand_chapter")

N <- structure(0, class="sand_next")
P <- structure(0, class="sand_print")

set_chapter <- function(which) {
  if (!is.numeric(which) || which %% 1 != 0 || which < 2 || which > 11) {
    stop("Invalid chapter, must be integer from 2 to 11")
  }

  ## Remove all objects
  rm(list=ls(envir=.GlobalEnv), envir=.GlobalEnv)

  ## Try to unload all packages
  pkgs <- names(sessionInfo()$otherPkgs)
  pkgs <- setdiff(pkgs, c("sand", "igraph", "igraphdata"))
  if (length(pkgs) != 0) {
    pkgs <- paste('package:', pkgs, sep="")
    lapply(pkgs, detach, character.only=TRUE, unload=TRUE, force=TRUE)
  }

  ## Set chapter, on the first chunk
  cat(colourise(paste0("SAND Chapter ", which,
                       ", press N (+ENTER) to run the next code chunk"),
                fg="red"), "\n")
  .sand_set(chapter=which, chunk=1)
}

.sand_set <- function(chapter, chunk) {
  assign("chapter", chapter, envir=.sand_cursor)
  assign("chunk", chunk, envir=.sand_cursor)
}

.get_chunk <- function(chapter, chunk, output=FALSE) {
  cf <- system.file(sprintf("code/chapter%d.R", chapter), package="sand")
  lines <- readLines(cf)

  ## Find beginning of chunk
  from <- which(grepl(sprintf("^# CHUNK %d$", chunk), lines))[1] + 1
  if (is.na(from)) {
    message("No more code chunks in Chapter ", chapter, ".")
    return(NULL)
  }
  lines <- lines[from:length(lines)]
  indx <- vector() 

  ## Find end of chunk
  to <- which(grepl(sprintf("^# CHUNK %d$", chunk+1), lines))[1]
  if (!is.na(to)) { lines <- lines[1:(to-1)] }

  ## Cut out output
  if (!output) {
    borders <- which(grepl("^# ---$", lines))
    if (length(borders) != 0) {
      bm <- matrix(borders, ncol=2, byrow=TRUE)
      indx <- c(1:length(lines))[-unlist(apply(bm, 1, function(x) seq(x[1], x[2])))] 
      lines <- lines[-unlist(apply(bm, 1, function(x) seq(x[1], x[2])))]
    }
  }
  list(lines, indx) 
}

.run_chunk <- function(chapter, chunk) {
  tmp <- .get_chunk(chapter, chunk) 
  code <- tmp[[1]]  
  indx <- tmp[[2]] 
  indx <- indx[1:length(indx)-1] 

  if (is.null(code)) { return() }
  cat(sep="",colourise(paste0("<<< ", chapter, ".", chunk, "\n"), fg="red")) 
  expr <- parse(text=code)
  len <- length(expr) 
  
  ## Special cases
  if(chapter == 4 && chunk == 18){
    cat(sep="", colourise(paste(code[1:5], collapse="\n"), fg="light blue")) 
    out <- capture.output(eval(expr[1:3], envir=.GlobalEnv))
    cat(sep="", colourise("\n>>>\n", fg="red"))
    cat(sep="\n", colourise(out, fg="cyan"))
    cat(sep="", colourise(paste(code[6], collapse="\n"), fg="light blue")) 
    out <- capture.output(eval(expr[4], envir=.GlobalEnv))
    cat(sep="", colourise("\n>>>\n", fg="red"))
    cat(sep="\n", colourise(out, fg="cyan"))
    cat(sep="", colourise(paste(code[7], collapse="\n"), fg="light blue")) 
    out <- capture.output(eval(expr[5], envir=.GlobalEnv))
    cat(sep="", colourise("\n>>>\n", fg="red"))
    cat(sep="\n", colourise(out, fg="cyan"))
    return()
  }
  if(chapter == 6 && chunk == 20){
    cat(sep="", colourise(paste(code[1:11], collapse="\n"), fg="light blue"))
    out <- capture.output(eval(expr[1:5], envir=.GlobalEnv)) 
    cat(sep="", colourise("\n>>>\n", fg="red"))
    cat(sep="\n", colourise(out, fg="cyan"))
    cat(sep="", colourise(paste(code[12], collapse="\n"), fg="light blue"))
    out <- capture.output(eval(expr[6], envir=.GlobalEnv)) 
    cat(sep="", colourise("\n>>>\n", fg="red"))
    cat(sep="\n", colourise(out, fg="cyan"))
    return()
  }
  
  ## If no output, run code and return
  if(length(indx) == 0){ 
    cat(sep="", colourise(paste(code, collapse="\n"), fg="light blue")) 
    eval(expr, envir=.GlobalEnv)
    return() 
    }
  
  ## If only one line of code, run code and return 
  if(len == 1){
    cat(sep="", colourise(paste(code, collapse="\n"), fg="light blue")) 
    out <- capture.output(eval(expr, envir=.GlobalEnv)) 
    if (!is.null(out) && length(out) != 0) { 
    cat(sep="", colourise(">>>\n", fg="red"))
    cat(sep="\n", colourise(out, fg="cyan"))
    }
    return()
  }
  
  prevInd = 0 
  rep = TRUE  
  while(rep){
    for(i in (prevInd+1):(length(indx)-1)){
      if(i == length(indx) && length(indx) == len){  
        cat(sep="", colourise(paste(code[(prevInd+1):i], collapse="\n"), fg="light blue")) 
        temp_expr <- expr[(prevInd+1):i]
        out <- capture.output(eval(temp_expr, envir=.GlobalEnv))
        if (!is.null(out) && length(out) != 0) { 
          cat(sep="", colourise("\n>>>\n", fg="red")) 
          cat(sep="\n", colourise(out, fg="cyan"))
        }
        rep = FALSE
        break 
      }
      ## Check for jump ie multiple outputs
      if((indx[i]+1) != indx[i+1]){
        cat(sep="", colourise(paste(code[(prevInd+1):i], collapse="\n"), fg="light blue")) 
        temp_expr <- expr[(prevInd+1):i]
        out <- capture.output(eval(temp_expr, envir=.GlobalEnv))
        if (!is.null(out) && length(out) != 0) { 
          cat(sep="", colourise("\n>>>\n", fg="red")) 
          cat(sep="\n", colourise(out, fg="cyan"))
        }
        prevInd <- i
        if(prevInd == len){
          rep = FALSE
          break 
        }
        break 
      }
      ## No jump found
      if((i == len - 1 && length(indx) == len)){ 
        cat(sep="", colourise(paste(code[(prevInd+1):(i+1)], collapse="\n"), fg="light blue")) 
        temp_expr <- expr[(prevInd+1):(i+1)] 
        out <- capture.output(eval(temp_expr, envir=.GlobalEnv))
        if (!is.null(out) && length(out) != 0) { 
          cat(sep="", colourise("\n>>>\n", fg="red")) 
          cat(sep="\n", colourise(out, fg="cyan"))
        }
        rep = FALSE
        break 
      } 
      else if(i == length(indx) - 1 && length(indx) != len){
        cat(sep="", colourise(paste(code[(prevInd+1):(length(code)-1)], collapse="\n"), fg="light blue"))
        temp_expr <- expr[(prevInd+1):(len)] 
        out <- capture.output(eval(temp_expr, envir=.GlobalEnv))
        if (!is.null(out) && length(out) != 0) { 
          cat(sep="", colourise("\n>>>\n", fg="red")) 
          cat(sep="\n", colourise(out, fg="cyan"))
        }
        rep = FALSE
        break
      }
    }
  }
}

print.sand_chapter <- function(x, ...) {
  set_chapter(unclass(x))
}

print.sand_next <- function(x, ...) {
  repeat {
    .run_chunk(.sand_cursor$chapter, .sand_cursor$chunk)
    .sand_set(.sand_cursor$chapter, .sand_cursor$chunk+1)
    x <- x - 1
    if (x < 0) { break }
  }
}

print.sand_print <- function(x, ...) {
  chapter <- .sand_cursor$chapter
  chunk <- .sand_cursor$chunk + x
  if (chunk < 1) { stop("The first chunk is chunk 1") }
  tmp <- .get_chunk(chapter, chunk) 
  code <- tmp[[1]] 
  if (is.null(code)) { return() }
  code <- paste(code, collapse="\n")
  cat(sep="",
      colourise(paste0("=== ", chapter, ".", chunk, "\n"), fg="red"),
      colourise(code, fg="blue"))
}

install_sand_packages <- function() {
  pkgs <- strsplit(packageDescription("sand")$Suggests, "[, \n]+")[[1]]
  source("http://bioconductor.org/biocLite.R")
  do.call("biocLite", list(pkgs, suppressUpdates=TRUE,
                           suppressAutoUpdate=TRUE))
}
