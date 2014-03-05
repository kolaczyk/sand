
.sand_cursor <- new.env()
.sand_cursor$chapter <- 2
.sand_cursor$chunk <- 1

.onAttach <- function(library, pkg) {
    unlockBinding(".sand_cursor", asNamespace("sand"))
    packageStartupMessage("\nStatistical Analysis of Network Data with R\n",
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

N <- structure(0, class="sand_next")
P <- structure(0, class="sand_print")

set_chapter <- function(which) {
  if (!is.numeric(which) || which %% 1 != 0 || which < 2 || which > 10) {
    stop("Invalid chapter, must be integer from 2 to 10")
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

  ## Find end of chunk
  to <- which(grepl(sprintf("^# CHUNK %d$", chunk+1), lines))[1]
  if (!is.na(to)) { lines <- lines[1:(to-1)] }

  ## Cut out output
  if (!output) {
    borders <- which(grepl("^# ---$", lines))
    if (length(borders) != 0) {
      bm <- matrix(borders, ncol=2, byrow=TRUE)
      lines <- lines[-unlist(apply(bm, 1, function(x) seq(x[1], x[2])))]
    }
  }

  lines
}

.run_chunk <- function(chapter, chunk) {
  code <- .get_chunk(chapter, chunk)
  if (is.null(code)) { return() }
  code <- paste(code, collapse="\n")
  cat(sep="",
      colourise(paste0("<<< ", chapter, ".", chunk, "\n"), fg="red"),
      colourise(code, fg="light green"))
  expr <- parse(text=code)

  out <- capture.output(eval(expr, envir=.GlobalEnv))
  if (!is.null(out) && length(out) != 0 && out != "") {
    cat(sep="", colourise(">>>\n", fg="red"))
    cat(sep="\n", colourise(out, fg="blue"))
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
  code <- .get_chunk(chapter, chunk)
  if (is.null(code)) { return() }
  code <- paste(code, collapse="\n")
  cat(sep="",
      colourise(paste0("=== ", chapter, ".", chunk, "\n"), fg="red"),
      colourise(code, fg="green"))
}

install_sand_packages <- function() {
  pkgs <- strsplit(packageDescription("sand")$Suggests, "[, \n]+")[[1]]
  source("http://bioconductor.org/biocLite.R")
  do.call("biocLite", list(pkgs, suppressUpdates=TRUE,
                           suppressAutoUpdate=TRUE))
}
