# Copyright (C) 2018  Jochen Weile, Roth Lab
#
# This file is part of YogiLog.
#
# YogiLog is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# YogiLog is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with YogiLog.  If not, see <https://www.gnu.org/licenses/>.


#' new logger
#' 
#' Constructs a new logger object. Allows for writing to log files and stdout. 
#' Entries are tagged by time stamps and event type.
#' 
#' @param logfile the name of the log file
#' @param stdout logical determining whether the logger should also write to standard out.
#' @return a new logger object with the following methods:
#' \describe{
#'   \item{info(...)}{writes an info message to the log file}
#'   \item{warn(...)}{writes an warning message to the log file}
#'   \item{err(...)}{writes an error message to the log file}
#' }
#' @export
#' @examples
#' logger <- new.logger("mylogfile.log")
#' logger$info("This is a test log entry!")
#' 
new.logger <- function(logfile,stdout=TRUE,useColor=TRUE) {
	.logfile <- logfile

	if (!file.exists(.logfile)) {
		file.create(.logfile)
	}
	if (file.access(.logfile, mode=2) < 0) {
		stop("Logfile cannot be written!")
	}

	supported <- c("xterm-color","xterm-256color", "screen", "screen-256color")
	useColor <- useColor && (Sys.getenv()[["TERM"]] %in% supported)

	red <- "\033[0;31m"
	yellow <- "\033[1;33m"
	nocol <- "\033[0m"

	.log <- function(type,...) {
		timestamp <- format(Sys.time(),format='%Y-%m-%d_%H:%M:%S')
		line <- paste(type,...)
		if (stdout) {
			if (useColor && type == "WARNING:") {
				cat(timestamp,paste0(yellow,line,nocol),"\n")
			} else if (useColor && type == "FATAL:") {
				cat(timestamp,paste0(red,line,nocol),"\n")
			} else {
				cat(timestamp,line,"\n")
			}
		}
		tryCatch({
			.con <- file(.logfile,open="a")
			writeLines(paste(timestamp,line), .con)
		},
		error = function(ex) {
			stop("Unable to write log file; ",ex,"\n")
		},
		finally = {
			if (exists(".con") && isOpen(.con)) {
				close(.con)
			}
		})
	}

	info <- function(...) .log("INFO:",...)
	warn <- function(...) .log("WARNING:",...)
	err <- function(...) .log("FATAL:",...)

	structure(
		list(
			info=info,
			warn=warn,
			warning=warn,
			err=err,
			error=err,
			fatal=err
		),class="yogilogger"
	)
}




#' register log-enabled error handler
#' 
#' Registers a top-level error handler that makes a log entry and exiting with an error status
#' 
#' When an error occurs, the handler will catch it, make a log entry detailing the error message
#' and stack trace, and then exit with an error status (unless overridden with die=FALSE)
#' 
#' @param logger a yogilogger object to be used for logging any fatal errors
#' @param die a logical value indicationg whether the process should exit with an error status
#'    upon handling the error. Defaults to TRUE
#' @export
#' @examples
#' 
registerLogErrorHandler <- function(logger,die=TRUE) {
	#make sure it's a valid logger
	stopifnot(inherits(logger,"yogilogger"))
	#define handler
	handler <- function() {
		tryCatch({
			logger$err(
				geterrmessage(),"\n",
				paste(.traceback(),collapse="\n\t--> ")
			)
		},finally={
			if (die) {
				quit(save="no",status=1)
			}
		})
	}
	#register handler
	options(
		error=handler
	)
}
