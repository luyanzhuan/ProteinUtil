# @Author       : luyz
# @Date         : 2024-08-16 18:30
# @Description  : Custom logger class for logging messages to a file and/or console
# Copyright (c) 2024 by luyz && luyz@aptbiotech.com
# Edited detail:
# 2024-08-16 18:30:00 [luyz] - Initial version


# Logger class definition using Reference Classes (setRefClass)
# 
# This class provides logging functionality, allowing messages to be logged to a file and/or the console.
# It supports different log levels, including INFO, WARNING, ERROR, DEBUG, and custom levels.
# The Logger class also supports a singleton pattern to ensure only one instance is used throughout the application.

Logger <- setRefClass(
  "Logger",
  fields = list(
    logFile = "character",         # Path to the log file where messages will be written
    consoleOutput = "logical",     # Flag to enable/disable console output of log messages
    fileOutput = "logical"         # Flag to enable/disable writing log messages to a file
  ),
  methods = list(
    
    # Initialize the Logger instance
    # 
    # This method initializes the Logger with the specified log file, and whether to enable
    # console output and/or file output.
    #
    # Parameters:
    #   logFile (character): The path to the log file. Default is "log.txt".
    #   consoleOutput (logical): Whether to print log messages to the console. Default is TRUE.
    #   fileOutput (logical): Whether to write log messages to a file. Default is TRUE.
    #
    # Returns:
    #   None
    #' Initialize the Logger instance
    #'
    #' @param logFile (character): The path to the log file. Default is "log.txt".
    #' @param consoleOutput (logical): Whether to print log messages to the console. Default is TRUE.
    #' @param fileOutput (logical): Whether to write log messages to a file. Default is TRUE.
    #' @return None
    #' @examples
    #' logger <- Logger$new(logFile = "logfile.txt", consoleOutput = TRUE, fileOutput = TRUE)
    initialize = function(logFile = "log.txt", consoleOutput = TRUE, fileOutput = TRUE) {
      .self$logFile <- logFile
      .self$consoleOutput <- consoleOutput
      .self$fileOutput <- fileOutput
    },
    
    # Log a message with a specified type
    # 
    # This method logs a message with a specified type (e.g., INFO, WARNING, ERROR).
    # The message is timestamped and can be output to the console and/or a log file.
    #
    # Parameters:
    #   type (character): The type of log message (e.g., "INFO", "ERROR").
    #   message (character): The log message to record.
    #
    # Returns:
    #   None
    #' Log a message with a specified type
    #'
    #' @param type (character): The type of log message (e.g., "INFO", "ERROR").
    #' @param message (character): The log message to record.
    #' @return None
    #' @examples
    #' logger$LogMessage("INFO", "This is an info message.")
    LogMessage = function(type, message) {
      # Get current time
      time <- base::format(base::Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      # Construct log message
      logEntry <- glue::glue("[{time}] [{type}] - {message}")
      
      # Print log message to console if enabled
      if (.self$consoleOutput) {
        base::cat(logEntry, "\n")
      }
      
      # Write log message to file if enabled
      if (.self$fileOutput) {
        base::write(logEntry, file = .self$logFile, append = TRUE)
      }
    },
    
    # Log an informational message
    # 
    # This method logs a message with the type "INFO".
    #
    # Parameters:
    #   message (character): The informational message to log.
    #
    # Returns:
    #   None
    #' Log an informational message
    #'
    #' @param message (character): The informational message to log.
    #' @return None
    #' @examples
    #' logger$Info("This is an info message.")
    Info = function(message) {
      .self$LogMessage("INFO", message)
    },
    
    # Log a warning message
    # 
    # This method logs a message with the type "WARNING".
    #
    # Parameters:
    #   message (character): The warning message to log.
    #
    # Returns:
    #   None
    #' Log a warning message
    #'
    #' @param message (character): The warning message to log.
    #' @return None
    #' @examples
    #' logger$Warning("This is a warning message.")
    Warning = function(message) {
      .self$LogMessage("WARNING", message)
    },
    
    # Log an error message and stop execution
    # 
    # This method logs a message with the type "ERROR" and then stops the execution.
    #
    # Parameters:
    #   message (character): The error message to log.
    #
    # Returns:
    #   None (Execution is stopped after logging the error)
    #' Log an error message and stop execution
    #'
    #' @param message (character): The error message to log.
    #' @return None (Execution is stopped after logging the error)
    #' @examples
    #' logger$Error("This is an error message.")
    Error = function(message) {
      .self$LogMessage("ERROR", message)
      base::stop(message)
    },
    
    # Log a custom type message
    # 
    # This method logs a message with a custom type specified by the user.
    #
    # Parameters:
    #   type (character): The custom log type.
    #   message (character): The message to log.
    #
    # Returns:
    #   None
    #' Log a custom type message
    #'
    #' @param type (character): The custom log type.
    #' @param message (character): The message to log.
    #' @return None
    #' @examples
    #' logger$Custom("CUSTOM_TYPE", "This is a custom message.")
    Custom = function(type, message) {
      .self$LogMessage(type, message)
    },
    
    # Log a debug message with multiple arguments
    # 
    # This method logs a message with the type "DEBUG". It can accept multiple
    # arguments and converts them into a string format for logging.
    #
    # Parameters:
    #   ... : Multiple arguments to log as part of the debug message.
    #
    # Returns:
    #   None
    #' Log a debug message with multiple arguments
    #'
    #' @param ... : Multiple arguments to log as part of the debug message.
    #' @return None
    #' @examples
    #' logger$Debug("Debug message with multiple arguments:", var1, var2)
    Debug = function(...) {
      # Capture all arguments and convert to string
      args <- list(...)
      message <- sapply(args, function(arg) {
        if (is.character(arg)) {
          arg
        } else {
          paste(capture.output(str(arg)), collapse = "\n")
        }
      }, USE.NAMES = FALSE)
      
      # Concatenate all messages
      message <- paste(message, collapse = "\n")
      
      .self$LogMessage("DEBUG", message)
    }
  )
)

# Singleton pattern to ensure only one Logger instance
# 
# This function ensures that only one instance of the Logger class is created and used throughout the application.
# If an instance already exists, it returns the existing instance; otherwise, it creates a new instance.
#
# Parameters:
#   logFile (character): The path to the log file. Default is "log.txt".
#   consoleOutput (logical): Whether to print log messages to the console. Default is TRUE.
#   fileOutput (logical): Whether to write log messages to a file. Default is TRUE.
#
# Returns:
#   Logger instance
#' Singleton pattern to ensure only one Logger instance
#'
#' @param logFile (character): The path to the log file. Default is "log.txt".
#' @param consoleOutput (logical): Whether to print log messages to the console. Default is TRUE.
#' @param fileOutput (logical): Whether to write log messages to a file. Default is TRUE.
#' @return Logger instance
#' @examples
#' logger <- GetLogger(logFile = "logfile.txt", consoleOutput = TRUE, fileOutput = TRUE)
GetLogger <- local({
  instance <- NULL
  function(logFile = "log.txt", consoleOutput = TRUE, fileOutput = TRUE) {
    if (base::is.null(instance)) {
      instance <<- Logger$new(logFile, consoleOutput, fileOutput)
    }
    instance
  }
})
