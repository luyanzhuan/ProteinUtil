# @Author       : luyz
# @Date         : 2024-08-16 18:30
# @Description  : Custom utility functions for base operations
# Copyright (c) 2024 by luyz && luyz@aptbiotech.com
# Edited detail:
# 2024-08-16 18:30:00 [luyz] - Initial version


# Load required libraries
library(foreach)
# Load custome log functions
source("/ProteinUtil/Base/BaseLog.R")

# ----------------------------------------------- File & Folder -----------------------------------------------

# # Function to create directories if they do not exist.
#
# This function ensures that all directories in the specified path exist.
# If any directory in the path does not exist, it will be created.
#
# Parameters:
#   path (string): The full file path including directories and filename.
#
# Returns:
#   None
#' Function to create directories if they do not exist.
#'
#' @param path (string): The full file path including directories and filename.
#' @return None
#' @examples
#' CreateDirs("/path/to/file")
CreateDirsByFilePath <- function(filePath) {
    # Get the directory portion of the filePath (removing the filename)
    dirPath <- dirname(filePath)

    # If the directory does not exist, create it
    if (!dir.exists(dirPath)) {
        dir.create(dirPath, recursive = TRUE)
    }
}

#' # Function to create directories by a nested list structure.
#'
#' This function creates directories based on a nested list structure.
#' Each element in the list represents a folder, and subfolders are represented by nested lists.
#' The function recursively creates folders according to the nested structure.
#'
#' Parameters:
#'  basePath (string): The base path where the folder structure will be created.
#'  folderList (list): A nested list representing the folder structure.
#'
#' Returns:
#'  None
#'
#' @param basePath (string): The base path where the folder structure will be created.
#' @param folderList (list): A nested list representing the folder structure.
#' @return None
#' @examples
#' CreateNestedFolders("/path/to/base", list(folder1 = list(subfolder1 = NULL, subfolder2 = NULL), folder2 = NULL))
CreateNestedFolders <- function(basePath, folderList, logObject = NULL) {
    # Init log object
    if (is.null(logObject)) {
        logObject <- GetLogger("_CreateNestedFolders.log", consoleOutput = TRUE, fileOutput = TRUE)
    }

    # Recursive function to create folders
    CreateFolders <- function(basePath, folderList) {
        for (folderName in names(folderList)) {
            currentPath <- file.path(basePath, folderName)

            # Check if the folder already exists
            if (!dir.exists(currentPath)) {
                dir.create(currentPath, showWarnings = FALSE, recursive = TRUE)
                logObject$Info(paste("创建文件夹：", currentPath))
            } else {
                logObject$Warning(paste("文件夹已存在：", currentPath))
            }

            # If the folderList element is a list, call the function recursively
            if (is.list(folderList[[folderName]])) {
                CreateFolders(currentPath, folderList[[folderName]])
            }
        }
    }

    # Error handling: Check if the base path is valid
    if (!is.character(basePath) || length(basePath) != 1) {
        logObject$Error("无效的基础路径。请提供一个有效的字符串作为基础路径。")
    }

    # Error handling: Check if the folder list is a list
    if (!is.list(folderList)) {
        logObject$Error("无效的文件夹列表。请提供一个有效的嵌套列表作为文件夹结构。")
    }

    # Create base folder if it doesn't exist
    if (!dir.exists(basePath)) {
        dir.create(basePath, showWarnings = FALSE, recursive = TRUE)
        logObject$Info(paste("创建基础文件夹：", basePath))
    } else {
        logObject$Warning(paste("基础文件夹已存在:", basePath))
    }

    # Call the recursive function
    CreateFolders(basePath, folderList)
}


# ----------------------------------------------- DataFrame -----------------------------------------------

#' #Function to read a data frame from a file
#'
#' This function reads a data frame from a file, handling different file formats such as Excel, CSV, and TSV.
#' It uses the readxl and readr packages to read Excel and text files, respectively.
#' If the file format is not recognized, it attempts to read the file as a CSV or TSV file using the readr package.
#' The function also handles Unicode encoding issues by converting the file to UTF-8 using a Bash script.
#' @param filePath The path to the file to read
#' @param delim The delimiter used in the file (default is NULL)
#' @param colNames Whether the file has column names (default is TRUE)
#' @param coverFileUnicodeScriptFilePath The path to the Bash script for converting file encoding
#' @return The data frame read from the file
#' @examples
#' ReadDataFrame("data.xlsx")
ReadDataFrame <- function(filePath, delim = NULL, colNames = TRUE, coverFileUnicodeScriptFilePath = "/root/ppbria/code/Tool/CovertFileUnicode.sh") {
    data <- NULL
    fileExt <- toupper(tools::file_ext(filePath))

    TryReadDelim <- function(filePath) {
        system(paste("sh", coverFileUnicodeScriptFilePath, filePath), ignore.stderr = TRUE)
        return(suppressMessages(readr::read_delim(filePath, delim = ifelse(is.null(delim), NULL, delim), col_names = colNames, guess_max = 10000)))
    }

    if (fileExt %in% c("XLSX", "XLS")) {
        # Attempt to read Excel file
        tryCatch(
            {
                data <- readxl::read_excel(filePath, col_names = colNames)
            },
            error = function(e) {
                # If Excel reading fails, attempt to read as CSV or TSV
                data <- TryReadDelim(filePath)
            }
        )
    } else {
        # For CSV and other text files, use readr functions
        data <- TryReadDelim(filePath)
    }

    return(data)
}



# ----------------------------------------------- Project -----------------------------------------------
#' #Function to get the group compare infomation
#'
#' This function reads the group compare information from the groupvs file.
#' It extracts the compare type, groups, folder name, and sheet names from the file.
#' The compare type is determined based on the compare column in the file.
#' The groupvs column is split into a list of groups for comparison.
#' The folder name is generated from the compare column.
#' The sheet names are generated based on the compare column.
#' @param groupvsFilePath The path to the groupvs file
#' @return A data frame containing the group compare information
#' @examples
#' GetCompareInfo("groupvs.xlsx")
GetCompareInfo <- function(groupvsFilePath) {
    compareInfo <- ReadDataFrame(groupvsFilePath)
    if (nrow(compareInfo) != 0) {
        compareInfo <- compareInfo |>
            dplyr::rename(compare = 1) |>
            dplyr::mutate(
                # 判断比较类型
                compareType = dplyr::if_else(grepl("oneway", compare), "oneway", "t.test"),
                # 比较组 list
                groupvs = strsplit(stringr::str_remove(compare, "_oneway"), "_vs_|\\_", perl = TRUE),
                # 生成结果文件夹名
                folderName = compare,
                # 鉴于xlsx的sheet名不能超过31个字符，这里进行了处理。
                sheetnames = dplyr::if_else(nchar(compare) < 31, compare, sprintf("%s.%s", dplyr::row_number(), stringr::str_sub(compare, end = 30 - nchar(dplyr::row_number())))),
            )
    }
    return(compareInfo)
}

#' #Function to retrieve a specific parameter value from an Excel file
#'
#' This function reads an Excel file containing parameter values and retrieves the value associated with a specified code.
#' It changes the working directory to the analysis path, checks for the existence of the Excel file,
#' reads the file, extracts the desired parameter value, and then restores the original working directory.
#' If the file does not exist, it stops with an error message.
#' @param parametersFilePath The path to the Excel file containing parameters.
#' @return The list of parameters with code as the key and value as the value.
#' @examples
#' # Set the path to the parameters Excel file
#' parametersFilePath <- "path/to/parameters.xlsx"
#'
#' # Get the parameter value
#' parameter <- GetParameters(parametersFilePath)
#' print(parameter)
GetParametersList <- function(parametersFilePath) {
    parameterDF <- ReadDataFrame(parametersFilePath)

    parameterList <- setNames(parameterDF$value, parameterDF$code)

    return(parameterList)
}
