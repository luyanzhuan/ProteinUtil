# @Author       : luyz
# @Date         : 2024-08-16 18:30
# @Description  : Custom Venn diagram plotting functions
# Copyright (c) 2024 by luyz && luyz@aptbiotech.com
# Edited detail:
# 2024-08-16 18:30:00 [luyz] - Initial version

# load custome code
source("/ProteinUtil/Base/BaseLog.R")
source("/ProteinUtil/Plot/PlotBase.R")
# load custome ggvenn r package
install.packages("/ProteinUtil/RPackage/ggvenn/ggvenn_0.1.10.tar.gz", repos = NULL, type = "source")


#' # Function to draw a Venn diagram and save it to a file.
#'
#' This function takes a data frame of set elements, generates a Venn diagram, and saves the plot to a specified file.
#'
#' @param inputDataFrame A data frame where each column represents a set, and the values are the elements of each set.
#' @param outputFileName The name of the file to save the plot to.
#' @param logObject An optional logger object for logging messages. If not provided, a new logger will be created.
#' @return A list containing the ggplot object for the Venn diagram and a data frame of the set elements.
#' @examples
#' inputDataFrame <- data.frame(
#'   Set1 = c("A", "B", "C", NA),
#'   Set2 = c("B", "C", "D", "E"),
#'   Set3 = c("A", "E", "F", NA)
#' )
#' DrawVennPlot(inputDataFrame, "venn_diagram.png")
DrawVennPlot <- function(inputDataFrame, outputFileName, logObject = NULL) {

    # Init log object
    if (is.null(logObject)) {
        logObject <- GetLogger("_DrawVennPlot.log", consoleOutput = TRUE, fileOutput = TRUE)
    }

    # Define English and Chinese fonts
    englishFont <- "Times" # Replace with your English font
    chineseFont <- "SimSun" # Replace with your Chinese font
    # Check if the input is a data frame
    if (!is.data.frame(inputDataFrame)) {
		logObject$Error("绘图输入数据类型错误：不是数据框")
    }
    # Get the names of the sets (columns)
    sets <- colnames(inputDataFrame)
    # Create a list to store elements of each set
    setList <- lapply(sets, function(col) {
        elements <- inputDataFrame[[col]]
        elements <- elements[!is.na(elements) & elements != ""]
        return(elements)
    })
	# Get names max length of plot
	textMaxLength <- max(GetTextLength(sets, fontFamily = englishFont))
	# Get the expected width and height of the plot
	if (length(sets) == 0 | length(sets) ==1 ) {
		stop("The number of sets should be less than or equal to 3")
	} else if (length(sets) == 2) {
		expectedWidth <- textMaxLength * 2 + 4
		expectedHeight <- max(GetTextLength("一")) + 7
	} else if (length(sets) == 3) {
		expectedWidth <- textMaxLength * 2 + 4
		expectedHeight <- max(GetTextLength("一")) * 2 + 7
	} else if (length(sets) == 4) {
		expectedWidth <- textMaxLength * 2 + 7
		expectedHeight <- max(GetTextLength("一")) * 2 + 7
	} else {
		stop("The number of sets should be less than or equal to 4")
	}
	# Set the plot width and height
	plotWidth <- ifelse(expectedWidth > 8, expectedWidth, 8)
	plotHeight <- expectedHeight

    names(setList) <- sets
    # Draw Venn diagram
    vennPlot <- ggvenn::ggvenn(setList, sets, fill_color = defaultColor[1:length(sets)], show_percentage = FALSE, text_family = englishFont, set_name_size = 10, text_size = 8)

	# Save the plot
    SavePlot(vennPlot, outputFileName, width = expectedWidth, height = expectedHeight, format = "both", logObject)

    # Save the result as a data frame
    maxLength <- max(sapply(setList, length))
    vennData <- data.frame(lapply(setList, function(x) {
        length(x) <- maxLength
        return(x)
    }))

    # Return the ggplot object and data frame
    list(
        Plot = vennPlot,
        DataFrame = vennData
    )
}


#' # Venn diagram demo
#' 
#' This function demonstrates how to use the DrawVennPlot function to create a Venn diagram from a data frame of set elements.
#' 
#' @examples
#' VennDiagramDemo()
VennDiagramDemo <- function(){
	
	# Example data frame
	inputDataFrame <- data.frame(
		Set1 = c("A", "B", "C", NA),
		Set2 = c("B", "C", "D", "E"),
		Set3 = c("A", "E", "F", NA)
	)

    # Define the output directory
    demoPath <- "/ProteinUtil/Demo/VennDemo"
    # Create the output directory if it does not exist
    if (!dir.exists(demoPath)) {
        dir.create(demoPath, recursive = TRUE)
    }

    logObject <- GetLogger(glue::glue("{demoPath}/_DrawVennPlotDemo.log"), consoleOutput = TRUE, fileOutput = TRUE)

	# Draw Venn diagram and get the result
	result <- DrawVennPlot(inputDataFrame, glue::glue("{demoPath}/VennDiagram"), logObject)

	# Print the output data frame
	print(result$DataFrame)

}
