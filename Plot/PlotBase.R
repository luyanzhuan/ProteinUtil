# @Author       : luyz
# @Date         : 2024-08-16 18:30
# @Description  : Custom Plotting base functions
# Copyright (c) 2024 by luyz && luyz@aptbiotech.com
# Edited detail:
# 2024-08-16 18:30:00 [luyz] - Initial version


# Load custome log functions
source("/ProteinUtil/Base/BaseLog.R")
# Load custome utility functions
source("/ProteinUtil/Base/BaseUtil.R")

# Import font file
fontFolder <- glue::glue("/ProteinUtil/Font")
extrafont::font_import(paths = fontFolder, prompt = FALSE)
extrafont::loadfonts(device = "all", quiet = TRUE)

# Init color
defaultColor <-
  c(
    "#D51F26", "#272E6A", "#208A42", "#89288F", "#F47D2B", "#FEE500", "#8A9FD1", "#C06CAB", "#E6C2DC",
    "#90D5E4", "#89C75F", "#F37B7D", "#9983BD", "#D24B27", "#3BBCA8", "#6E4B9E", "#0C727C", "#7E1416",
    "#D8A767", "#7DD06F", "#844081", "#688EC1", "#C17E73", "#484125", "#6CD3A7", "#597873", "#7B6FD0",
    "#CF4A31", "#D0CD47", "#722A2D", "#CBC594", "#D19EC4", "#5A7E36", "#D4477D", "#403552", "#76D73C",
    "#96CED5", "#CE54D1", "#C48736", "#FFB300", "#803E75", "#FF6800", "#A6BDD7", "#C10020", "#CEA262",
    "#817066", "#007D34", "#F6768E", "#00538A", "#FF7A5C", "#53377A", "#FF8E00", "#B32851", "#F4C800",
    "#7F180D", "#93AA00", "#593315", "#F13A13", "#232C16", "#faa818", "#41a30d", "#fbdf72", "#367d7d",
    "#d33502", "#6ebcbc", "#37526d", "#916848", "#f5b390", "#342739", "#bed678", "#a6d9ee", "#0d74b6",
    "#60824f", "#725ca5", "#e0598b", "#371377", "#7700FF", "#9E0142", "#FF0080", "#DC494C", "#F88D51",
    "#FAD510", "#FFFF5F", "#88CFA4", "#238B45", "#02401B", "#0AD7D3", "#046C9A", "#A2A475", "grey35",
    "#D52126", "#88CCEE", "#FEE52C", "#117733", "#CC61B0", "#99C945", "#2F8AC4", "#332288", "#E68316",
    "#661101", "#F97B72", "#DDCC77", "#11A579", "#E73F74", "#A6CDE2", "#1E78B4", "#74C476", "#34A047",
    "#F59899", "#E11E26", "#FCBF6E", "#F47E1F", "#CAB2D6", "#6A3E98", "#FAF39B", "#B15928", "#1a1334",
    "#01545a", "#017351", "#03c383", "#aad962", "#fbbf45", "#ef6a32", "#ed0345", "#a12a5e", "#710162",
    "#3B9AB2", "#2a7185", "#a64027", "#9cdff0", "#022336", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00",
    "#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"
  )


#' # Function to check if a string contains Chinese characters
#'
#' This function checks if a given string contains any Chinese characters.
#'
#' @param string A character string to be checked.
#' @return A logical value indicating whether the string contains Chinese characters.
#' @examples
#' ContainsChinese("Hello") # FALSE
#' ContainsChinese("你好") # TRUE
ContainsChinese <- function(string) {
  grepl("[\u4e00-\u9fa5]", string)
}

#' # Function to calculate the width of text strings
#'
#' This function calculates the width of each text string in a vector, using a specified font family and size.
#'
#' @param textVector A character vector containing the text strings to be measured.
#' @param fontFamily The font family to be used for measurement. Default is "Arial".
#' @param fontSize The font size to be used for measurement. Default is 12.
#' @return A numeric vector containing the widths of the text strings in centimeters.
#' @examples
#' GetTextLength(c("Hello", "World"), fontFamily = "Times", fontSize = 12)
GetTextLength <- function(textVector, fontFamily = "Arial", fontSize = 12) {
  textWidths <- sapply(textVector, function(text) {
    textGrob <- grid::textGrob(text, gp = grid::gpar(fontfamily = fontFamily, fontsize = fontSize))
    textWidth <- grid::convertWidth(grid::grobWidth(textGrob), "cm", valueOnly = TRUE)
    return(textWidth)
  })
  return(textWidths)
}

#' # Function to save a ggplot object to a file in PNG and/or PDF format
#'
#' This function saves a ggplot object to a specified file path in PNG, PDF, or both formats.
#'
#' @param plot A ggplot object to be saved.
#' @param outputPath The file path (without extension) where the plot will be saved.
#' @param width The width of the saved plot in inches. Default is 8.
#' @param height The height of the saved plot in inches. Default is 6.
#' @param format The format(s) to save the plot in. Can be "png", "pdf", or "both". Default is "both".
#' @param logObject An optional logger object for logging messages. If not provided, a new logger will be created.
#' @return None
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' SavePlot(p, "my_plot", width = 10, height = 7, format = "png")
SavePlot <- function(plot, outputPath, width = 8, height = 6, format = c("png", "pdf", "both"), logObject = NULL) {
  # Init log object
  if (is.null(logObject)) {
    logObject <- GetLogger("_SavePlot.log", consoleOutput = TRUE, fileOutput = TRUE)
  }

  # Check if the format is valid
  format <- match.arg(format)

  # Save as PNG
  if (format == "png" || format == "both") {
    ggplot2::ggsave(filename = paste0(outputPath, ".png"), plot = plot, width = width, height = height, device = "png", create.dir = TRUE)
    logObject$Info(paste("图已经保存为PNG格式：", paste0(outputPath, ".png")))
  }

  # Save as PDF
  if (format == "pdf" || format == "both") {
    ggplot2::ggsave(filename = paste0(outputPath, ".pdf"), plot = plot, width = width, height = height, device = "pdf", create.dir = TRUE)
    logObject$Info(paste("图已经保存为PDF格式：", paste0(outputPath, ".pdf")))
  }
}
