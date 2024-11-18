###bogdan romocea, 2004 and later
###R functions for data manipulation, graphics and various utility and automation tasks

# Table of Contents:
# abbrev <- function(x, from, to)
# addCumSum <- function(DF, BY, sortby, v, out)
# addDiffs <- function(DF, BY, sortby, v, lags, out, pct=FALSE, cumul=FALSE)
# addMA <- function(DF, BY, sortby, v, lags, out)
# addMissingPeriods <- function(DF, BY, period, vars, fill=0)
# addMovSum <- function(DF, BY, sortby, v, lag, out, dt=NULL)
# all.NA <- function(x)
# app <- function(x, new) {c(x, list(new))}
# applyXLformat <- function(df, wb, sheet, vars, format, rows, startCol)
# blanks2NA <- function(x, factor=TRUE)
# blanks2NA.DF <- function(DF)
# boxp <- function(DF, by, v, ttl=NULL, sttl=NULL, log=FALSE, notch=TRUE, outliers=TRUE, ylim=NA)
# c2n <- function(x) {as.numeric(x)}
# capWords <- function(x)
# consecutive <- function(x)
# csv <- function(obj, outfile, rnames=FALSE, cnames=TRUE, append=FALSE, sep=",", na='NA', quote=TRUE)
# dedupe <- function(DF, BY, sortby, lowest=FALSE)
# delBlanks <- function(x)
# dfrep <- function(DF, digits=2)
# dotpl <- function(DF, x, y, sort=TRUE, clr=NULL, size=NULL)
# dtbsumm <- function(x, digits=4)
# excelSheet <- function(wb, df, ttl, sheet='Report', startRow=2, startCol=1, BY=NULL, pct=NULL, curr=NULL, comma=NULL, condFmt=NULL,
# F2C <- function(f)
# f2n <- function(x) {as.numeric(as.character(x))}
# fact.NA.replace <- function(x, newlevel)
# fact.rep <- function(DF, freq=TRUE)
# fromstr <- function(x, BY, N, last=FALSE, asNum=TRUE)
# getDict <- function(vec, freq, top=5000, dedupe=FALSE)
# ggpDataLabels <- function(p, DF, date, v, yfmt=waiver(), N=4, digits=0, totdigits=0, size=2.6, color='black', vjust=-0.5,
# ggrep <- function(x, what, any=TRUE)
# ggsub <- function(x, char, with=NULL, fixed=FALSE)
# gtbl <- function(x, N=19, byfreq=FALSE, main='', sz=2.8)
# hbp <- function(DF, x, y, bins=30)
# integer_breaks <- function(n = 5, ...)
# isnum <- function(x)
# listlike <- function(x)
# lst.nonull <- function(lst)
# matp <- function(DF, x, y, fill, ttl=NULL, sz=3, digits=3, lab=NULL)
# mavg <- function(x, lg, exp=FALSE)
# MonthStarting <- function(x) { as.Date(paste(format(x, '%Y-%m-'), '01', sep='')) }
# mp <- mpgg <- function(plt, nrow=1, ncol=1, rowFirst=TRUE)
# mp.new <- function(plt, nrow, ncol, top=NULL, topfont=16, bottom=NULL, left=NULL, right=NULL, common.legend=FALSE, lright=TRUE)
# msum <- function(x, lg, dt=NULL, past=TRUE)
# mysearch <- function(patt, ext=NULL, refresh=FALSE, dirs=c('C:/1br/1code'),
# n2c <- f2c <- function(x) {as.character(x)}
# n2d <- function(x) {as.Date(x, origin='1970-01-01')}
# na.test <- function(x)
# NA2zero <- function(x)
# now <- function(message="", after="\n") 
# obj <- function(envir=1, all=FALSE)
# prettyCuts <- function(x, prefix='', suffix='')
# printWrap <- function(x, width=90, tab=TRUE)
# qpl <- function(x, ylab='', main='', log=FALSE, full=TRUE)
# quarterDate <- function(x)
# read.mem <- function(x=NULL, ...)
# read.pipe <- function(fname, sep="|", na=c(".","","NA"), ...)
# reord <- function(x, lev)
# rets <- function(x, log=TRUE)
# Rpath <- function(x)
# rstack <- function(lst, names=FALSE, col='id')
# saveFct <- function(x=0)
# sortDF <- function(DF, BY, decreasing=FALSE)
# sq <- function(x, fmt="R", label="Value", digits=2)
# StackedBarLabelPos <- function(DF, by, class, var)
# stdTxt <- function(x)
# t2v <- function(x) {out <- as.numeric(x) ; names(out) <- names(x) ; out}
# tbl <- function(df, v='', sort=FALSE, name='Count', n=30)
# textSlide <- function(label='', size=5, boxcol='black', txtcol='black', backcol='white', wrap=25)
# todate <- function(x, fmt='%d%b%Y')
# toExcel <- function(x=NULL)
# top <- function(DF, v, n=30, desc=TRUE, rn=TRUE) 
# topdf <- function(fname, width=19.2, height=10.8, landscape=TRUE)
# topng <- function(fname, width=1275, height=960, landscape=TRUE)
# tsbar <- function(DF, date, v, datelab=NULL, stack=NULL, facet=NULL, ttl=NULL, sttl=NULL, labx=NULL, laby=NULL, 
# tsline <- function(DF, date, v, datelab=NULL, ttl=NULL, sttl=NULL, labx=TRUE, laby=NULL, clr=NULL, shape=NULL, size=NULL,
# v2w <- function(x, sep=' ')
# vplot <- function(x, label=names(x), log='', sort=FALSE, size=2, flip=FALSE, ...) 
# WeekStarting <- function(x) { x - (as.numeric(format(x, '%u')) - 1) }  #%u means Monday=1
# wide2long <- function(DF, id.vars, measure.vars, groupname='Group', varname='Value')

#-------------------------------------------------------------------------------

#2024-06: Start from the levels created by cut() and make them prettier - for reporting.
#x = a vector of factor levels, to be made prettier
prettyCuts <- function(x, prefix='', suffix='')
{
nl <- rep(NA, length(x))
for (i in 1:length(x)) {
   v <- ggsub(x[i], c('[',']','(',')'), fixed=TRUE)
   amt <- base::paste0(prefix, base::prettyNum(as.numeric(base::strsplit(v, ',')[[1]]), big.mark=",", scientific=FALSE), suffix)
   nl[i] <- base::paste0(base::substr(x[i], 1, 1), paste(amt, collapse=' to '), base::substr(x[i], nchar(x[i]), nchar(x[i])))
}
return(nl)
}

#-------------------------------------------------------------------------------

#br 2024-06
#Start from a vector of strings (such as transaction descriptions) and their frequencies (weights), and
#    assemble a dictionary showing the most frequently used words.
#NB: Working with matrices instead of data frames (at the string level) makes the function run ~5 times faster
#vec = a vector of strings
#freq = the counts/weights of each string
getDict <- function(vec, freq, top=5000, dedupe=FALSE)
{
#WOW....... str_extract_all() is THE way to go.
#Guess what: delete all digits, because some that are not meaningful make it into the final output
# words <- str_split(vec, ' ')
#Split all descriptions into words
words <- stringr::str_extract_all(str_remove_all(vec, "[0123456789]"), boundary("word"))
#Some strings contain the same word(s) multiple times
if (dedupe) words <- lapply(words, unique)

#Put everything in a really TALL format
ww <- vector('list', length(words))
for (i in 1:length(words)) {
   NW <- length(words[[i]])
   # ww[[i]] <- data.frame(words=words[[i]], count=rep(freq[i], NW))
   ww[[i]] <- cbind(words[[i]], rep(freq[i], NW))
}
# ww <- dplyr::bind_rows(ww)
ww <- as.data.frame(do.call('rbind', ww))
colnames(ww) <- c('Word', 'count')
ww[['count']] <- as.numeric(ww[['count']])
#Summarize by word
ws <- dplyr::group_by(ww, Word) %>% dplyr::summarize(Count=sum(count), Strings=n()) %>% ungroup() %>%
   dplyr::arrange(desc(Count))
head(ws, top)  #return the top X words only
}

#-------------------------------------------------------------------------------

#br 2023-11
#This function saves some typing

NA2zero <- function(x)
{
x[is.na(x)] <- 0
x
}

#-------------------------------------------------------------------------------

#br 2013-10
#This function saves some typing when a series of strings need to be abbreviated.
#from = vector with the strings to abbreviate
#to = vector with the shortened names

abbrev <- function(x, from, to)
{
y <- x
for (i in 1:length(from)) y[y == from[i]] <- to[i]
y
}

#-------------------------------------------------------------------------------

#br 2023-07
#Given a data frame and a vector of BY fields which define the groups, insert records for those
#    groups of records which are missing certain dates. This emerged as a very significant issue
#    on the Shiny apps, because it leads to cumulative curves that have missing values for any
#    periods absent from the data. Furthermore the tidyr package already has some functionality
#    like that (complete) but I just could not make it work in the Shiny modules, apparently 
#    because of scenarios where some variables in the BY list can be duplicated. But how to dedupe
#    them and still have the dplyr formulas work? So I ended up writing my own function.

addMissingPeriods <- function(DF, BY, period, vars, fill=0)
{
# if (!exists('bind_rows')) require(tidyverse)
aper <- base::unique(DF[, period])  #all periods
#All combinations of records. For each of these, any missing periods need to be added
ID <- apply(DF[, BY], 1, function(x) paste(x, collapse='_'))
#Loop by group and determine the records to add, if any
perByID <- base::split(DF[, period], ID)
toAdd <- vector('list', length(perByID))
for (i in 1:length(perByID)) {
missingPer <- n2d(setdiff(aper, perByID[[i]]))  #so the assumption is that these are dates
# print(missingPer)
if (length(missingPer) > 0) {
    recs <- DF[which(ID == names(perByID)[i]), ]
    toFill <- recs[1, BY, drop=FALSE]
    rownames(toFill) <- NULL
    filling <- data.frame(missingPer, toFill)
    colnames(filling)[1] <- period
    toAdd[[i]] <- filling
}
}
toAdd <- do.call('rbind', toAdd)
if (is.null(toAdd)) return(DF)
# if (nrow(toAdd) == 0) return(DF)
for (v in vars) toAdd[, v] <- fill
sortDF(bind_rows(DF, toAdd), period)
}

#-------------------------------------------------------------------------------

#br 2023-08
#This function prints a data frame to an Excel sheet and applies a number of customizations
#    wb = the workbook to use (created previously)
#    df, ttl = the name of the data frame and the title of the report
#    sheet = the name of the sheet
#    startRow, startCol = where to start printing the report
#    BY = a column whose values will be used for alternate shading
#    pct, curr, comma = the names of fields for which %, $ and comma formats will be applied
#    condFmt = the names of fields for which conditional formatting will be added
#    altFill = the color to use for alternate foreground shading. The other color will be white (actually none)
#   dataWidth - whether to give priority to the data when determining the column widths. If TRUE then
#       all data values will appear un-truncated
excelSheet <- function(wb, df, ttl, sheet='Report', startRow=2, startCol=1, BY=NULL, pct=NULL, curr=NULL, comma=NULL, condFmt=NULL,
                   altFill="#D9D9D9", fontSize=11, dataWidth=TRUE)
{
allRows <- seq(startRow +1, startRow + nrow(df))  #all the rows in the report
allCols <- (1:ncol(df)) + (startCol - 1)

#---define the styles to use. These can remain fixed - or if desired could be further customized through additional parameters
pct.fmt <- openxlsx::createStyle(numFmt="PERCENTAGE", border="TopBottomLeftRight", fontSize = fontSize)
pct.fmtAlt <- openxlsx::createStyle(numFmt="PERCENTAGE", border="TopBottomLeftRight", fgFill = altFill, fontSize = fontSize)
#sty1 = openxlsx::createStyle(numFmt="$0")
#sty2 = openxlsx::createStyle(numFmt="$0.00")
curr.fmt = openxlsx::createStyle(numFmt="$0,0", border="TopBottomLeftRight", fontSize = fontSize)
curr.fmtAlt = openxlsx::createStyle(numFmt="$0,0", border="TopBottomLeftRight", fgFill = altFill, fontSize = fontSize)
comma.fmt = openxlsx::createStyle(numFmt="0,0", border="TopBottomLeftRight", fontSize = fontSize)
comma.fmtAlt = openxlsx::createStyle(numFmt="0,0", border="TopBottomLeftRight", fgFill = altFill, fontSize = fontSize)
headerStyle <- openxlsx::createStyle(textDecoration = "Bold", fontSize = 12, fontColour = "navy",
                                     fgFill = "lightyellow", halign = "left", border="TopBottomLeftRight", borderStyle="medium")
titleStyle <- openxlsx::createStyle(fontSize = 17, fontColour = "navy")  #textDecoration = "Bold"
allBorders <- openxlsx::createStyle(border="TopBottomLeftRight", fontSize = fontSize)
altStyle <- openxlsx::createStyle(fgFill = altFill, border="TopBottomLeftRight", fontSize = fontSize)  #fontColour = "#006100"

#---Alternate shading of rows. As it turns out, the shading must be added first: because if added after 
#    the data is written, it will impact/overwrite the date formats as well as the borders.
openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
if (!is.null(BY)) {
    FlagForShading <- cumsum(c(TRUE, head(df[, BY], -1) != tail(df[, BY], -1)))
    FFS.isodd <- startRow + which(FlagForShading / 2 == floor(FlagForShading / 2))
    openxlsx::addStyle(wb, sheet, style = altStyle, cols = allCols, rows = FFS.isodd, gridExpand=TRUE)
    openxlsx::addStyle(wb, sheet, style = allBorders, cols = allCols, rows = setdiff(allRows, FFS.isodd), gridExpand=TRUE)
} else {
    openxlsx::addStyle(wb, sheet, style = allBorders, cols = allCols, rows = allRows, gridExpand=TRUE)
}

# openxlsx::setRowHeights(wb, 1, rows = 1, heights = 40)

#---write the title + report
openxlsx::writeData(wb, sheet, x = ttl, startRow = startRow - 1, startCol = startCol)
openxlsx::addStyle(wb, sheet, rows = startRow - 1, cols = startCol, style = titleStyle)
openxlsx::writeData(wb, sheet, df, startRow = startRow, startCol = startCol)  #borders = "all", headerStyle = headerStyle
openxlsx::addStyle(wb, sheet, style = headerStyle, rows=startRow, cols=allCols, gridExpand=TRUE)

#---Apply the %, $ and comma formats. These must have their foreground tweaked as well... or will all show up as white
if (!is.null(BY)) {
    wb <- applyXLformat(df, wb, sheet, pct, pct.fmtAlt, FFS.isodd, startCol)
    wb <- applyXLformat(df, wb, sheet, pct, pct.fmt, setdiff(allRows, FFS.isodd), startCol)
    wb <- applyXLformat(df, wb, sheet, curr, curr.fmtAlt, FFS.isodd, startCol)
    wb <- applyXLformat(df, wb, sheet, curr, curr.fmt, setdiff(allRows, FFS.isodd), startCol)
    wb <- applyXLformat(df, wb, sheet, comma, comma.fmtAlt, FFS.isodd, startCol)
    wb <- applyXLformat(df, wb, sheet, comma, comma.fmt, setdiff(allRows, FFS.isodd), startCol)
} else {
    wb <- applyXLformat(df, wb, sheet, pct, pct.fmt, allRows, startCol)
    wb <- applyXLformat(df, wb, sheet, curr, curr.fmt, allRows, startCol)
    wb <- applyXLformat(df, wb, sheet, comma, comma.fmt, allRows, startCol)
}

#---freeze the title(s) and header
openxlsx::freezePane(wb, sheet, firstActiveRow = startRow + 1)

#---Adjust the column widths to reasonable values (e.g. anything that is not completely wrong)
#It turns out that the "auto" option fails to work with some date fields, which show up as "#######" in Excel.
#    Therefore query the widths from the data. One can set the widths to (1) the longest value in each column, or
#    (2) the width of the header, or even combine these. But the great thing is that this can all be done with
#    just a couple of lines of code
#setColWidths(wb, sheet = 1, cols = 1:ncol(df), widths = "auto")
#get the longest width for each column + 2
data_widths <- apply(df, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
#the widths of the header + 2
header_widths <- nchar(colnames(df))  + 2
if (dataWidth) {
   final_widths <- pmax(data_widths, header_widths)
} else {
    #In some cases, the data is only slightly wider than the field widths. If so, then give priority to the data
    final_widths <- header_widths
    left2longest <- data_widths - header_widths
    toadj <- which(left2longest > 0 & left2longest < 10)
    final_widths[toadj] <- data_widths[toadj]
}
openxlsx::setColWidths(wb, sheet, cols = 1:ncol(df) + (startCol - 1), widths = final_widths)

#---conditional formatting
if (!is.null(condFmt)) {
    openxlsx::conditionalFormatting(wb, sheet, cols = which(colnames(rep) %in% condFmt) + (startCol - 1), rows = allRows, type = "databar")
    #openxlsx::conditionalFormatting(wb, sheet, cols = which(colnames(rep) %in% condFmt) + (startCol - 1), rows = allRows, type = "databar", style=c("green"))
}
#at this point the sheet has been added and the workbook is ready for saving or for further edits (such as adding another sheet)
return(wb)  
}

#-------------------------------------------------------------------------------

#br 2023-08
#Given a data frame, Excel workbook and sheet, apply a format (such as $ or %) to the requested columns and fields.
#   This is a helper function for excelSheet().
applyXLformat <- function(df, wb, sheet, vars, format, rows, startCol)
{
if (is.null(vars)) return(wb)
cols <- which(colnames(df) %in% vars) + (startCol - 1)
openxlsx::addStyle(wb, sheet, style = format, rows=rows, cols=cols, gridExpand=TRUE)
return(wb)
}

#-------------------------------------------------------------------------------

#br 2023-10
#This is meant just to save some typing
toExcel <- function(x=NULL)
{
#if (!exists('createWorkbook')) require(openxlsx)
openxlsx::createWorkbook()
}

#-------------------------------------------------------------------------------

#br 2023-08
#Borrowed part of the code from tsline() and put it into a separate function - to make this functionality
#    available to all kinds of ggplots.

ggpDataLabels <- function(p, DF, date, v, yfmt=waiver(), N=4, digits=0, totdigits=0, size=2.6, color='black', vjust=-0.5,
                         stack=NULL, totcolor='blue', showTotal=TRUE)
{
vlab <- 'point_labels'
if (class(DF[, v, drop=TRUE])[1] == 'integer') digits <- 0  #do not label integers with decimals
req.fmt <- toupper(deparse(substitute(yfmt)))
# print(paste0('Requested format: ', req.fmt))
if (req.fmt %in% c('WAIVER()', 'COMMA', 'DOLLAR')) {
   # DF[,vlab] <- sprintf(paste0('%3.', digits, 'f'), DF[,v])
   # DF[,vlab] <- format(DF[,v], digits=digits, nsmall=digits, big.mark=",")
   DF[,vlab] <- format(round(DF[,v, drop=TRUE], digits), nsmall=digits, big.mark=",", scientific = FALSE)
} else if (req.fmt == 'PERCENT') {
   DF[,vlab] <- sprintf(paste0('%3.', digits, 'f'), DF[,v, drop=TRUE] * 100)
}
dts <- sort(unique(DF[,date, drop=TRUE]))

#Label no more than N periods on the charts, to avoid overcrowding
if (length(dts) <= N) {
   dtslab <- dts
} else {
   # byLab <- - floor(length(dts) / (N - 1))
   byLab <- -N
   dtslab <- dts[seq(length(dts), 1, by=byLab)]
}
flab <- DF[DF[,date, drop=TRUE] %in% dtslab, ]

if (!is.null(stack)) {
   flab <- StackedBarLabelPos(as.data.frame(flab), date, stack, v)
   flab.tot <- group_by_at(flab, date) %>% dplyr::summarize(N=sum(!!sym(v))) %>% rename(!!v := 'N') %>% dplyr::ungroup()
   #Weird but true: ggplot errors out without this field present there. ALSO... it is necessary to use the
   #    first value so that the factor order, if any, is preserved -- otherwise the legend will be reordered!!
   flab.tot[, stack] <- DF[1, stack]
   #It turns out that the totals should be formatted in the same way...
   if (req.fmt %in% c('WAIVER()', 'COMMA', 'DOLLAR')) {
           flab.tot[, vlab, drop=TRUE] <- format(round(flab.tot[, v, drop=TRUE], totdigits), nsmall=totdigits, 
                                                 big.mark=",", scientific=FALSE)
   } else if (req.fmt == 'PERCENT') {
           flab.tot[, vlab, drop=TRUE] <- sprintf(paste0('%3.', totdigits, 'f'), flab.tot[, v, drop=TRUE] * 100)
   }
   ypos <- paste0(v, '_lab')
   vjust <- 0
} else {
   ypos <- v
}

p <- p + geom_text(data=flab, aes(.data[[date]], .data[[ypos]], label=.data[[vlab]]), size=size, vjust=vjust, hjust=0.5, 
             color=color, check_overlap=TRUE) 

if (!is.null(stack)) {
   if (showTotal) {
        p <- p + geom_text(data=flab.tot, aes(.data[[date]], .data[[v]], label=.data[[vlab]]), size=size + 0.5, vjust=vjust - 0.3, hjust=0.5,
             color=totcolor)
   }
}
return(p)
}

#-------------------------------------------------------------------------------

#br 2017-06
#Wrote a function to group all of the features and best practices that are useful when plotting
#    time series data with ggplot in stacked bar format.
#2017-07: Automatically label the totals for the first and last bars.
#2017-10: Check the overlaps when plotting the text labels. The output is thus much cleaner and easier to read.
#2017-12: Gray borders for all bars (very useful in some cases), plus colors that can be customized for
#    printing in black & white
#2018-05: Label the first and last bar with the name of the categories. This is particularly helpful when
#    the stacked bars include lots of slices, which may become difficult to identify just by color.
#2020-01: Repeated the tsline() enhancements here. Quotes are no longer required for X and Y, and also the
#    point labels are created inside the function.
#Arguments:
#    date, datelab = the name of the field with the date, and its label to be displayed on the X axis
#    v = the name of the metric of interest
#    sz = the size of the text labels
#    ttl, sttl, laby = title, subtitle and Y axis label; the X axis is customized per datelab above
#    facet, fs, fr = the name of the facetting variable, the scale type, and the number of rows
#    yfmt = the format, if any, to be applied to the Y scale
#    bw = bar width
#    baw = stands for Black And White. If true, use a color scale that allows to tell which is which
#        when printing in black & white.
#    xlim = the limits for the X axis, if wanting to overwrite the default
#    total = whether to label the totals on top of the bars
#    labels, sz = whether to print labels and if yes, their size
#    names = whether to also label the names of the categories below the figures

tsbar <- function(DF, date, v, datelab=NULL, stack=NULL, facet=NULL, ttl=NULL, sttl=NULL, labx=NULL, laby=NULL, 
              fs='fixed', fr=NULL, sz=3, labels=TRUE, bw=NULL, xlim=NULL, N=NULL, baw=FALSE, total=TRUE, names=FALSE, 
              yfmt=waiver(), label=NULL, digits=1, dfmt="%b'%Y")
{
if (!exists('ggplot')) require(ggplot2)
if (!exists('date_format')) require(scales)
#Well well. For Shiny, the v argument can be passed as string and there is no way around that...
isc.date <- isc.v <- FALSE
try(isc.v <- is.character(v), silent=TRUE)
try(isc.date <- is.character(date), silent=TRUE)
if (!isc.v) v <- deparse(substitute(v))
if (!isc.date) date <- deparse(substitute(date))
if (is.null(datelab)) datelab <- date  #use the column name if no label was provided

#2020-01: Set up the point labels. Looong overdue: so all of that extra code creating labels can now be deleted
vlab <- 'point_labels'
#2023-05: digits=0 is now an error in R 4.1: 
if (class(DF[,v]) == 'integer') digits <- 1  #0  #do not label integers with decimals
req.fmt <- toupper(deparse(substitute(yfmt)))
# print(paste0('Requested format: ', req.fmt))

if (req.fmt %in% c('WAIVER()', 'COMMA')) {
   #2024-05: Amazing but true. I cannot round this field for simple counts - otherwise they show up with .0 decimals
   DF[,vlab] <- base::format(DF[, v], digits=digits, nsmall=digits, big.mark=",", scientific=FALSE)
} else if (req.fmt == 'DOLLAR') {
   DF[,vlab] <- base::format(round(DF[, v], digits), digits=digits, nsmall=digits, big.mark=",", scientific=FALSE)
} else if (req.fmt == 'PERCENT') {
   DF[,vlab] <- sprintf(base::paste0('%3.', digits, 'f'), DF[,v] * 100)
}

#2020-02: Adjusted the default N value. Instead of 8 everywhere, use 4 if facetting
if (is.null(N)) N <- if (is.null(facet)) 8 else 4

#customize the label for the X axis
if (is.null(labx)) labx <- paste(datelab, ' (', paste(range(DF[,date], na.rm=TRUE), collapse = ' to '), ')', sep='')
#Label no more than 8 periods on the charts, to avoid overcrowding. These periods need to be determined
#    dynamically, to work with time series of any length.
dts <- sort(unique(DF[,date]))
if (length(dts) <= N) {
    dtslab <- dts
} else {
    byLab <- - floor(length(dts) / (N-1))
    dtslab <- dts[seq(length(dts), 1, by=byLab)]
}
#compute the position of the labels 
#Also get the totals for the first and last bars. Well, actually display the totals for all the bars, 
#    since this is rather good to have.
if (!is.null(stack)) {
    if (is.null(facet)) {
        pDF <- StackedBarLabelPos(DF, date, stack, v)
        tot <- aggregate(as.formula(paste(v, '~', date)), data=DF, sum)
        #HA! this is completely irrelevant but it turns out ggplot refuses to plot these labels unless this column is also present
        tot[,stack] <- head(DF[,stack], 1)
    } else {
        pDF <- StackedBarLabelPos(DF, c(facet, date), stack, v)
        tot <- aggregate(as.formula(paste(v, '~', paste(c(date, facet), collapse=' + '))), data=DF, sum)
        tot[,stack] <- head(DF[,stack], 1)
    }
} else {
    pDF <- tot <- DF
    pDF[, paste0(v, '_lab')] <- pDF[, v]
}

#It turns out that the totals should be formatted in the same way...
if (req.fmt %in% c('WAIVER()', 'COMMA', 'DOLLAR')) {
    tot[,vlab] <- format(tot[, v], digits=digits, nsmall=digits, big.mark=",")
} else if (req.fmt == 'PERCENT') {
    tot[,vlab] <- sprintf(paste0('%3.', digits, 'f'), tot[,v] * 100)
}

flab <- pDF[pDF[,date] %in% dtslab,]  #the numeric labels
tot <- tot[tot[,date] %in% dtslab,]
#The plot, with the customizations requested 
p <- if (is.null(stack)) {
    ggplot(pDF, aes_string(date, v, label=label)) + geom_bar(stat='identity', width=bw, color='gray80', fill='lightblue')
} else {
    ggplot(pDF, aes_string(date, v, fill=stack, label=label)) + geom_bar(stat='identity', width=bw, color='gray80')
}
p <- p + labs(title=ttl, subtitle=sttl, x=labx, y=laby)
if (class(DF[,date])[1] == "Date") p <- p + scale_x_date(labels = date_format(dfmt), limits=xlim)
if (total) p <- p + geom_text(data=tot, aes_string(date, v, label=vlab), color="blue", size=sz, vjust=-0.4)
if (labels) p <- p + geom_text(data=flab, aes_string(date, paste(v, '_lab', sep=''), label=vlab),
                               color="black", size=sz, check_overlap=TRUE)
if (names) p <- p + geom_text(data=flab, aes_string(date, paste(v, '_lab', sep=''), label=stack), 
                              color="brown", size=sz-1, vjust=2, check_overlap=TRUE)
if (!is.null(facet)) p <- p + facet_wrap(as.formula(paste('~', facet)), scales=fs, nrow=fr)
if (baw) p <- p + scale_fill_brewer()
if (class(yfmt) != "waiver") p <- p + ggplot2::scale_y_continuous(labels=yfmt)
p + scale_fill_brewer(palette = "Paired")
}

#-------------------------------------------------------------------------------

#br 2017-05
#Wrote a function to group all of the features and best practices that are useful when plotting
#    time series data with ggplot.
#2017-10: Check the overlaps when plotting the text labels. The output is thus much cleaner and easier to read.
#2017-12: Text label colors are now automated to be blue for single series, but the same colors as the lines
#    for multiple series. Moving away from a single label color (blue) in all cases is helpful particularly
#    when plotting multiple series where it may not always be clear which is which.
#2018-10: Multiple edits meant to support the conversion of ggplots to plotly.
#2020-01: Major changes, sort of. Added functionality so that it is no longer necessary to quote some of the
#    arguments, particularly X and Y. In addition the data labels are now generated inside this function, thus
#    eliminating all of the extra code required to create those labels separately. Unfortunately this breaks
#    all of the existing code, but so be it... there is no other way.
#Arguments:
#    date, datelab = the name of the field with the date, and its label to be displayed on the X axis
#    v, vlab = the name of the fields with the metric of interest (first to plot, second formatted for labelling)
#    sz, ps = the size of the text labels & points
#    labx = whether to label the X axis. Strangely enough, I now (2017-09) have a valid case where the X axis labels
#        should be hidden (in a loop where facetting could not be used).
#    ttl, laby = title and Y axis label; the X axis is customized per datelab above
#    clr = name of column to be used for color, if any
#    facet, fs, fr = the name of the facetting variable, the scale type, and the number of rows
#    yfmt = the format, if any, to be applied to the Y scale
#    labLast = whether only the last point for each group should be labelled. The default is to label up to
#        8 points, but labelling only the last one is helpful for series where the last points will vary,
#        such as cumulative balances of accounts opened from various promos.
#    N = the number of points to be labelled on the charts
#    Yaxis = a percentage that defines the min value on the Y scale. If given, the limits on the Y axis become
#        [Yaxis * max(Y), max(Y)]. The idea is to prevent the potential mis-perception of a series that appears
#        to show some significant changes, but only if zoomed-in.
#        2018-07: This can also be given as a vector to be used as-is.
#    xlim = the limits for the X axis; useful in some cases

tsline <- function(DF, date, v, datelab=NULL, ttl=NULL, sttl=NULL, labx=TRUE, laby=NULL, clr=NULL, shape=NULL, size=NULL,
               facet=NULL, fs='fixed', fr=NULL, fc=NULL, sz=2.6, ps=1.2, labLast=FALSE, N=NULL, Yaxis=NULL, yfmt=waiver(), xlim=NULL, 
               group=NULL, label=NULL, dfmt="%b'%Y", digits=1, lines=TRUE)
{
if (!exists('ggplot')) require(ggplot2)
if (!exists('date_format')) require(scales)
#Well well. For Shiny, the v argument needs to be passed as string and there is no way around that...
isc.date <- isc.v <- FALSE
try(isc.v <- is.character(v), silent=TRUE)
try(isc.date <- is.character(date), silent=TRUE)
if (!isc.v) v <- deparse(substitute(v))
if (!isc.date) date <- deparse(substitute(date))
if (is.null(datelab)) datelab <- date  #use the column name if no label was provided

#2020-01: Set up the point labels. Looong overdue: so all of the extra code creating labels can now be deleted
vlab <- 'point_labels'
if (class(DF[,v]) == 'integer') digits <- 0  #do not label integers with decimals
req.fmt <- toupper(deparse(substitute(yfmt)))
# print(paste0('Requested format: ', req.fmt))
if (req.fmt %in% c('WAIVER()', 'COMMA', 'DOLLAR')) {
    # DF[,vlab] <- sprintf(paste0('%3.', digits, 'f'), DF[,v])
    # DF[,vlab] <- format(DF[,v], digits=digits, nsmall=digits, big.mark=",")
    DF[,vlab] <- format(round(DF[,v], digits), nsmall=digits, big.mark=",", scientific = FALSE)
} else if (req.fmt == 'PERCENT') {
    DF[,vlab] <- sprintf(paste0('%3.', digits, 'f'), DF[,v] * 100)
}

#2020-02: Adjusted the default N value. Instead of 8 everywhere, use 4 if facetting
if (is.null(N)) N <- if (is.null(facet)) 8 else 4

#customize the label for the X axis
labxcust <- if (labx) paste(datelab, ' (', paste(range(DF[,date], na.rm=TRUE), collapse = ' to '), ')', sep='') else NULL
if (class(DF[,date]) != "Date") labxcust <- datelab
dts <- sort(unique(DF[,date]))
if (!labLast) {
    #Label no more than 8 periods on the charts, to avoid overcrowding. These periods need to be determined
    #    dynamically, to work with time series of any length.
    if (length(dts) <= N) {
        dtslab <- dts
    } else {
        byLab <- - floor(length(dts) / (N - 1))
        dtslab <- dts[seq(length(dts), 1, by=byLab)]
    }
    flab <- DF[DF[,date] %in% dtslab,]
} else {
    #Which one is the BY group? It can be either clr, shape or facet, or a combination... so put them all 
    #    together and keep the one(s) that are not null.
    BY <- list(clr, shape, facet)
    BY <- unlist(BY[!sapply(BY, is.null)])
    sBY <- if (length(BY) == 1) DF[, BY] else apply(DF[,BY], 1, function(x) paste(x, collapse=' '))
    #finally, get the latest observation from every group
    #PS: also add the first one...
    flab <- rstack(lapply(split(DF, sBY), function(x) x[c(which.min(x[, date]), which.max(x[, date])), ]))
}
#Guess what - label the first and last points separately, because the same alignment may cut off some labels
#2023-08: Finally fixed the warnings caused by flab having 0 rows
if (nrow(flab) > 0) {
    firstp <- which(flab[,date] == min(flab[,date]))
    flabeg <- flab[firstp,]
    flab <- flab[-firstp,]
} else {
    flabeg <- flab
}

#the Y axis limits
if (!is.null(Yaxis)) {
    Yax <- if (length(Yaxis) == 1) c(min(max(DF[,v]) * Yaxis, min(DF[,v])), max(DF[,v])) else Yaxis
} else Yax <- range(DF[,v])

#The plot, with the customizations requested
#2017-12: If a color argument was given then have the text labels take the color of the lines, to avoid
#    potential confusions. But if a color argument is not given then color the text labels blue, to
#    increase contrast.
#2023-11: Replaced aes_string(), which has been deprecated, with the currently recommended syntax
my_opts <- if (is.null(clr)) c(geom_text(data=flab, aes(.data[[date]], .data[[v]], label=.data[[vlab]]), size=sz, vjust=-0.5, hjust=0.9, 
                                         check_overlap=TRUE, color="blue"),
                               geom_text(data=flabeg, aes(.data[[date]], .data[[v]], label=.data[[vlab]]), size=sz, vjust=-0.5, hjust=0, 
                                         check_overlap=TRUE, color="blue"))
else c(geom_text(data=flab, aes(.data[[date]], .data[[v]], label=.data[[vlab]]), size=sz, vjust=-0.5, hjust=0.9, check_overlap=TRUE),
       geom_text(data=flabeg, aes(.data[[date]], .data[[v]], label=.data[[vlab]]), size=sz, vjust=-0.5, hjust=0, check_overlap=TRUE))

if (lines) my_opts <- c(geom_line(), my_opts) else my_opts <- c(geom_point(), my_opts)
if (is.null(xlim)) xlim <- range(DF[,date])
if (class(DF[,date]) == "Date") my_opts <- c(my_opts, scale_x_date(labels = date_format(dfmt), limits=xlim))
#The initial version with group=NULL thrown in there by default was entirely fine in ggplot,
#    but it caused a strange error in plotly, that took me quite a bit to identify. So have that
#    part being run separately.

#2023-11: Tried to get rid of aes_string() here as well but kept getting some errors back. Troubleshoot those later
p <- if(is.null(group)) ggplot(DF, aes_string(date, v, color=clr, shape=shape, label=label)) else
    ggplot(DF, aes_string(date, v, color=clr, shape=shape, label=label, group=group))
# p <- if(is.null(group)) ggplot(DF, aes(.data[[date]], .data[[v]], color=.data[[clr]], shape=.data[[shape]], label=.data[[label]])) else
#    ggplot(DF, aes(.data[[date]], .data[[v]], color=.data[[clr]], shape=.data[[shape]], label=.data[[label]], group=.data[[group]]))

p <- p + labs(title=ttl, subtitle=sttl, x=labxcust, y=laby) + my_opts
if (is.null(size)) p <- p + geom_point(size=ps) else p <- p + geom_point(aes_string(size=size))
if (!is.null(facet)) p <- p + facet_wrap(as.formula(paste('~', facet)), scales=fs, nrow=fr, ncol=fc)
p <- if (tolower(fs) %in% c('free','free_y')) p + ggplot2::scale_y_continuous(labels=yfmt) else p + ggplot2::scale_y_continuous(limits=Yax, labels=yfmt)
p #+ scale_color_brewer(palette = "Dark2")
}

#-------------------------------------------------------------------------------

#br 2016-10
#Compute and add cumulative figures to a data frame (DF)
#2017-02: Added a flag called "Latest", which is useful for labelling the latest points in granular
#	data (such as branch-level), where not all BY levels can be expected to have results from the 
#	same periods, yet it would be helpful to label the latest points for every BY group anyway.
#BY = text vector with the names of the columns identifying the groups for which cumulative sums
#	are to be computed
#sortby = the name of the field (usually a date) by which the data will be sorted
#v, out = the name of the field of interest, respectively of the output field

addCumSum <- function(DF, BY, sortby, v, out)
{
sBY <- if (length(BY) == 1) DF[, BY] else apply(DF[,BY], 1, function(x) paste(x, collapse=' '))
rstack(lapply(split(DF, sBY), function(x) {
tmp <- sortDF(x, sortby)
#	if (!'Latest' %in% colnames(DF)) {
	tmp$Latest <- FALSE
	tmp$Latest[nrow(tmp)] <- TRUE
#	}
tmp[, out] <- cumsum(tmp[, v])
tmp
}))
}

#-------------------------------------------------------------------------------

#br 2017-04
#Add differences of various lags for a given field
#It turns out that the percent change can also be very helpful, even strictly necessary, for 
#	properly tracking balance changes.
#2017-06: A strange bug came up here, whereby a factor with a level whose rows had all been
#	subset caused a strange error ("replacement has 1 row, data has 0"). This was fixed by
#	converting BY to character.
#2019-06: Added the cumulative changes.

addDiffs <- function(DF, BY, sortby, v, lags, out, pct=FALSE, cumul=FALSE)
{
sBY <- if (length(BY) == 1) as.character(DF[, BY]) else apply(DF[,BY], 1, function(x) paste(x, collapse=' '))
rstack(lapply(split(DF, sBY), function(x) {
tmp <- sortDF(x, sortby)
for (l in lags) {
	tmp[, paste0(out, l)] <- c(rep(NA, l), diff(tmp[, v], l))
	if (pct) tmp[, paste0('Pct', out, l)] <- c(rep(NA, l), diff(tmp[, v], l) / tmp[1:(nrow(tmp) - l), v])
	#The cumsum function will return NA ever after a single NA is encountered. Also, it does not have an na.rm
	#	argument like many other functions. As a workaround, replace any missing values with 0 before cumsum.
	if (cumul) {
		forcs <- tmp[, paste0(out, l)]
		forcs[is.na(forcs)] <- 0
		tmp[, paste0('Cumul', out, l)] <- cumsum(forcs)
	}
}
tmp
}))
}

#-------------------------------------------------------------------------------

#br 2017-05
#Add the Moving Average to a data frame.

addMA <- function(DF, BY, sortby, v, lags, out)
{
sBY <- if (length(BY) == 1) DF[, BY] else apply(DF[,BY], 1, function(x) paste(x, collapse=' '))
rstack(lapply(split(DF, sBY), function(x) {
tmp <- sortDF(x, sortby)
for (l in lags) {
	tmp[, paste(out, l, sep='')] <- mavg(tmp[, v], l)
}
tmp
}))
}

#-------------------------------------------------------------------------------

#br 2017-02
#Add the Moving Sum to a data frame. The Moving Sum is meant to supplement the Cumulative Sum.

addMovSum <- function(DF, BY, sortby, v, lag, out, dt=NULL)
{
sBY <- if (length(BY) == 1) DF[, BY] else apply(DF[,BY], 1, function(x) paste(x, collapse=' '))
rstack(lapply(split(DF, sBY), function(x) {
tmp <- sortDF(x, sortby)
tmp$Latest <- FALSE
tmp$Latest[nrow(tmp)] <- TRUE
dtf <- if (is.null(dt)) NULL else tmp[, dt]
tmp[, out] <- msum(tmp[, v], lag, dt=dtf)
tmp
}))
}

#-------------------------------------------------------------------------------

#br 2011-07
#Check whether all values in a given vector are NA.

all.NA <- function(x)
{
allmiss <- unique(is.na(x))
if (length(allmiss) == 1) am <- allmiss else am <- FALSE
invisible(am)
}

#-------------------------------------------------------------------------------

#br 2020-01
#This function is only meant to save some typing when appending new objects (say ggplots) 
#	to a list. One confusing thing there is that unless "list()" is used, the ggplot objects 
#	are flattened, so the end result is not even close to something that can be printed. 
#	"App" comes from append.
app <- function(x, new) {c(x, list(new))}

#-------------------------------------------------------------------------------


#bogdan romocea 2010-01
#This function deletes leading and trailing spaces, then replaces blanks with NAs.
#	The problem is that less than perfect data may contain a varying number of
#	spaces which (a) waste memory and (b) prevent the effective identification of
#	missing values with the na.strings argument of read.csv() because the number
#	of blank spaces indicating missing values may vary among columns.

blanks2NA <- function(x, factor=TRUE)
{
y <- sub(" +$", "", sub("^ +", "", x))  #remove trailing and leading blanks
y[y == ""] <- NA
if (factor) factor(y) else y
}

#-------------------------------------------------------------------------------

#bogdan romocea 2010-01
#Apply blanks2NA() on all factors from a data frame.

blanks2NA.DF <- function(DF)
{
DFna <- DF
fact <- sapply(lapply(DF, class), function(x) {length(intersect(x, c("factor","character"))) > 0})
if (TRUE %in% fact) for (v in colnames(DF)[fact]) DFna[,v] <- blanks2NA(DF[,v])
DFna
}

#-------------------------------------------------------------------------------

#br 2012-10
#I finally converted most of the previous code for box plots from package "graphics" to "ggplot2".
#	The benefits are primarily aesthetic, as most of the hard work behind ensuring high quality
#	looks and proper layout on pages with multiple plots is now handled automatically by ggplot.
#Revised 2014-06; added limits for the Y scale and cleaned up some syntax

#2015-03: Known ggplot BUG. If a limit is used for the Y axis, then the box plot summary changes...
#	While the only intention is to zoom in for better reading, the summary statistics for the
#	percentiles change as well. This is clearly defective.

boxp <- function(DF, by, v, ttl=NULL, sttl=NULL, log=FALSE, notch=TRUE, outliers=TRUE, ylim=NA)
{
if (!exists('ggplot')) require(ggplot2)
#if (!exists('ddply')) require(plyr)
#the group means and sample sizes
outsize <- if (outliers) 1.5 else 0
summ <- as.data.frame(t(sapply(split(DF[,v], DF[,by]), function(x) c(mean(x, na.rm=TRUE), 
length(which(!is.na(x))), min(x, na.rm=TRUE), max(x, na.rm=TRUE)))))
colnames(summ) <- c('avg','N','min','max') ; summ[,by] <- rownames(summ)
# x <- group_by_at(DF, by) %>%
# 	summarize_at(avg=mean(v))
summ$gmin <- max(min(summ$min), min(ylim), na.rm=TRUE)
summ$label <- paste('N=', summ$N, sep='')
my_opts <- list(geom_boxplot(aes_string(x=by, y=v), alpha=0.6, colour="darkgreen", size=0.8, notch=notch,
	outlier.colour = "gray40", outlier.shape = 1, outlier.size = outsize, varwidth=TRUE),
geom_point(aes_string(x=by, y='avg'), data=summ, colour='red', size=3),
geom_text(aes_string(x=by, y='gmin', label='label'), data=summ, color='navy', size=2.7, vjust=1, hjust=0.5),
labs(title=ttl, subtitle=sttl))
if (log) my_opts <- c(my_opts, list(scale_y_log10(v)))
if (!is.na(ylim[1])) my_opts <- c(my_opts, list(ggplot2::scale_y_continuous(limits=ylim)))
ggplot(DF, aes_string(by, v, group=by)) + my_opts
}

#-------------------------------------------------------------------------------


#br 2005-05
#export data frames in CSV format

csv <- function(obj, outfile, rnames=FALSE, cnames=TRUE, append=FALSE, sep=",", na='NA', quote=TRUE)
{
write.table(obj, file=outfile, append=append, quote=quote, sep=sep, na = na,
dec = ".", row.names=rnames, col.names=cnames)
}

#-------------------------------------------------------------------------------

#br 2013-01
#This function does the equivalent of a SAS PROC SORT followed by a DATA STEP with BY and FIRST/LAST
#	statements (or alternately PROC SORT with nodupkey). Given a data set with duplicate X values,
#	it keeps one row for each X value, the one where the Y value is highest or lowest.
#DF = data frame
#BY = the column(s) defining the cases whose repeated 'sortby' values need to be deduped
#sortby = the column(s) whose values control which rows will be kept - the one with the
#	highest or lowest value
#lowest = if TRUE, the row with the lowest 'sortby' value will be kept; if FALSE the opposite

dedupe <- function(DF, BY, sortby, lowest=FALSE)
{
#Use only the necessary columns for performance reasons. This function could waste
#	a lot of resources if all columns from a big data frame were kept.
uDF <- DF[,c(BY, sortby)] ; uDF$dedid <- 1:nrow(DF)
splitby <- if (length(BY) == 1) uDF[,BY] else apply(uDF[,BY], 1, function(x) paste(x, collapse='_'))
tDF <- lapply(split(uDF, splitby), function(x) sortDF(x, sortby, decreasing=!lowest))
deduped <- sapply(tDF, function(x) x$dedid[1])
DF[deduped,]
}

#-------------------------------------------------------------------------------

#br 2010-11
#Given a character vector, delete all values that are blank or contain only spaces.
#Revised 2013-01 to make it run faster

delBlanks <- function(x)
{
nospaces <- gsub(' ', '', x)
del <- which(nospaces == '')
if (length(del) > 0) x[-del] else x
}

#-------------------------------------------------------------------------------

#br 2006-04
#function which takes a data frame as input and produces a missing values report
#arguments:
#	- DF = the data frame
#	- digits = round the output to this many digits

dfrep <- function(DF, digits=2)
{
cat(paste("Missing values report for data frame ===", deparse(substitute(DF)), "=== (", 
nrow(DF), " rows)", sep=""), "\n")
fact <- sapply(lapply(DF, class), function(x) {length(intersect(x, c("factor","character"))) > 0})
#---numerical vars
if (FALSE %in% fact) {
dfn <- DF[, !fact, drop=FALSE]
nmiss <- sapply(dfn, function(x) {length(which(is.na(x)))})
pctmiss <- round((nmiss / nrow(dfn))*100, 1)
vmin <- sapply(dfn, function(x) {min(x, na.rm=TRUE)})
vmax <- sapply(dfn, function(x) {max(x, na.rm=TRUE)})
vmean <- sapply(dfn, function(x) {mean(x, na.rm=TRUE)})
vmed <- sapply(dfn, function(x) {median(x, na.rm=TRUE)})
distinct <- sapply(dfn, function(x) {length(unique(x[!is.na(x)]))})
rep <- round(data.frame(nmiss, pctmiss, vmin, vmean, vmed, vmax, distinct), digits)
rep <- rep[order(-rep[, "nmiss"]), ]
colnames(rep) <- c("Missing", "PctMissing", "Min", "Mean", "Median", "Max", "UniqueVal")
rep <- cbind(Variable=rownames(rep), rep) ; rep$Variable <- as.character(rep$Variable)
rownames(rep) <- 1:nrow(rep)
} else rep <- "None"
#---factors
if (TRUE %in% fact) {
dff <- DF[, fact, drop=FALSE]
#if there's only one factor in DF,  dff is factor,  and needs to be converted
#	to a data frame for the sapply() to work
if (is.factor(dff) | is.character(dff)) {
	dff <- as.data.frame(dff)
	colnames(dff) <- colnames(DF)[fact == 1]
}
nmiss <- sapply(dff, function(x) {length(which(is.na(x)))})
pctmiss <- round((nmiss / nrow(dff))*100, 1)
distinct <- sapply(dff, function(x) {length(unique(x))})

repf <- data.frame(nmiss, pctmiss, distinct)
repf <- repf[order(-repf[, "nmiss"]), ]
colnames(repf) <- c("Missing", "PctMissing", "UniqueVal")
repf <- round(repf, digits)
repf <- cbind(Variable=rownames(repf), repf) ; repf$Variable <- as.character(repf$Variable)
rownames(repf) <- 1:nrow(repf)
outrepf <- list(missing=repf, summary=summary(dff))
} else outrepf <- "None"
out <- list() ; out[["numerics"]] <- rep ; out[["factors"]] <- outrepf
out
}

#-------------------------------------------------------------------------------

#br 2014-06
#Switched the previous version to ggplot.

dotpl <- function(DF, x, y, sort=TRUE, clr=NULL, size=NULL)
{
if (!exists('ggplot')) require(ggplot2)
if (sort) DF[,y] <- factor(DF[,y], ordered=TRUE, levels=f2c(DF[,y])[order(DF[,x])])
p <- ggplot(DF, aes_string(x, y)) + 
geom_point(aes_string(color=clr, size=size)) +
scale_color_gradient2(low="blue", high="brown")
# if (!is.null(size)) p <- p + geom_point(aes_string(size=size))
# p <- if (is.null(clr)) p + geom_point() else
# 	p + geom_point(aes_string(color=clr)) + scale_color_gradient2(low="blue", high="brown")
p
}

#-------------------------------------------------------------------------------

#br 2018-11
#Summarize a distribution by returning its percentiles and the mean as a data frame, meant
#	to be plotted with ggplot

dtbsumm <- function(x, digits=4)
{
pctl <- quantile(x, probs=(0:100)/100, na.rm=TRUE)
avg <- mean(x, na.rm=TRUE)
avgp <- tail(which(pctl < avg), 1) + 0.5
out <- rbind(data.frame(pctl=0:100, v=pctl, type='q'), data.frame(pctl=avgp, v=avg, type='mean'))
out$vl <- round(out$v, digits)
rownames(out) <- NULL
out
}

#-------------------------------------------------------------------------------

#br 2006-11
#get the frequency distributions or the levels of all factors from a data frame

fact.rep <- function(DF, freq=TRUE)
{
fact <- rep(FALSE, ncol(DF)) ; fact[sapply(DF, class) == "factor"] <- TRUE
rep <- if (freq) lapply(DF[, fact], table) else lapply(DF[, fact], levels)  #name="level"
#rep <- if (freq) lapply(DF[, fact], table) else lapply(DF[, fact], levels)
rep
}

#-------------------------------------------------------------------------------

#br 2008-01
#replace NAs in a factor with a given level

fact.NA.replace <- function(x, newlevel)
{
cat('===Input:', '\n') ; print(summary(x))
temp <- as.character(x)
temp[is.na(temp)] <- newlevel
out <- factor(temp)
cat('===Output:', '\n') ; print(summary(out))
out
}

#-------------------------------------------------------------------------------

#br 2007-04
#save some typing

f2n <- function(x) {as.numeric(as.character(x))}
#-------------------------------------------------------------------------------
n2c <- f2c <- function(x) {as.character(x)}
#-------------------------------------------------------------------------------
c2n <- function(x) {as.numeric(x)}
#-------------------------------------------------------------------------------
t2v <- function(x) {out <- as.numeric(x) ; names(out) <- names(x) ; out}
#-------------------------------------------------------------------------------
n2d <- function(x) {as.Date(x, origin='1970-01-01')}
#-------------------------------------------------------------------------------

#br 2008-10
#given a vector of characters with values like A_B_C_D, recover each particular value (A, B, ...)
#	by splitting the values by BY and keeping the Nth element
#2009-11: Adjusted the code to allow N to take more than 1 value, with the intention of saving
#	resources when more than one value needs to be retrieved from very large vectors.
#2016-10: Added the capability to retrieve the last value from strings of varying lengths. This
#	is helpful when working with something like trees of subfolders, where the depth of each
#	tree may vary.

fromstr <- function(x, BY, N, last=FALSE, asNum=TRUE)
{
if (is.factor(x)) x <- as.character(x)
splt <- strsplit(x, BY, fixed=TRUE)
v <- list()
for (i in 1:length(N)) {
v[[i]] <- if (!last) sapply(splt, function(x) x[[N[i]]]) else sapply(splt, function(x) tail(x, 1)[[1]])
if (asNum) {
	num <- isnum(v[[i]])
	if (num) v[[i]] <- as.numeric(v[[i]])
}
}
if (length(N) == 1) v[[1]] else v
}

#-------------------------------------------------------------------------------

#br 2006-06
#graphical table
#what = the column to plot (say 'pct' or 'freq')
#txt = whether to show the percentages on the plot
#N, byfreq = parameters for collapsing the frequency distribution, to show no more than N
#	rows sorted alphabetically or by frequency
#2012-10: converted to ggplot2

gtbl <- function(x, N=19, byfreq=FALSE, main='', sz=2.8)
{
if (!exists('qplot')) require(ggplot2)
dtb <- tbl(x)
dtb <- tbl.collapse(dtb, N=N, byfreq=byfreq)
#dtb <- dtb[nrow(dtb):1,]
dtb$what <- factor(as.character(dtb$what), ordered=TRUE, levels=rev(dtb$what))  #ensure proper order in ggplot
dtb$label <- paste(round(dtb$pct, 0), '%', sep='')
#if (txt) {
#		dotpl(dtb[, what], as.character(dtb[, "what"]), txt=dtb[, what], xax=xax, tcex=tcex, ...)
#} else {
#		dotpl(dtb[, what], as.character(dtb[, "what"]), xax=xax, tcex=tcex, ...)
#}
#grid()
#mtext(paste("N=", sum(dtb$freq), " / NA=", length(which(is.na(x))), sep=""), col="tomato", line=-1)
#if (what == "freq") title(xlab="Frequency") else title(xlab="Percent")
forlab <- data.frame(x=round(min(dtb$freq) + (max(dtb$freq) - min(dtb$freq)) * 0.4), y=nrow(dtb) + 1/nrow(dtb),
label=paste("N=", sum(dtb$freq), " / NA=", length(which(is.na(x))), sep=""))
ggplot(data=dtb, aes(freq, what)) + geom_point() + 
geom_text(aes(x, y, label=label), data=forlab, color='black', size=3, vjust=0, hjust=0) +
geom_text(aes(freq, what, label=label), data=dtb, color='blue', size=sz, vjust=1, hjust=1) + 
labs(title=main) + scale_y_discrete('')  #, limits=c(0.5, nrow(dtb) + 0.5))
}

#-------------------------------------------------------------------------------

#br 2010-08
#Save some typing when deleting several characters from the same strings.
#x = character vector
#char = the characters to delete
#2013-01: Modified so that char is a vector, instead of a string. This allows
#	deleting more complex strings of characters, instead of strictly one character
#	at a time.
#2013-01: Yet another enhancement: added a new argument, 'with'. If NULL, then the
#	strings in 'char' are deleted. If specified (and must be of the same length as
#	'char'), then the strings from 'char' are replaced with those from 'with'.

ggsub <- function(x, char, with=NULL, fixed=FALSE)
{
if (is.null(with)) with <- rep('', length(char))
for (j in 1:length(x)) {
for (i in 1:length(char)) x[j] <- gsub(char[i], with[i], x[j], fixed=fixed)
}
x
}

#-------------------------------------------------------------------------------

#br 2019-03: Hexagonal binning plots. Rewrote the function initially written back in 2007. The issue
#	is that the hexbin package changed and now produces some really bad charts. Better switch to ggplot.

hbp <- function(DF, x, y, bins=30)
{
ggplot(DF, aes_string(x, y)) + 
geom_hex(bins=bins) +
geom_smooth()
}

#-------------------------------------------------------------------------------

#br 2013-04
#This is helpful to enforce integer breaks for scales where the plots are built with ggplot.

integer_breaks <- function(n = 5, ...)
{
if (!exists('pretty_breaks')) require("scales")
breaker <- pretty_breaks(n, ...)
function(x) {
  breaks <- breaker(x)
	breaks[breaks == floor(breaks)]
}
}

#-------------------------------------------------------------------------------

#br 2010-01
#Determine whether a given character vector contains only numbers and thus can
#	be safely converted to numeric. This is done by checking for the presence
#	of these characters: letters / : _ and - in other places than the start
#2011-02: updated to take care of more than one -, or one - but in the wrong place
#	(such as 2011-02-28)

isnum <- function(x)
{
letters <- length(grep("[a-z]", x, ignore.case=TRUE))
slash <- length(grep("/", x))
semicolon <- length(grep(":", x))
underscore <- length(grep("_", x))
#Testing for - : this is more challenging because it can represent a legitimate part
#	of a number, and care is needed to distinguish minus signs from other uses
#	that indicate a character field, such as 2011-02 or 2011-02-15.
numparts <- sapply(strsplit(x, '-'), function(x) {x <- x[nchar(x) > 0] ; length(x)})
dash <- if (max(numparts) == 1) 0 else 1
(letters + slash  + semicolon + underscore + dash) == 0
}

#-------------------------------------------------------------------------------

#br 2011-03
#Save some typing when creating lists for storing output.
#2022-11: Finally updated to work with multiple vectors, given as a list.

listlike <- function(x)
{
if (is.list(x)) {
nm <- apply(expand.grid(x), 1, function(x) paste(x, collapse='_'))
lst <- vector('list', length(nm))
names(lst) <- nm
} else {
lst <- vector('list', length(x))
names(lst) <- x
}
lst
}

#-------------------------------------------------------------------------------

#br 2010
#delete NULL nodes from a given list

lst.nonull <- function(lst)
{
isnull <- sapply(lst, is.null)
lst[!isnull]
}

#-------------------------------------------------------------------------------

#br 2018-07
#A matrix plot using ggplot. This basically replaces the old matmos() function.
#DF = data frame
#x,y = the matrix rows and columns
#fill, labfill = the values to be plotted and their labels

matp <- function(DF, x, y, fill, ttl=NULL, sz=3, digits=3, lab=NULL)
{
if (!exists('ggplot')) require(ggplot2)
#Well well. For Shiny, the v argument needs to be passed as string and there is no way around that...
isc.x <- isc.y <- isc.fill <- FALSE
try(isc.x <- is.character(x), silent=TRUE)
try(isc.y <- is.character(y), silent=TRUE)
try(isc.fill <- is.character(fill), silent=TRUE)
if (!isc.x) x <- deparse(substitute(x))
if (!isc.y) y <- deparse(substitute(y))
if (!isc.fill) fill <- deparse(substitute(fill))

DF$labfill <- if (is.null(lab)) round(DF[,fill], digits) else DF[,lab]
p <- ggplot(data=DF, aes_string(x=x, y=y, fill=fill)) +
geom_tile() +
labs(title=ttl) +
scale_fill_gradient2(low="blue", high="brown", mid="white") +
geom_text(data=DF, aes_string(x, y, label='labfill'), color="black", size=sz)
p
}

#-------------------------------------------------------------------------------

#br 2006
#Compute the simple or exponential moving average for a given series.
#2018-02: Added a check on the length of the input vector, which was observed to be shorter
#	than the lag in one case. Clearly bad/improper data, but causing an error all the same.

mavg <- function(x, lg, exp=FALSE)
{
ma <- rep(NA, length=length(x))
if (length(x) > lg) {
if (exp) {
	k <- 2/(1+lg)
	ma[lg] <- mean(x[1:lg]) #start = lg-period simple MA
	for (i in (lg+1):length(x)) ma[i] <- ma[(i-1)] + k*(x[i]-ma[i-1])
} else {
	for (i in lg:length(x)) ma[i] <- mean(x[(i-lg+1):i])
}
}
ma
}

#-------------------------------------------------------------------------------

#br 2017-02
#Compute the moving sum for a given series. This is similar to a simple moving average
#	except there is no division, and is meant for dealing with daily balances and accounts.
#	The advantages are the same: smooth out the variations, make the trends easier to grasp.
#It turns out that a significant caveat applies due to sparse data (especially daily). For example Sunday
#	CD closures / openings are present in some weeks but not others. Also the CD renewals and closures
#	can be very sparse at the branch level, given many days with no actions (which do not show up in
#	the summary data unless added explicitly). In these scenarios a lag that works directly with the 
#	observations will not produce similar results, or will be completely unusable. To get around this
#	issue, use a date field to determine exactly which observations need to be included in a moving sum 
#	at every step.
#2019-12: This can now be applied to the future, in addition to the past
#x = the variable of interest (e.g. Balance)
#lg = the lag
#dt = a date field that if given will be used to enforce a rolling window that is "lg" calendar days long.
#	This is appropriate (and in some cases necessary) when a series is sparse (with missing days).

msum <- function(x, lg, dt=NULL, past=TRUE)
{
ms <- rep(NA, length=length(x))
if (length(x) < lg) return(ms)  #guess what, some data like that came up
if (is.null(dt)) {
if (past) {
	for (i in lg:length(x)) ms[i] <- sum(x[(i-lg+1):i])
} else {
	for (i in 1:(length(x)-lg)) ms[i] <- sum(x[(i+1):(i+lg)])
}
} else {
for (i in lg:length(x)) {
	idx <- (i-lg+1):i
	daysdiff <- as.numeric(dt[i] - dt[idx])
	idxkeep <- idx[daysdiff < lg]
	ms[i] <- sum(x[idxkeep])
}
}
ms
}

#-------------------------------------------------------------------------------

#br 2010-11
#Plot a list of ggplot objects on the same page.
#plt = list with ggplot objects
#2017-09: Replaced the prior lower-level implementation using "grid" with a function
#	from "gridExtra". The big difference is the ability to add common titles and
#	axis labels, which was completely missing before, and thus required that related
#	plots each have their own titles and labels (even if very similar).
#	Well... it turns out that due to a bug in a package somewhere, an empty page is 
#	added when saving to PDF. Keep using the old version for now...
#2018-12: Finally fixed the blank page. Also managed to remove the legend from individual
#	plots, and show it just once as a common legend for all plots. The issue is that
#	occasionally some plots share the same legend, which if plotted separately for every
#	chart, wastes a lot of space for no gain.
#2019-08: Added rowFirst. This parameter determines whether the plots on a page are filled by row, or by column.

mp.new <- function(plt, nrow, ncol, top=NULL, topfont=16, bottom=NULL, left=NULL, right=NULL, common.legend=FALSE, lright=TRUE)
{
if (!exists('grid.newpage')) require(grid)
if (!exists('marrangeGrob')) require(gridExtra)
laym <- matrix(seq_len(nrow * ncol), nrow=nrow, ncol=ncol, byrow=TRUE)
if (common.legend) {
#extract the legend from one plot
g <- ggplotGrob(plt[[1]] + theme(legend.position="right"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
#remove the legend from all plots
plt <- lapply(plt, function(x) x + theme(legend.position="none"))
if (lright) right <- legend else bottom <- legend
}
gplt <- lapply(plt, ggplotGrob)  #this gets rid of the first blank page
ptop <- if (is.null(top)) NULL else textGrob(top, gp=gpar(fontsize=topfont))  #custom font size
marrangeGrob(grobs=gplt, layout_matrix=laym, nrow=nrow, ncol=ncol, top=ptop, left=left, bottom=bottom, 
right=right, as.table=TRUE)
# grid.arrange(
# 	top=top,
# 	legend, #ncol=1, heights = unit.c(unit(0, "npc") - lheight, lheight),
# 	do.call(arrangeGrob(layout_matrix=laym), lapply(plt, function(x) x + theme(legend.position="none"))))
}

mp <- mpgg <- function(plt, nrow=1, ncol=1, rowFirst=TRUE)
{
if (!exists('grid.newpage')) require(grid)
loops <- ceiling(length(plt) / (nrow * ncol))
p <- 1
for (l in 1:loops) {
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow, ncol)))
if (rowFirst) {
	for (r in 1:nrow) for (c in 1:ncol) {
		if (p > length(plt)) break
		try(print(plt[[p]], vp=viewport(layout.pos.row=r, layout.pos.col=c)))
		p <- p + 1
	}
} else {
	for (c in 1:ncol) for (r in 1:nrow) {
		if (p > length(plt)) break
		try(print(plt[[p]], vp=viewport(layout.pos.row=r, layout.pos.col=c)))
		p <- p + 1
	}		
}
}
}

#-------------------------------------------------------------------------------

#br 2006-08
#print a warning if x contains NAs or NaNs

na.test <- function(x)
{
if (length(na.omit(x)) != length(x)) cat(paste("===WARNING===", deparse(substitute(x)), 
"contains NAs and/or NaNs."), "\n")
all.NA(x)  #report whether all values are NA
}

#-------------------------------------------------------------------------------

#br 2004-11
#function which prints the current time and optionally a message
#purpose: estimate the time to completion of a for/while/repeat loop

now <- function(message="", after="\n") 
{
cat(paste(message, format(Sys.time(), "%H:%M:%S")), after)
}

#-------------------------------------------------------------------------------

#br 2007-03
#this function returns the largest objects from the global environment
#purpose: find out quickly which objects to delete to release memory
#all = whether to report all objects and their class, or not

obj <- function(envir=1, all=FALSE)
{
ob <- ls(envir)
osize <- oclass <- vector()
for (o in ob) {
osize[o] <- round(object.size(get(o)) / (1024^2), 2)  #in MB
oclass[o] <- class(get(o))[1]
}
if (!all) {
osize <- osize[oclass != "function"]  #exclude functions
osize <- osize[osize > 0]  #ignore very small objects such as constants, small vectors etc
sort(osize, decreasing=TRUE)
} else {
out <- split(osize, oclass)
lapply(out, function(x) sort(x, decreasing=TRUE))
}
}

#-------------------------------------------------------------------------------

#BR 2019-04
#Take a long string and wrap it to a certain width before printing. This is meant to make the
#	generated syntax (much) more user-friendly.
printWrap <- function(x, width=90, tab=TRUE)
{
xp <- strwrap(x, width)
for (i in 1:length(xp)) if (tab) cat(paste0('\t', xp[i]), '\n') else cat(xp[i], '\n')
}

#-------------------------------------------------------------------------------

#br 2011-07
#Replaced the former version written in base graphics in 2005-11 with a ggplot one. The benefit
#	is more consistent layout and formatting - and in particular not having to adjust the
#	margins for multiple plots per page to make sure enough space is available for titles
#	and labels, as ggplot takes care of that automatically.

qpl <- function(x, ylab='', main='', log=FALSE, full=TRUE)
{
if (!exists('ggplot')) require(ggplot2)
nna <- paste("N=", length(which(!is.na(x))), ' / NA=', round(100*(length(which(is.na(x))) / length(x)), 1), '%', sep='')
allmiss <- na.test(x)
if (allmiss) {
plot(0, type='n', axes=FALSE, xlab='', ylab='', ...) ; box()
mtext(nna, line=-1, cex=0.8)
} else {
y <- if (class(x) == 'Date') as.numeric(x) else x
pct <- quantile(y, probs=(0:100)/100, na.rm=TRUE)
ylim <- if (!full) quantile(y, c(0.01, 0.99), na.rm=TRUE) else range(pct)
if (class(x) == 'Date') ylim <- as.Date(ylim, origin='1970-01-01')

rng <- diff(range(pct))
#set the number of digits to show in the labels
if (rng < 10) {dgt <- 3} else if (rng < 100) {dgt <- 2} else if (rng < 1000) {dgt <- 1} else dgt <- 0
dpct <- data.frame(Percentile=as.numeric(gsub('%','',names(pct))), X=pct, label=round(pct, dgt))
if (class(x) == 'Date') dpct$label <- as.Date(round(dpct$label, 0), origin='1970-01-01')
sdpct <- dpct[dpct$Percentile %in% (10*c(0:4, 6:10)),]
med <- dpct[dpct$Percentile == 50,]
avg <- mean(y, na.rm=TRUE)
davg <- data.frame(Percentile=dpct$Percentile[max(which(pct <= avg))], X=avg, label=round(avg, dgt))
if (class(x) == 'Date') davg$label <- as.Date(round(davg$label, 0), origin='1970-01-01')
forlab <- data.frame(Percentile=42, X=max(y, na.rm=TRUE), label=nna)
sameopt <- c(geom_step(color='gray50'),
geom_point(aes(Percentile, X), data=sdpct, colour='blue', size=2),
geom_text(aes(Percentile, X, label=label), data=sdpct[-nrow(sdpct),], color='blue', size=3, vjust=1, hjust=0),
geom_text(aes(Percentile, X, label=label), data=sdpct[nrow(sdpct),], color='blue', size=3, vjust=1, hjust=1),
geom_point(aes(Percentile, X), data=davg, colour='red', size=3),
geom_text(aes(Percentile, X, label=label), data=davg, color='red', size=3, vjust=-1, hjust=1),
geom_point(aes(Percentile, X), data=med, colour='black', size=3),
geom_text(aes(Percentile, X, label=label), data=med, color='black', size=3, vjust=1, hjust=0),
geom_text(aes(Percentile, X, label=label), data=forlab, color='black', size=3, vjust=0, hjust=0))
if (log) {
ggplot(dpct, aes(Percentile, X)) + sameopt + scale_y_log10(ylab, limits=ylim) + labs(title=main)
} else {
if (class(x) == 'Date') 
	ggplot(dpct, aes(Percentile, X)) + sameopt + 
		#scale_y_date(ylab, limits=ylim) + 
		labs(title=main) else
	ggplot(dpct, aes(Percentile, X)) + sameopt + ggplot2::scale_y_continuous(ylab, limits=ylim) + labs(title=main)
}
}
}

#-------------------------------------------------------------------------------

#br 2013-01
#This function takes a date field and returns a flag in the format YYYYQN, where YYYY is the year
#	and N is the quarter (1-4).

quarterDate <- function(x)
{
mth <- c2n(format(x, '%m'))
q <- rep(1, length(mth))
q[mth >= 4 & mth <= 6] <- 2
q[mth >= 7 & mth <= 9] <- 3
q[mth >= 10 & mth <= 12] <- 4
out <- paste(format(x, '%YQ'), q, sep='')
out[is.na(x)] <- NA
out
}

#-------------------------------------------------------------------------------

#br 2009-06
#I just can't remember the word 'clipboard'.

read.mem <- function(x=NULL, ...)
{
DF <- read.delim(file='clipboard', na.strings=c(".","","NA"), ...)
DF
}

#-------------------------------------------------------------------------------

#bogdan romocea 2007-01
#this function imports files exported from SAS in pipe-delimited format
#NB: I got stung twice, quite seriously, by two default values in read.table() - na.strings
#	and comment.char; so to avoid surprises, read the documentation for read.table(), and also
#	prepare (and compare) the missing value reports in SAS and R

read.pipe <- function(fname, sep="|", na=c(".","","NA"), ...)
{
DF <- read.table(file=fname, header=TRUE, sep=sep, quote="\"", dec=".", na.strings=na, 
comment.char="", fill=TRUE, ...)
DF
}

#-------------------------------------------------------------------------------

#br 2019-05
#Save some typing when creating an ordered factor

reord <- function(x, lev)
{
factor(as.character(x), ordered=TRUE, levels=lev)
}

#-------------------------------------------------------------------------------

#br 2012-03
#Finally put together a function that returns a Windows path in the format needed
#	for R and the Korn Shell.
#2018-12: No more Korn shell, after the Windows 10 upgrade.
#2019-06: Added paths for Python

Rpath <- function(x)
{
cat('paste the Windows path below:', '\n')
path <- scan("", "", n=1)
fspath <- gsub('\\\\', '/', path)
pypath <- gsub('\\\\', '\\\\\\\\', path)  #Wow. Trash, but it works
cat('- path for R:', '\n')
cat(fspath, '\n')
cat('- path for Python:', '\n')
cat(pypath, '\n')
#cat('- path for Korn Shell:', '\n')
#cat(paste('/net', substr(fspath, 2, nchar(fspath)), sep=''), '\n')
#list(formatA=path, formatB=fspath, Korn=paste('/net', substr(fspath, 2, nchar(fspath)), sep=''))
}

#-------------------------------------------------------------------------------

#br 2006-11
#This function stacks a list of data frames as a SAS DATA STEP with a SET statement does.
#	The resulting data frame contains all columns from all data frames, with NAs inserted
#	for columns that may not exist in some data frames.
#lst = list of data frames
#2018-11: If names=TRUE, then the list names are added as a column

rstack <- function(lst, names=FALSE, col='id')
{
if (length(lst) == 0) return(NULL)
if (unique(sapply(lst, function(x) class(x)[1])) == 'numeric') return(do.call('rbind', lst))
#---add columns to some data frames if needed
allnames <- unique(unlist(lapply(lst, colnames)))
for (i in 1:length(lst)) {
if (nrow(lst[[i]]) == 0) next
miss.cols <- setdiff(allnames, colnames(lst[[i]]))
if (length(miss.cols) > 0) for (m in miss.cols) lst[[i]][,m] <- NA
if (names) lst[[i]][,col] <- names(lst)[i]
}
out <- do.call('rbind', lst)
rownames(out) <- NULL
out
}

#-------------------------------------------------------------------------------

#br 2008-04
#sort a data frame by any number of columns, in the same order (all ascending or all descending)
#DF = a data frame
#BY = vector with the names of the columns to be used for sorting

sortDF <- function(DF, BY, decreasing=FALSE)
{
eval(parse(text=paste('ord <- order(', paste(paste("DF[,'", BY, "']", sep=''), collapse=', '), 
', decreasing=', decreasing, ')')))
DF[ord,]
}

#-------------------------------------------------------------------------------

#br 2004-12
#function to print several descriptive statistics
#fmt = if "B" (big), format output for big numbers; "R" = regular

sq <- function(x, fmt="R", label="Value", digits=2)
{
qtl <- as.vector(c(0,1,5,25,50,75,95,99,100))	#the quantiles
a <- rep(NA, length(qtl)+3)
avg <- mean(x, na.rm=TRUE)
avgtr <- mean(x, na.rm=TRUE, trim=0.05)
sde <- sd(x, na.rm=TRUE)
if (fmt == "R") a <- round(c(length(which(!is.na(x))), length(which(is.na(x))), avg, avgtr,
sde, sde/avg, as.vector(quantile(x, probs=qtl/100, na.rm=TRUE))), digits=digits)
if (fmt == "B") a <- base::prettyNum(round(c(mean(x), as.vector(quantile(x, probs=qtl/100)),
sum(x), length(x)), digits=digits), big.mark = ",")
nm <- c('N','Missing','mean','TrMean5p','sd','cv',paste('Q', qtl, sep=''))
names(a) <- nm
a
}

#-------------------------------------------------------------------------------

#br 2015-10
#Compute the position of the labels in stacked bar charts. This is helpful for plotting the figures
#	behind stacked bar charts done with ggplot.
#2016-10: Replaced the initial BY field, initially just the date, with a potential combination of
#	fields. This was prompted by the need to properly compute the position of the labels when
#	plotting by date but also facetting by another variable. If the facetting variable is not 
#	included then the labels will be positioned incorrectly.
#2016-11: The prior syntax stopped working, because in ggplot2 v2.2.0 the bars are stacked in the
#	reverse order of the grouping, so the stack order matches the legend. Turns out the fix was
#	quite easy and required only changing the sort order to decreasing.
#by = a vector with the name of the date variable to be plotted on the X axis, plus any other BY variable
#	such as one used for facetting
#class = the variable with the groups to be stacked
#var = the variable to be plotted

StackedBarLabelPos <- function(DF, by, class, var)
{
BY <- if (length(by) == 1) DF[,by] else apply(DF[,by], 1, function(x) paste(x, collapse='_'))
rstack(lapply(split(DF, BY), function(x) {
tmp <- sortDF(x, class, decreasing=TRUE)
tmp[,paste(var, '_lab', sep='')] <- cumsum(tmp[,var]) - tmp[,var] / 2
tmp
}))
}

#-------------------------------------------------------------------------------

#br 2005-04
#function that customizes the output from table() to make it more user-friendly
#2024-04: Rewritten to work with the forcats package, plus a few other long-needed improvements. For example
#    it is quite exciting that if a column name is not provided, then this function will output a list of
#    frequency distributions for all factor and character columns.
#Weird bug: the new pipe operator |> does not work in the old R version on the SAS server.

tbl <- function(df, v='', sort=FALSE, name='Count', n=30)
{
isc.v <- FALSE
try(isc.v <- is.character(v), silent=TRUE)
if (!isc.v) v <- deparse(substitute(v))
if (!v == '') {  #if v is given, then report just that
   out <- df |>
       dplyr::mutate(tmp.tocount = forcats::fct_lump_n(!!sym(v), n = n, other_level = 'z.All Others')) |>
       dplyr::count(tmp.tocount, sort=sort, name=name) |> dplyr::rename(!!v := 'tmp.tocount')
   out$Pct <- round(out[, name] / sum(out[, name]), 3)
} else {  #otherwise produce a summary for all factors and characters in the data frame
   mdf <- df |> dplyr::select(tidyselect::where(~ is.factor(.x) | is.character(.x)))
   out <- listlike(colnames(mdf))
   for (vc in colnames(mdf)) {
       ot <- df |>
           dplyr::mutate(tmp.tocount = forcats::fct_lump_n(!!sym(vc), n = n, other_level = 'z.All Others')) |>
           dplyr::count(tmp.tocount, sort=sort, name=name) |> dplyr::rename(!!vc := 'tmp.tocount')
       ot$Pct <- round(ot[, name] / sum(ot[, name]), 3)
       out[[vc]] <- ot
       # print(ot, n = min(nrow(ot)-1, n+5))
       print(ot)
   }
}
return(invisible(out))
}

#-------------------------------------------------------------------------------

#br 2013-04
#This function creates a Power Point-like text slide with ggplot. The slide can be useful to
#	annotate subsequent charts, where the explanations would be too long to fit in the title
#	and axis labels.

textSlide <- function(label='', size=5, boxcol='black', txtcol='black', backcol='white', wrap=25)
{
if (!exists('ggplot')) require(ggplot2)
mydf<-data.frame(x=c(0,1), y=c(0,1)) 
ggplot(data=mydf, aes(x, y))+ 
theme(axis.ticks=element_blank(), axis.text=element_blank(), axis.title=element_blank(), 
	panel.grid=element_blank(), panel.background=element_blank()) + 
geom_rect(aes(xmax=1, ymax=1, xmin=0, ymin=0), colour=boxcol, fill=backcol) + 
annotate(x=0.5, y=0.58, geom="text", label=paste(strwrap(label, wrap, prefix='\n'), collapse=' '), 
	size=size, color=txtcol)  #, vjust=0.5
}

#-------------------------------------------------------------------------------

#br 2007-06
#Return the top n rows of a data frame or matrix, sorted by a variable
#2010-03: Added the capability to return the highest/lowest values of a vector.

top <- function(DF, v, n=30, desc=TRUE, rn=TRUE) 
{
if (is.vector(DF)) {
head(sort(DF, decreasing=desc), n)
} else {
if (!is.null(DF)) {
	tDF <- if (desc) DF[order(-DF[,v]),] else DF[order(DF[,v]),]
	if (!rn) rownames(tDF) <- NULL
	head(tDF, n)
}
}
}

#-------------------------------------------------------------------------------

#br 2006-06
#open the pdf/png device

topdf <- function(fname, width=19.2, height=10.8, landscape=TRUE)
{
if (landscape) {
pdf(paste(fname, ".pdf", sep=""), width=width, height=height)
} else {
pdf(paste(fname, ".pdf", sep=""), width=height, height=width)
}	
}

topng <- function(fname, width=1275, height=960, landscape=TRUE)
{
if (landscape) {
png(paste(fname, "%03d.png", sep=""), width=width, height=height)
} else {
png(paste(fname, "%03d.png", sep=""), width=height, height=width)
}
}

#-------------------------------------------------------------------------------

#br 2011-04
#Take a character vector with sentences, paragraphs etc and return the individual words.
#	The words in the input vector are assumed to be separated by spaces. If not, then use
#	the sep= argument to specify the separator.
#2013-01: It turns out that quotes within the strings impact the division into words, as
#	quoted expressions and even sentences will not be split. So remove those beforehand.

v2w <- function(x, sep=' ')
{
if (is.null(x)) return(NULL)
x <- gsub('\"', '', x)
con <- textConnection(x)
words <- scan(con, what="character", quiet=TRUE, allowEscapes=TRUE, sep=sep)
close(con)
words
}

#-------------------------------------------------------------------------------

#br 2007-05
#plot a named vector
#2012-07: updated to use ggplot2

vplot <- function(x, label=names(x), log='', sort=FALSE, size=2, flip=FALSE, ...) 
{
if (is.table(x)) {tmp <- as.numeric(x) ; names(tmp) <- names(x) ; x <- tmp}
if (!exists('qplot')) require(ggplot2)
if (sort) x <- sort(x)
p <- qplot(1:length(x), x, log=log, geom='point') + 
geom_text(aes(1:length(x), x, label=label), color='navy', size=size, vjust=0, hjust=1.2) +
geom_point(shape='.')
if (flip) p + coord_flip() else p
}

#-------------------------------------------------------------------------------


#br 2013-01
#Retrieve the capitalized words from a given character vector. Also do some cleaning up.

capWords <- function(x)
{
words <- v2w(x)
wd <- words[substr(words, 1, 1) %in% c(LETTERS)]
#remove possessives
pos <- grep("'s", wd)
if (length(pos) > 0) {
#make sure 's are the last 2 characters
upto <- nchar(wd[pos])
last2 <- substr(wd[pos], upto-1, upto)
todo <- which(last2 == "'s")
if (length(todo) > 0) wd[pos[todo]] <- substr(wd[pos[todo]], 1, upto[todo] - 2)
}
#remove punctuation signs
ggsub(wd, c(',', '.', ';', "'"))
}

#-------------------------------------------------------------------------------

#br 2007-06, found on R-help (posted by jim holtman <jholtman <at> gmail.com>)
#given a vector x, this function produces a list with the consecutive group assignment
#	(where each series of consecutive numbers is assigned the same ID), and a number
#	that indicates the length of the longest series of consecutive numbers

consecutive <- function(x)
{
group <- c(0, cumsum(diff(x) != 1)) + 1
#tb <- table(group)
#longest <- as.numeric(names(tb)[which.max(tb)])
#longestrng <- range(which(group == longest))
list(series=group, max=max(table(group)))  #rng=longestrng
}

#-------------------------------------------------------------------------------

#br 2012-03
#Convert temperature from Fahrenheit to Celsius

F2C <- function(f)
{
tc <- (5/9) * (f - 32)
cat(paste(f, 'F = ', round(tc, 1), 'C', sep=''), '\n')
}

#-------------------------------------------------------------------------------

#br 2010-11
#Run grep for a vector of strings. The objective is to determine where any of the given strings appear.
#x = character vector
#what = vector with the strings to match
#2013-01: Added a new argument, any. If TRUE, the function returns the places where ANY of the given
#	strings were found. If FALSE, it returns the places where ALL of the 'what' strings were found.
#	This is intended to robustly identify the start location for sections of interest in longer
#	articles, where multiple strings may be needed to identify a single line.

ggrep <- function(x, what, any=TRUE)
{
ag <- vector('list', length(what))
for (i in 1:length(what)) ag[[i]] <- grep(tolower(what[i]), tolower(x), fixed=TRUE)  #ignore.case=TRUE
if (any) {
return(sort(unique(unlist(ag))))
} else {
tb <- table(unlist(ag))
tb <- tb[which(tb == length(what))]
return(c2n(names(tb)))
}
}

#-------------------------------------------------------------------------------

#br 2005-09
#get simple or log returns
#2009-10: switched to base2 logarithms, to make interpreting the returns easier

rets <- function(x, log=TRUE)
{
if (log) c(0, log2(x[-1] / x[-length(x)])) else c(0, (x[-1] - x[-length(x)]) / x[-length(x)])
}

#-------------------------------------------------------------------------------

#br 2010-12
#Save all functions in an environment.

saveFct <- function(x=0)
{
sr()
tosave <- sort(names(obj(all=TRUE)[['function']]))
save(list=tosave, file='RF', compress=TRUE)
cat(paste('Saved all functions into ', getwd(), '/RF', sep=''), '\n')
}

#-------------------------------------------------------------------------------

#br 2011-05
#This function searches all of my syntax files for a given pattern. While I
#	already have a short and fairly effective bash script based on find and grep, its
#	output isn't terribly user-friendly and I think there's room for better, more
#	effective results. One central idea is that the files don't need to be read from
#	disk every time a search is run - they can be read and saved in advance in an R
#	object, which will speed up searches significantly.
#patt = the string to search
#ext = extension for the files to search, case-insensitive; if provided, only files with
#	the given extension will be searched
#refresh = whether the files should be re-read from the disk; otherwise a copy saved
#	previously is used
#dirs = the folders to search
#exc = the folders to exclude, use NULL if none
#
#2012-02: set up a separate object with the archived code, which does not need to be refreshed
#	mysearch('gaga', refresh=TRUE, dirs='C:/work/0keep/past_code_2004_2010')
#	load('code_curr') ; code_past <- code_curr ; save(code_past, file='code_past_2004_2010')
#Also, added two new arguments, bef and aft, indicating the number of rows to print before
#	and after a match is found. The idea is to provide additional context that can make
#	it unnecessary to open and look into the actual files.

mysearch <- function(patt, ext=NULL, refresh=FALSE, dirs=c('C:/1br/1code'),
    exc=c('C:/1br/1code/misc','C:/1br/1code/R/etc'), bef=0, aft=0, ...)
{
#---read and save all files of interest
if (refresh) {
aF <- listlike(dirs)
for (d in dirs) aF[[d]] <- list.files(d, recursive=TRUE, full.names=TRUE)
aF <- unlist(aF)
if (!is.null(exc)) {  #some exclusions
	del <- ggrep(aF, exc)  
	if (length(del) > 0) aF <- aF[-del]
}
code_curr <- listlike(aF)
for (i in names(code_curr)) code_curr[[i]] <- readLines(i, n=-1)
#names(mycode)[sapply(mycode, length) > 1000]
save(code_curr, file='code_curr', compress=TRUE)
}
#---search
if (!exists('code_past')) load('code_past_2004_2010')
if (!exists('code_curr')) load('code_curr')
mycode <- c(code_curr, code_past)
if (!is.null(ext)) {
    if (!exists('str_locate_all')) require(stringr)
    #This is pretty clumsy but it works. Retrieve the file extensions, to use for subsetting
    ext3 <- str_extract(names(mycode), "\\....")
    ext2 <- str_extract(names(mycode), "\\...")
    ext1 <- str_extract(names(mycode), "\\..")
    repl <- which(is.na(ext3)) ; ext3[repl] <- ext2[repl]
    repl <- which(is.na(ext3)) ; ext3[repl] <- ext1[repl]
    keep <- which(toupper(ext3) == toupper(ext))
    usecode <- mycode[keep]
} else usecode <- mycode
for (i in names(usecode)) {
    found <- grep(patt, usecode[[i]], ignore.case=TRUE, ...)
    if (length(found) == 0) next
    found <- which(usecode[[i]] %in% unique(usecode[[i]][found]))  #dedupe possibly duplicate lines
    cat(paste('======', i), '\n')
    for (f in found) {
    	prt <- max(1, f - bef) : min(length(usecode[[i]]), f + aft, found[found > f] - 1)
    	for (p in prt) cat(usecode[[i]][p], '\n')
    }
    cat('\n')
}
}

#-------------------------------------------------------------------------------

#br 2011-04
#"Standardize" a given a character vector by removing the stop words, punctuation signs and
#	extra spaces, and converting to lower case.

stdTxt <- function(x)
{
#remove stop words, copied from tm
x <- gsub(sprintf("\\b(%s)\\b", paste(stopwords("english"), collapse = "|")), "", tolower(x))
x <- gsub("[[:punct:]]+", "", x)
x <- gsub("[[:space:]]+", " ", x)
x
}

#-------------------------------------------------------------------------------

#br 2019-08
#Save some typing when converting fields imported from Excel or CSV, from factor or character, to date.
#	The default format corresponds to date9. in SAS, and is the one I have been using most.

todate <- function(x, fmt='%d%b%Y')
{
as.Date(as.character(x), fmt)
}

#-------------------------------------------------------------------------------

#br 2023-05
#Save some typing when calculating WeekStarting and MonthStarting from a date vector. The week is defined as starting on Monday
#x = a date field
WeekStarting <- function(x) { x - (as.numeric(format(x, '%u')) - 1) }  #%u means Monday=1
MonthStarting <- function(x) { as.Date(paste(format(x, '%Y-%m-'), '01', sep='')) }

#-------------------------------------------------------------------------------

#br 2020-01
#This function saves some typing when converting data from wide to long. This functionality is
#	very good to have when multiple columns from a table are to be plotted on the same chart,
#	using ggplot of course.
wide2long <- function(DF, id.vars, measure.vars, groupname='Group', varname='Value')
{
if (!exists('melt')) require(reshape2)	
melt(DF,
id.vars=id.vars,  # ID variables: all the variables to keep but not split apart on
measure.vars=measure.vars,  #the columns with the metrics of interest
variable.name=groupname,  #name of new field with the grouping values
value.name=varname)  #name of new field with the values
}
