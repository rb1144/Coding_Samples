#bogdan romocea 2023 and later
#Various functions related to Shiny apps, in particular modules. Modules are a great way to automate Shiny apps,
#   as the same functionality can be deployed in completely different apps, by changing just their arguments. In other
#   words Shiny modules are very similar to functions - but for Shiny apps (where special considerations apply).

# Table of Contents:
# df2factor <- function(df)
# df2UTF <- function(df)
# DMTD.changes <- function(DF, aggrBY, date, dateField, balField, countField, intField, extra=FALSE)
# LoadToEnvironment <- function(RData, env=new.env()) {
# Shiny.aboutPanel <- function(x=NULL)
# Shiny.AsIsTabServer <- function(id, DF, objname=NULL, columns, ExcelTtl, ExcelFN, caption, amount=NULL, intrt=NULL, count, oneExcelButton=TRUE, 
# Shiny.AsIsTabUI <- function(id, ttl, color, notes='')
# Shiny.atAGlanceTabServer <- function(id, DF, objname=NULL, subsetField=NULL, groupField=NULL,
# Shiny.atAGlanceTabUI <- function(id, ttl, color, subset, group, whichGroup='ALL', BY, whichBY, dateField, 
# Shiny.buildDT <- function(DF, pageLen=15, oneExcelButton, ExcelTtl, ExcelFN, wrap=FALSE, caption='', type='vanilla', exclude=NULL)
# Shiny.ExcelButtons <- function(oneExcelButton=TRUE, ExcelTtl='My Report', ExcelFN='MyReport')
# Shiny.formatDT <- function(DT, myDF, formats, cuts.neg=0)
# Shiny.formatPlot <- function(plt, xfmt=NULL, transx="identity", yfmt=NULL, transy="identity",
# Shiny.getSinceDate <- function(date, goback)
# Shiny.histogramServer <- function(id, dataFile, readFun, selections, caption, formats, suffix=NULL)
# Shiny.histogramTabUI <- function(id, ttl, color, columns, slct, notes='')
# Shiny.SummTabServer <- function(id, DF, objname=NULL, ExcelTtl, ExcelFN, caption, amount=NULL, intrt=NULL, countField, 
# Shiny.summTabUI <- function(id, ttl, color, columns, slct, notes='Use the Checkbox Input, the Filters and the Search box to assemble the report that you need.')
# Shiny.summTrendTabServer <- function(id, DF, objname=NULL, ExcelTtl, ExcelFN, caption, amount=NULL, amount2=NULL, intrt=NULL, 
# Shiny.summTrendTabUI <- function(id, ttl, color, columns, slct, whichData='Date Range', plotSeries='Counts', subset='ALL',

#-------------------------------------------------------------------------------

#br 2023-04
#Finally started to place repeated Shiny statements in functions. This is very helpful to update the About tab on all dashboards from this single place.

Shiny.aboutPanel <- function(x=NULL)
{
tabPanel(
    title = span("About", style = "color: gray"),
    mainPanel(width=10,
              h2('About'),
              h5("The functionality of this Shiny app is made possible by an ecosystem of very flexible tools. The components include but are not limited to:"),  #style = "color:gray"
              HTML('<li>The R Project for Statistical Computing <a href="https://www.r-project.org/about.html" target="_blank">https://www.r-project.org/about.html</a>'),
              HTML('<li>Shiny <a href="https://shiny.posit.co/" target="_blank">https://shiny.posit.co/ </a>'),
              HTML('<li>Shiny Server <a href="https://posit.co/products/open-source/shinyserver/" target="_blank">https://posit.co/products/open-source/shinyserver/</a>'),
              HTML('<li>Tidyverse (ggplot2, dplyr and others) <a href="https://www.tidyverse.org/" target="_blank">https://www.tidyverse.org/</a>'),
              HTML('<li>Plotly <a href="https://plotly.com/r/" target="_blank">https://plotly.com/r/</a>'),
              HTML('<li>DataTables <a href="https://datatables.net/" target="_blank">https://datatables.net/</a>'),
              HTML('<li>Leaflet <a href="https://leafletjs.com" target="_blank">https://leafletjs.com</a><br><br>'),
              HTML('Please reach out to Bogdan at <a href="mailto:bogdan_romocea@yahoo.com">bogdan_romocea@yahoo.com</a> or <a href="https://www.linkedin.com/in/bogdan-romocea" target="_blank">
                   <img src="linkedin.png" title="LinkedIn" width="30" height="30"></a> with any questions or comments.')
    )
)
}

#-------------------------------------------------------------------------------

#br 2023-07
#Finally... a good function for assembling DataTables. The one that I wanted 1 year ago, but wasn't sure how to assemble it back then
#2023-11: Fixed a bug whereby if DF only had a single column, it was converted to factor or whatever - in the absence of drop=FALSE.

Shiny.buildDT <- function(DF, pageLen=15, oneExcelButton, ExcelTtl, ExcelFN, wrap=FALSE, caption='', type='vanilla', exclude=NULL)
{
if (type == 'vanilla') {  #regular DataTable with all common features enabled
   excelButtons <- Shiny.ExcelButtons(oneExcelButton, ExcelTtl, ExcelFN)
   classDT <- if (wrap) "display compact" else "display nowrap compact"  #whether to wrap long strings
   out <- DT::datatable(DF[, setdiff(colnames(DF), exclude), drop=FALSE], 
                        class = classDT, 
                        filter = "top", rownames=FALSE, 
                        extensions = "Buttons", 
                        options = list(orderClasses=TRUE, pageLength=pageLen, dom = "Blfrtip", buttons = excelButtons ),
                        caption = caption )
} else if (type == 'print') {  #simple version just for printing - no filters, etc.
   out <- DT::datatable(DF[, setdiff(colnames(DF), exclude), drop=FALSE], 
                        rownames=FALSE, options = list(dom = 't'),
                        caption = caption )
}
out    
}

#-------------------------------------------------------------------------------

#br 2023-05
#After the recent upgrade of our Shiny Server as well as of the R packages on the server, Nick noticed that the "Excel" button created 
#    only formatted reports - where the decimals were lost. As a fix, I researched and added a second Excel button which allows the user 
#    to download the raw data. This function centralizes that functionality in a single place.

Shiny.ExcelButtons <- function(oneExcelButton=TRUE, ExcelTtl='My Report', ExcelFN='MyReport')
{
if (oneExcelButton) {
    list( list(extend = 'excel', text = "Excel", title = ExcelTtl, messageTop = NULL, filename = paste0(ExcelFN, Sys.Date())))
} else {
    list( list(extend = 'excel', text = "Excel - Formatted", title = paste(ExcelTtl, '(formatted: decimals may be rounded)'), 
               messageTop = NULL, filename = paste0(ExcelFN, Sys.Date())),
          list(extend = 'excel', text = "Excel - All Decimals", title = paste(ExcelTtl, '(not formatted: raw data)'), 
               exportOptions = list(orthogonal = "export"),
               messageTop = NULL, filename = paste0(ExcelFN, Sys.Date())))
}
}

#-------------------------------------------------------------------------------

#br 2023-07
#It turns out that Shiny returns some errors when it should not (mere formatting for columns that do not exist). To fix those in a general way,
#    assemble a function that checks which fields are present in the data. The great thing about this function is that not only it avoids
#    trivial yet terminal errors, but it also simplifies the existing syntax quite nicely. 
#DT = the datatable object whose columns are to be formatted
#myDF = the dataframe used to assemble DT. This is used to determine which fields are present
#formats = a list of lists that indicates the fields, the formats to be applied (comma, currency, percent), and the number of digits.
#    Note that multiple lists can be used to apply the same format but with different number of digits. The format that I chose also
#    allows specifying the settings for the conditional formatting, which is very cool. For example:
# formats=list(list(c('PctCases', 'WAC'), 'pct', 2), 
#             list(c(countField), 'comma', 0), 
#             list(c(amount), 'curr', 1),
#             list(c(countField), 'style', 'skyblue'),
#             list(c(amount), 'style', 'palegreen'),
#             list(c('WAC'), 'style', 'lightpink') )
#cuts.neg = a vector with the cuts to use for positive and negative formatting. This defaults to 0 meaning red for negative and green for
#    positive, however if given as c(-5, 5) then the values in between would appear in a 3rd color.
#2023-11: Eliminated these warnings which kept surfacing in the context of reactive Shiny objects where some fields were initially all NA,
#    for just a fraction of a second but enough to generate these warnings. The problem was in the style formatting section.
# Warning in min(x) : no non-missing arguments to min; returning Inf
# Warning in max(x) : no non-missing arguments to max; returning -Inf

Shiny.formatDT <- function(DT, myDF, formats, cuts.neg=0)
{
tmp <- DT
for (i in 1:length(formats)) {
   fm <- formats[[i]]
   tofmt <- intersect(fm[[1]], colnames(myDF))
   if (length(tofmt) == 0) next
   if (fm[[2]] == 'pct') {
       tmp <- tmp %>% formatPercentage(tofmt, digits=fm[[3]])
   }
   if (fm[[2]] == 'comma') {
       tmp <- tmp %>% formatRound(tofmt, digits=fm[[3]])
   }
   if (fm[[2]] == 'curr') {
       tmp <- tmp %>% formatCurrency(tofmt, digits=fm[[3]])
   }
   if (fm[[2]] == 'style') {
       #Avoid warnings about min() and max() returning +/-Inf. This syntax works because the colored bars are only 
       #    used one at a time (for different colors), therefore tofmt can be expected to always be a single column.
       if (!all.NA(myDF[, tofmt])) {
           tmp <- tmp %>% formatStyle(tofmt, background = styleColorBar(range(myDF[, tofmt], na.rm=TRUE), fm[[3]]), 
                                  backgroundSize = '100% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
       }
   }
   #This is meant for fields which may have both positive and negative values. In those cases, the approach below fails. To convey both
   #    magnitude and direction, I settled on the font color to separate positive from negative, while the gray bars are proportional
   #    to the absolute values and thus indicate the size.
   #2023-10: Also added the option for a 3rd color, needed for the GDS app. There, small changes (< $5M) appear in gray
   if (fm[[2]] == 'style.neg') {
       values.neg <- if (length(cuts.neg) == 1) c("red", "green") else c("red", "gray", "green")
       tmp <- tmp %>% formatStyle(tofmt, color = styleInterval(cuts = cuts.neg, values = values.neg)) %>%   #fontWeight = "bold"
           formatStyle(tofmt, background = gsub("value", "Math.abs(value)", styleColorBar(abs(myDF[, tofmt]), 'lightgray'), fixed=TRUE),
                       backgroundSize = '100% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
   }
}
tmp
}

#-------------------------------------------------------------------------------

#br 2023-04
#Finally, a function that takes care of the rather convoluted syntax for formatting ggplot scales as dollar, percent or comma. 
#    The syntax is quite ugly but is only repeated once.
#2023-10: Changed the syntax from labels=dollar to labels=label_dollar(), and also added an optional argument for accuracy.
#    This turned out to be required for the new hexagonal binning plots where balances in thousand $ and on the log scale
#    appear with insufficient accuracy.

Shiny.formatPlot <- function(plt, xfmt=NULL, transx="identity", yfmt=NULL, transy="identity",
                            accuracy=c(dollar=NULL, percent=NULL, comma=NULL))
{
if (!is.null(yfmt)) {
   yfmt <- tolower(yfmt)  #just in case
   if (yfmt == 'dollar') {
       plt <- plt + ggplot2::scale_y_continuous(labels=label_dollar(accuracy=accuracy['dollar']), trans=transy)
   } else if (yfmt == 'percent') {
       plt <- plt + ggplot2::scale_y_continuous(labels=label_percent(accuracy=accuracy['percent']), trans=transy)
   } else if (yfmt == 'comma') {
       plt <- plt + ggplot2::scale_y_continuous(labels=label_comma(accuracy=accuracy['comma']), trans=transy)
   }
}

if (!is.null(xfmt)) {
   xfmt <- tolower(xfmt)
   if (xfmt == 'dollar') {
       plt <- plt + ggplot2::scale_x_continuous(labels=label_dollar(accuracy=accuracy['dollar']), trans=transx)
   } else if (xfmt == 'percent') {
       plt <- plt + ggplot2::scale_x_continuous(labels=label_percent(accuracy=accuracy['percent']), trans=transx)
   } else if (xfmt == 'comma') {
       plt <- plt + ggplot2::scale_x_continuous(labels=label_comma(accuracy=accuracy['comma']), trans=transx)
   }
}
plt
}

#-------------------------------------------------------------------------------

#br 2023-07
#This syntax is being used in a couple of places, so better centralize it here
#date = the reference date - go back (or forward, eventually?) from here
#goback = an expected string, standardized across apps, with values like 30 days, YTD, etc.
Shiny.getSinceDate <- function(date, goback)
{
since <- NULL  #this is required to avoid app crashes - in case goback does not match a coded value
if (goback == '7 days') since <- date - 6 else
   if (goback == '15 days') since <- date - 14 else
       if (goback == '30 days') since <- date - 29 else
           if (goback == '90 days') since <- date - 89 else
               if (goback == 'YTD') since <- as.Date(paste0(format(date, '%Y'), '-01-01')) - 1 else
                   if (goback == '12M') since <- date - 364 else
                       if (goback == 'Latest') since <- date else 
                           if (goback == 'ALL') since <- as.Date('1900-01-01')
since
}

#-------------------------------------------------------------------------------

#br 2023-05
#Load an Rdata file into a new environment to avoid side effects. This Rdata file may contain many objects/tables,
#    which is alright, because the Shiny modules have an extra argument for the name of the dataframe that should be used.
LoadToEnvironment <- function(RData, env=new.env()) {
    load(RData, env)
    # return(env[names(env)])
    now(paste('The following objects are present in the Rdata file', RData, ':'))
    print(names(env))
    return(env)
}

#---This can probably be deprecated
#br 2023-05
#Figured out how to load Rdata files reactively
#wait = the number of miliseconds to wait until checking the file modified time. 60 seconds is fine
# get.Rdata <- function(session, wait=60000, dataFile)
# {
# DF <- reactivePoll(wait, session,
#    # This function returns the time that the data file was last modified
#    checkFunc = function() {
#        if (file.exists(dataFile)) {
#            print(file.info(dataFile)$mtime[1])
#            file.info(dataFile)$mtime[1]
#        } else ""
#    },
#    # This function returns the contents of the Rdata file
#    valueFunc = function() {
#    # read.csv(log_file)
#        data <- reactiveVal(value = NULL)
#        n <- new.env()
#        now("loading Rdata reactively")
#        env <- load(dataFile, envir = n)
#        data(n[[names(n)]])
#        # n[[names(n)]]
#    }
# )
# DF
# }

#-------------------------------------------------------------------------------

#br 2023-06: This new module is meant to provide a higher level of abstraction than is provided by the
#    Shiny.summTrendTabServer module. Given some data, apply some subsetting (e.g. MBD, or Retail Banking), then aggregate
#    by date and by the [selected] levels in a given field, at a given frequency (e.g. daily, weekly, monthly),
#    then finally display the resulting series as counts/amounts and percentages.
# ttl, color = title and color for the Tab
#subset = a vector with the Values to use for subsetting. The name of the field to use is specified in the Server module.
#    The first value is used by default. To avoid subsetting, use 'ALL' as the first value
#group, whichGroup = same as subset, plus the default value to use. If ALL then nothing happens
#BY, whichBY = a vector with Field Names, and the default choice. This will split the results by the values present in
#    the BY field chosen
#toplot, whichPlot = the field to plot, and the default value. If Counts, then 'countField' from the server module is used
#    for aggregation. If Amounts, then 'amount' from the server module is used.
#whichSeries = the choice for the series to plot by default. Can be either Regular or Cumulative
#facet, whichFacet = vector with the Field Names to use for facetting, and the default value. For the plots to work, the facet
#    field(s) must have been provided already, through 'subsetField' or 'groupField' or 'BY'. If providing a brand new field
#    then the plots will break, as the data will not have been aggregated that way. Note that all summaries are produced as:
#        group_by_at(fltDF, c(subsetField, groupField, input$by))

Shiny.atAGlanceTabUI <- function(id, ttl, color, subset, group, whichGroup='ALL', BY, whichBY, dateField, 
                             notes='', whichData='30 days',
                             toplot=c('Counts', 'Amounts'), whichPlot='Counts', whichSeries='Regular', 
                             facet='None', whichFacet='None',
                             freq=c('Daily','Weekly','Monthly'), whichFreq='Daily', whichChart='Line')
{
tabPanel(
    title = span(ttl, style = paste("color:", color)),
    sidebarLayout(
        # position="right",
        sidebarPanel(width=2, 
                     selectInput(NS(id, 'subset'), 'Subset:', choices=subset, selected=subset[1]),
                     selectInput(NS(id, 'group'), 'Group:', choices=group, selected=whichGroup),
                     selectInput(NS(id, 'toplot'), label = "Plot", choices = toplot, selected=whichPlot),
                     selectInput(NS(id, 'series'), label = "Series", choices = c('Regular', 'Cumulative'), selected=whichSeries),
                     selectInput(NS(id, "whichData"), label = "Period", choices = c("30 days", "90 days", "YTD", "12M"), selected=whichData),
                     selectInput(NS(id, 'freq'), 'Frequency:', choices=freq, selected=whichFreq),
                     # selectInput(NS(id, 'tocount'), 'Plot:', choices=tocount, selected=tocount[1]),
                     selectInput(NS(id, 'by'), 'BY:', choices=BY, selected=whichBY),
                     # selectInput(NS(id, 'dateField'), 'Date Field:', choices=dateField, selected=dateField[1]),
                     # selectInput(NS(id, names(BY)), 'By:', choices=BY[[1]], selected=BY[[1]][1])
                     selectInput(NS(id, 'chart'), label = "Chart", choices = c('Line', 'Bar'), selected=whichChart),
                     selectInput(NS(id, 'facet'), label = "Facet", choices = facet, selected=whichFacet)
        ),
        mainPanel(width=10,
                  HTML(paste0("<font color='navy' size=2>", notes, "</font>")),
                  plotlyOutput(NS(id, "plot"), height=400), br(),
                  plotlyOutput(NS(id, "pplot"), height=400), br(), br(),
                  div(DT::dataTableOutput(NS(id, "summ")), style = "font-size:95%"), br(), br(),
                  div(DT::dataTableOutput(NS(id, "trending")), style = "font-size:95%")
        )
    )
)
}

#-------------------------------------------------------------------------------

#br 2023-06: Started from Shiny.summTrendTabServer to assemble this new module which is meant to provide
#    pre-canned charts at a higher level of abstraction, where the default settings are already good and
#    the user does not have to go through all those rather granular steps to get to the trends of interest.
#subsetField = a field with values to be used for subsetting. Only records with the values provided by the UI
#    module will be kept. For example, only keep Retail Banking CDs 
#groupField = yet another way to subset the data. This was added due to the need to subset the data twice
#    in some cases. For example keep Retail Banking CDs (one field) + New account openings (another field)
#colors = a vector with colors for the charts, meant to override the default values
#2023-11: Fixed the same problem for Cumulative WAC and WATerm that had surfaced for Amounts long ago.
#    Filling in missing values for intrt and waterm is required so as to not end up with blank cumulative curves.
Shiny.atAGlanceTabServer <- function(id, DF, objname=NULL, subsetField=NULL, groupField=NULL,
                                 ExcelTtl, ExcelFN, caption, amount=NULL, intrt=NULL, waterm=NULL,
                                 countField, addCounter=FALSE, countUnique=FALSE,
                                 oneExcelButton=TRUE, dateField, digits=0, sortBy=NULL, showTotal=TRUE, colors=NULL) 
{
moduleServer(id, function(input, output, session) {
    
    #---First retrieve and aggregate the data 
    #BUT. Since this module is for trending data, then why not aggregate that here as well? Then it would be ready for any selections made later.
    #    The output is a list with 2 elements: summary (no date field - totals) and trending
    myDF <- shiny::reactive({
        #Retrieve the data. If name is NULL then DF is assumed to be a reactive data frame - otherwise it is a reactive environment
        sDF <- if (is.null(objname)) DF() else DF()[[objname]]
        if (addCounter) sDF[, countField] <- 1
        
        #---Calculate the start date
        maxd <- max(sDF[, dateField])
        since <- Shiny.getSinceDate(maxd, input$whichData)
        
        #---Calculate the periods if a different frequency is selected
        if (input$freq == 'Weekly') {
            sDF$WeekStarting <- WeekStarting(sDF[, dateField])
            dateField <- 'WeekStarting'
        }
        if (input$freq == 'Monthly') {
            sDF$MonthStarting <- MonthStarting(sDF[, dateField])
            dateField <- 'MonthStarting'
        }
        
        #---Filter by date
        fltDF <- dplyr::filter(sDF, !!sym(dateField) >= since & !!sym(dateField) <= maxd)
        fltPeriod <- paste(range(fltDF[, dateField]), collapse=' to ')
        
        #---Subset. This is optional - nothing happens if the name of the field to be used for subsetting is not provided,
        #    Or if the subset value is ALL
        if (!is.null(subsetField)) {
            if (tolower(input$subset) != 'all') {
                fltDF <- dplyr::filter(fltDF, tolower( !!sym(subsetField) ) == tolower(input$subset))
            }
        }
        
        #---Yet another subset, by group. This is optional as well
        if (!is.null(groupField)) {
            if (tolower(input$group) != 'all') {
                fltDF <- dplyr::filter(fltDF, tolower( !!sym(groupField) ) == tolower(input$group))
            }
        }
        
        #---Collapse the input$by levels with small amounts or counts
        #Turns out this is going to be pretty big, and ugly. Ignore it for now 
        
        
        #---Aggregate
        #2023-06: The trending data must be adjusted by grouping categories beyond the top 8 or 9 into 'All Others'.
        #    This was prompted by a CD new + renew dataset where the terms really cannot be plotted unless cleaned up first.
        
        #2023-08: Adjusted the group_by_at() fields to remove groupField from the aggregation when its value is ALL. This was 
        #    prompted by the desire to avoid having multiple points/bars for each level of groupField when the chosen value is ALL:
        #    better aggregate everything together then.
        #Actually, WAIT: there is an extra catch there. Simply removing groupField from the aggregation when its value = ALL will
        #    break the facetting functionality. Therefore... collapse groupField but only when it is not used for facetting. In 
        #    addition... to avoid a lot of edits in the existing formulas, simply add an extra column with all values = ALL.
        if (!is.null(groupField)) {
            if (tolower(input$group) == 'all' & input$facet != groupField) {
                groupField <- 'temporaryCollapse'
                fltDF[, groupField] <- 'ALL'
            }
        }
        
        #The summary depends on whether an amount field is present. 
        if (!is.null(amount)) { 
            if (is.null(intrt)) {  #1. Amounts + Counts
                tDF <- group_by_at(fltDF, c(subsetField, groupField, input$by)) %>%
                    dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount))) %>%  #n()
                    rename(!!amount := 'total', !!countField := 'N') %>% dplyr::ungroup() %>%
                    mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
                trenDF <- group_by_at(fltDF, c(dateField, subsetField, groupField, input$by)) %>%
                    dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount))) %>%
                    rename(!!amount := 'total', !!countField := 'N') %>% dplyr::ungroup()
            } else {  
                if (is.null(waterm)) {  #2. Amounts + Counts + WAC
                    tDF <- group_by_at(fltDF, c(subsetField, groupField, input$by)) %>%
                        dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), 
                                  WACost=sum(!!sym(amount) * !!sym(intrt)) / sum(!!sym(amount))) %>%  #n()
                        rename(!!amount := 'total', !!countField := 'N', WAC=WACost) %>% dplyr::ungroup() %>%
                        mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
                    trenDF <- group_by_at(fltDF, c(dateField, subsetField, groupField, input$by)) %>%
                        dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), 
                                  WACost=sum(!!sym(amount) * !!sym(intrt)) / sum(!!sym(amount))) %>%
                        rename(!!amount := 'total', !!countField := 'N', WAC=WACost) %>% dplyr::ungroup()
                } else {  #3. Amounts + Counts + WAC + WATerm
                    tDF <- group_by_at(fltDF, c(subsetField, groupField, input$by)) %>%
                        dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), 
                                  WACost=sum(!!sym(amount) * !!sym(intrt)) / sum(!!sym(amount)),
                                  WgtAvgTerm=sum(!!sym(amount) * !!sym(waterm)) / sum(!!sym(amount))) %>%
                        rename(!!amount := 'total', !!countField := 'N', WAC=WACost, !!waterm := 'WgtAvgTerm') %>% dplyr::ungroup() %>%
                        mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
                    trenDF <- group_by_at(fltDF, c(dateField, subsetField, groupField, input$by)) %>%
                        dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), 
                                  WACost=sum(!!sym(amount) * !!sym(intrt)) / sum(!!sym(amount)),
                                  WgtAvgTerm=sum(!!sym(amount) * !!sym(waterm)) / sum(!!sym(amount))) %>%
                        rename(!!amount := 'total', !!countField := 'N', WAC=WACost, !!waterm := 'WgtAvgTerm') %>% dplyr::ungroup()
                    
                }
                
            }
            
            #add the overall proportions. NB: these only make sense for Counts and Amounts. The result is the % for the BY values.
            #    BUT...!! if input$by is identical to groupField, then all percentages will be 100%... so correct that.
            tDF <- group_by_at(tDF, c(subsetField, groupField)) %>%
                mutate(PctCases=!!sym(countField) / sum(!!sym(countField)), 
                       PctBal=!!sym(amount) / sum(!!sym(amount))) %>% dplyr::ungroup()
            if (groupField == input$by) {
                tDF <- group_by_at(tDF, c(subsetField)) %>%
                    mutate(PctCases=!!sym(countField) / sum(!!sym(countField)), 
                           PctBal=!!sym(amount) / sum(!!sym(amount))) %>% dplyr::ungroup()
            }
            
            # if (nrow(tDF) > 8) {
            #    tDF <- as.data.frame(arrange(tDF, desc(!!sym(amount))))
            #    # collapse <- tDF[9:nrow(tDF), groupField]
            #    tDF$New <- as.character(tDF[, input$by])
            #    tDF$New[9:nrow(tDF)] <- 'z.All Others'
            #    print(tDF[, input$by][tDF$New == 'z.All Others'])
            # }
        } else if (is.null(amount)) {  #4. Only Counts
            # Note that counting unique values is currently implemented only for this section - for the Online Application paths data
            if (!countUnique) {
                tDF <- group_by_at(fltDF, c(subsetField, groupField, input$by)) %>%
                    dplyr::summarize(N=sum(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup()
                trenDF <- group_by_at(fltDF, c(dateField, subsetField, groupField, input$by)) %>%
                    dplyr::summarize(N=sum(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup()
            } else {
                tDF <- group_by_at(fltDF, c(subsetField, groupField, input$by)) %>%
                    dplyr::summarize(N=n_distinct(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup()
                trenDF <- group_by_at(fltDF, c(dateField, subsetField, groupField, input$by)) %>%
                    dplyr::summarize(N=n_distinct(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup()
            }
            
            #add the overall proportions
            tDF <- group_by_at(tDF, c(subsetField, groupField)) %>%
                mutate(PctCases=!!sym(countField) / sum(!!sym(countField))) %>% dplyr::ungroup()
            if (groupField == input$by) {
                tDF <- group_by_at(tDF, c(subsetField)) %>%
                    mutate(PctCases=!!sym(countField) / sum(!!sym(countField))) %>% dplyr::ungroup()
                
            }
            
        }
        
        #2023-07: Add the period to the summary table. This was omitted initially but is important to add
        tDF <- mutate(tDF, Period=fltPeriod, .before=!!sym(subsetField)) %>% rename(!!sym(dateField) := Period)
        
        #2023-07: There is a big BUBU in the trending data: if a category does not have any counts for a certain period, then that period 
        #    shows up as missing in the cumulative series... This is really bad and must be fixed. The good news is that dplyr
        #    already has a function out there for this kind of stuff.
        #2023-11: Missing values for intrt and waterm must be filled in as well
        #         trenDF <- complete(trenDF, !!sym(dateField), !!sym(subsetField), !!sym(groupField), !!sym(input$by))  #doesn't work when groupField is the same as input$by
        #the unique() takes care of cases where groupField and input$by are the same
        trenDF <- addMissingPeriods(as.data.frame(trenDF), BY=unique(c(subsetField, groupField, input$by)), period=dateField, 
                                      vars=c(countField, amount, intrt, waterm), fill=0)
        #2023-05: Add the Cumulative series as well - they can be very helpful for plots
        #WOW. This looks ugly but it works
        #2023-07: Added the formulas for the cumulative WAC and WATerm 
        
        if (!is.null(amount)) {
            if (is.null(intrt)) {  #1. Amounts + Counts
                trenDF <- group_by_at(trenDF, c(subsetField, groupField, input$by)) %>% arrange(!!sym(dateField)) %>%
                    mutate(CumulCases=cumsum(!!sym(countField)), CumulAmt=cumsum(!!sym(amount))) %>% dplyr::ungroup()
            } else {  
                if (is.null(waterm)) {  #2. Amounts + Counts + WAC
                    trenDF <- group_by_at(trenDF, c(subsetField, groupField, input$by)) %>% arrange(!!sym(dateField)) %>%
                        mutate(CumulCases=cumsum(!!sym(countField)), CumulAmt=cumsum(!!sym(amount)), 
                               WGTwac=sum(!!sym(amount) * !!sym(intrt)),
                               CumulWAC=cumsum(WGTwac) / cumsum(!!sym(amount))) %>% dplyr::ungroup()
                } else {  #3. Amounts + Counts + WAC + WATerm
                    trenDF <- group_by_at(trenDF, c(subsetField, groupField, input$by)) %>% arrange(!!sym(dateField)) %>%
                        mutate(CumulCases=cumsum(!!sym(countField)), CumulAmt=cumsum(!!sym(amount)), 
                               # WGTwac=sum(!!sym(amount) * !!sym(intrt)), 
                               # WGTwaterm=sum(!!sym(amount) * !!sym(waterm)),
                               CumulWAC=cumsum(!!sym(amount) * !!sym(intrt)) / cumsum(!!sym(amount)),
                               CumulWATerm=cumsum(!!sym(amount) * !!sym(waterm)) / cumsum(!!sym(amount))) %>% dplyr::ungroup()
                }
            }
        } else {  #4. Counts only
            trenDF <- group_by_at(trenDF, c(subsetField, groupField, input$by)) %>% arrange(!!sym(dateField)) %>%
                mutate(CumulCases=cumsum(!!sym(countField))) %>% dplyr::ungroup()
        }
        
        #2023-07: Also add the proportions. Warning: the cumulative proportions can reach 100% on those dates/periods
        #    when one level does not have any values. To avoid those charts which do look weird, replace 100% with NA
        #    but only in the cumulative series
        #2023-07: Yet another catch, NaN values resulting from division by 0, after adding records for the missing periods
        if (!is.null(amount)) {  #proportions for Counts + Amounts
            trenDF <- group_by_at(trenDF, c(subsetField, groupField, dateField)) %>%
                mutate(PctCases=!!sym(countField) / sum(!!sym(countField)), PctCumulCases=CumulCases / sum(CumulCases),
                       PctBal=!!sym(amount) / sum(!!sym(amount)), PctCumulBal=CumulAmt / sum(CumulAmt)) %>% dplyr::ungroup() %>%
                arrange(!!sym(dateField))
            #Yet another scenario... IF all values are 100%, then the groupField is identical with the BY field. This happened
            #    with the New & Renew CDs where the group (new, renew, All) is the same as the action. If so, then the summary
            #    needs to be adjusted by removing the groupField
            # if (length(unique(trenDF$PctCumulBal)) == 1 & min(trenDF$PctCumulBal) == 1) {  #Wrong. This fails in case of NaNs
            if (groupField == input$by) {
                trenDF <- group_by_at(trenDF, c(subsetField, dateField)) %>%
                    mutate(PctCases=!!sym(countField) / sum(!!sym(countField)), PctCumulCases=CumulCases / sum(CumulCases),
                           PctBal=!!sym(amount) / sum(!!sym(amount)), PctCumulBal=CumulAmt / sum(CumulAmt)) %>% dplyr::ungroup() %>%
                    arrange(!!sym(dateField))
            }
            #This corrects only the remaining few exceptions
            trenDF$PctCumulBal[trenDF$PctCumulBal == 1] <- NA
            trenDF$PctCumulCases[trenDF$PctCumulCases == 1] <- NA
        } else {  #proportions for Counts only
            trenDF <- group_by_at(trenDF, c(subsetField, groupField, dateField)) %>%
                mutate(PctCases=!!sym(countField) / sum(!!sym(countField)), PctCumulCases=CumulCases / sum(CumulCases)) %>% 
                dplyr::ungroup() %>% arrange(!!sym(dateField))
            if (groupField == input$by) {
                trenDF <- group_by_at(trenDF, c(subsetField, dateField)) %>%
                    mutate(PctCases=!!sym(countField) / sum(!!sym(countField)), PctCumulCases=CumulCases / sum(CumulCases)) %>% 
                    dplyr::ungroup() %>% arrange(!!sym(dateField))
            }
            trenDF$PctCumulCases[trenDF$PctCumulCases == 1] <- NA
        }
        
        if (!is.null(sortBy)) tDF <- arrange(tDF, desc(!!sym(sortBy)))
        
        #Set up the info in the tooltips
        # tDF$FYI <- paste0('$', formatC(tDF$Bal, format="f", digits=1, big.mark=","), 'M<br>', round(tDF$WAC*100, 2), '%<br>',
        #                  formatC(tDF$Acct, format="f", digits=0, big.mark=","), ' accts<br>', tDF$MaturityMth)
        # if ('Bank' %in% input$columns) tDF$FYI <- paste0(tDF$FYI, '<br>', tDF$Bank)
        # if ('Term' %in% input$columns) tDF$FYI <- paste0(tDF$FYI, '<br>', tDF$Term, ' term')
        # 
        # head(tDF, 5000)
        list(summ=tDF, trend=trenDF)
    })
    
    output$summ <- DT::renderDataTable(server=FALSE, {
        # if (is.null(trend()$selected)) return()
        out <- Shiny.buildDT(myDF()$summ, pageLen=10, oneExcelButton, ExcelTtl, ExcelFN, exclude=c('FYI'),
                             caption='Overall figures. This is the same data but in tabular format:')
        
        Shiny.formatDT(out, myDF()$summ, formats=list(list(c('PctCases', 'PctBal', 'WAC'), 'pct', 2), 
                                                      list(c(countField), 'comma', 0), 
                                                      list(c('WATerm'), 'comma', 1), 
                                                      list(c(amount), 'curr', 1)))
    })
    
    output$trending <- DT::renderDataTable(server=FALSE, {
        out <- Shiny.buildDT(myDF()$trend, pageLen=10, oneExcelButton, ExcelTtl, ExcelFN, exclude=c('FYI'),
                             caption='Trending view. Same data but in tabular format:')
        
        Shiny.formatDT(out, myDF()$trend, formats=list(list(c('PctCases', 'PctCumulCases', 'PctBal', 'PctCumulBal', 'WAC', 'CumulWAC'), 'pct', 2), 
                                                       list(c(countField, 'CumulCases'), 'comma', 0), 
                                                       list(c('WATerm', 'CumulWATerm'), 'comma', 1), 
                                                       list(c(amount, 'CumulAmt'), 'curr', 1)))
    })
    
    #Ugh. I was hoping for a simpler syntax, however this is also not bad - because it is done only once
    output$plot <- renderPlotly({
        if (is.null(myDF()$trend)) {
            # plt <- textSlide('This trending chart requires some user selections in section (A) Click on the records to select/deselect.', boxcol='white', wrap=75)
            return()
        } else {
            size <- NULL
            if (input$toplot == 'Counts') {
                if (input$series == 'Cumulative') {
                    pttl <- paste(countField, input$subset, input$group, 'Cumulative', sep=' - ')    
                    plotVar <- 'CumulCases'
                } else {
                    pttl <- paste(countField, input$subset, input$group, sep=' - ')
                    plotVar <- countField                
                }
                yfmt <- 'comma'
            } else if (input$toplot == 'Amounts') {
                if (input$series == 'Cumulative') {
                    pttl <-  paste(amount, input$subset, input$group, 'Cumulative', sep=' - ')
                    plotVar <- 'CumulAmt'
                } else {
                    pttl <- paste(amount, input$subset, input$group, sep=' - ')
                    plotVar <- amount
                }
                yfmt <- 'dollar'
            } else if (input$toplot == 'WAC') {
                #use the size argument for WAC and WATerm: because those numbers are susceptible to big swings in the context of (very) low volumes
                if (input$series == 'Cumulative') {
                    pttl <- paste(intrt, input$subset, input$group, 'Cumulative', sep=' - ')
                    plotVar <- 'CumulWAC'
                    size <- 'CumulAmt'
                } else {
                    pttl <- paste(intrt, input$subset, input$group, sep=' - ')
                    plotVar <- intrt
                    size <- 'BALANCE'
                }
                yfmt <- 'percent'
            } else if (input$toplot == 'WATerm') {
                if (input$series == 'Cumulative') {
                    pttl <- paste(waterm, input$subset, input$group, 'Cumulative', sep=' - ')
                    plotVar <- 'CumulWATerm'
                    size <- 'CumulAmt'
                } else {
                    pttl <- paste(waterm, input$subset, input$group, sep=' - ')
                    plotVar <- waterm
                    size <- 'BALANCE'
                }
                yfmt <- 'comma.1'
            }
            
            pttl <- paste('A.', pttl)
            if (tolower(input$facet) == 'none') dofacet <- NULL else dofacet <- input$facet
            
            #which date field to use for plots
            dateFieldPlt <- if (input$freq == 'Weekly') 'WeekStarting' else if (input$freq == 'Monthly') 'MonthStarting' else dateField
            
            #Bar charts do not make sense for WAC or WATerm - because those cannot be added up - so disallow them
            if (input$chart == 'Line' | input$toplot %in% c('WAC', 'WATerm')) {
                plt <- tsline(as.data.frame(myDF()$trend), dateFieldPlt, plotVar, ttl=pttl, N=1, clr=input$by, facet=dofacet, size=size, laby=plotVar)
                # scale_color_brewer(palette = "Dark2")
                if (!is.null(colors)) plt <- plt + scale_color_manual(values = colors)
            } else {
                plt <- tsbar(as.data.frame(myDF()$trend), dateFieldPlt, plotVar, ttl=pttl, N=1, digits=1, stack=input$by, facet=dofacet, laby=plotVar)   
                # scale_fill_brewer(palette = "Dark2")
                if (!is.null(colors)) plt <- plt + scale_fill_manual(values = colors)
            }
            
            plt <- Shiny.formatPlot(plt, yfmt=yfmt, transy="identity")
            
        }
        ggplotly(plt)  #tooltip=c("FYI")
    })
    
    output$pplot <- renderPlotly({
        if (is.null(myDF()$trend)) {
            # plt <- textSlide('This trending chart requires some user selections in section (A) Click on the records to select/deselect.', boxcol='white', wrap=75)
            return()
        } else {
            if (input$toplot == 'Counts') {
                if (input$series == 'Cumulative') {
                    ppttl <- paste(countField, input$subset, input$group, 'Cumulative Poportions', sep=' - ')
                    pplotVar <- 'PctCumulCases'
                } else {
                    ppttl <- paste(countField, input$subset, input$group, 'Proportions', sep=' - ')
                    pplotVar <- 'PctCases'
                }
            } else if (input$toplot == 'Amounts') {
                if (input$series == 'Cumulative') {
                    ppttl <-  paste(amount, input$subset, input$group, 'Cumulative Proportions', sep=' - ')
                    pplotVar <- 'PctCumulBal'
                } else {
                    ppttl <-  paste(amount, input$subset, input$group, 'Proportions', sep=' - ')
                    pplotVar <- 'PctBal'
                }
            } else if (input$toplot %in% c('WAC', 'WATerm')) {
                # pttl <- paste(intrt, input$subset, input$group, sep=' - ')
                # plotVar <- intrt
                #The proportions don't really make sense for WAC and WATerm
                return()
            }
            ppttl <- paste('B.', ppttl)
            
            if (tolower(input$facet) == 'none') dofacet <- NULL else dofacet <- input$facet
            
            #which date field to use for plots
            dateFieldPlt <- if (input$freq == 'Weekly') 'WeekStarting' else if (input$freq == 'Monthly') 'MonthStarting' else dateField
            
            if (input$chart == 'Line') {
                pplt <- tsline(as.data.frame(myDF()$trend), dateFieldPlt, pplotVar, ttl=ppttl, N=1, clr=input$by, facet=dofacet, laby=pplotVar) +
                    ggplot2::scale_y_continuous(labels=percent, trans='identity')
                # scale_color_brewer(palette = "Dark2")
                if (!is.null(colors)) pplt <- pplt + scale_color_manual(values = colors)
            } else {
                pplt <- tsbar(as.data.frame(myDF()$trend), dateFieldPlt, pplotVar, ttl=ppttl, N=1, digits=1, stack=input$by, facet=dofacet, laby=pplotVar) +
                    ggplot2::scale_y_continuous(labels=percent, trans='identity')
                if (!is.null(colors)) pplt <- pplt + scale_fill_manual(values = colors)
            }
        }
        ggplotly(pplt)
    })
    
}
)}

#-------------------------------------------------------------------------------

#br 2023-04: Use this module to insert a tab for a summary report
# ttl, color = title and color for the Tab
# columns = a vector with the names of the columns to make available
# slct = a numeric vector with the indexes of the columns selected by default
Shiny.summTabUI <- function(id, ttl, color, columns, slct, notes='Use the Checkbox Input, the Filters and the Search box to assemble the report that you need.')
{
tabPanel(
    title = span(ttl, style = paste("color:", color)),
    sidebarLayout(
        # position="right",
        sidebarPanel(width=2, 
                     checkboxGroupInput(NS(id, "columns"), 'Report Columns:', choices=columns, selected=columns[slct]) 
        ),
        mainPanel(width=10,
                  HTML(base::paste0("<font color='navy' size=2>", notes, "</font>")),
                  div(DT::dataTableOutput(NS(id, "tbl")), style = "font-size:95%"), br(),
                  fluidRow(column(DT::dataTableOutput(NS(id, "tbltot")), width = 4, style = "font-size:95%"))
        )
    )
)
}

#-------------------------------------------------------------------------------

#br 2023-04: This module reads a data file and produces the output for a summary report aggregated by the columns selected. 
#    A Total report is also provided. This kind of functionality is fantastic because it provides an extremely flexible way
#    to assemble any report that may be needed - from high level to the most granular.
# 2023-05: Multiple improvements, to make this work with data that does not include amounts and WAC. For example the Online
# Applications data from fiVision.
# DF = the reactive data frame to use. I moved the import outside of the module: so that the same data can be used by
#    multiple modules if needed.
# objname = a belated addition that addresses the scenario where DF is an environment and 'name' is the data frame of interest
# ExcelTtl, ExcelFN = the title and file name of the data downloaded to Excel
# caption = caption for the summary report
# amount, intrt = the names of the fields containing the balance and interest rate. These are also used to calculate the WAC.
#     After an update, they can be NULL - meaning only counts will be produced. This was required for use with the fiVision data.
# count = the name to give to the count field. Could be Records, Accts, etc. Note that the n() function is used
# oneExcelButton = determines where exporting the raw data to Excel is offered as an option

Shiny.SummTabServer <- function(id, DF, objname=NULL, ExcelTtl, ExcelFN, caption, amount=NULL, intrt=NULL, countField, 
                            addCounter=FALSE, oneExcelButton=TRUE, amtDigits=1)
{
moduleServer(id, function(input, output, session) {
    myDF <- shiny::reactive({
        #Retrieve the data. If name is NULL then DF is assumed to be a data frame - otherwise it is an environment
        sDF <- if (is.null(objname)) DF() else DF()[[objname]]
        if (addCounter) sDF[, countField] <- 1
        
        #The summary depends on whether an amount field is present
        if (is.null(amount)) {  #1. Counts only
            tDF <- group_by_at(sDF, input$columns) %>% 
                dplyr::summarize(N=sum(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup()
        } else {
            if (!is.null(intrt)) {
                tDF <- group_by_at(sDF, input$columns) %>% 
                    dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), WAC=sum(!!sym(amount) * !!sym(intrt)) / sum(!!sym(amount))) %>%
                    rename(!!amount := 'total', !!countField := 'N') %>% dplyr::ungroup()
            } else {
                tDF <- group_by_at(sDF, input$columns) %>% 
                    dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount))) %>%
                    rename(!!amount := 'total', !!countField := 'N') %>% dplyr::ungroup()
            }
        }
        
        #Set up the info in the tooltips
        # tDF$FYI <- paste0('$', formatC(tDF$Bal, format="f", digits=1, big.mark=","), 'M<br>', round(tDF$WAC*100, 2), '%<br>',
        #                  formatC(tDF$Acct, format="f", digits=0, big.mark=","), ' accts<br>', tDF$MaturityMth)
        # if ('Bank' %in% input$columns) tDF$FYI <- paste0(tDF$FYI, '<br>', tDF$Bank)
        # if ('Term' %in% input$columns) tDF$FYI <- paste0(tDF$FYI, '<br>', tDF$Term, ' term')
        # 
        # print(head(tDF))
        # head(tDF, 5000)
        tDF
    })
    
    output$tbl <- DT::renderDataTable(server=FALSE, {
        if(nrow(myDF()) == 0) {
            return()
        }
        out <- Shiny.buildDT(myDF(), pageLen=15, oneExcelButton, ExcelTtl, ExcelFN, exclude=c('FYI'), caption=caption)
        
        Shiny.formatDT(out, myDF(), formats=list(list(c('WAC'), 'pct', 2), 
                                                 list(c(countField), 'comma', 0),
                                                 list(c(amount), 'curr', amtDigits),
                                                 list(c(countField), 'style', 'skyblue'),
                                                 list(c(amount), 'style', 'palegreen'),
                                                 list(c('WAC'), 'style', 'lightpink') ))
    })
    
    #Calculate the report totals: everything from the main report, after any filters and search, but Without selections (rows clicked)
    output$tbltot <- DT::renderDataTable(server=FALSE, {
        if (nrow(myDF()) == 0) return()
        filtered_data <- input$tbl_rows_all
        #2023-05: Stopped including this CURR_DT field - because it can be deselected. Instead aggregate without assuming the presence of a field
        # tot <- group_by(myDF()[filtered_data, ], CURR_DT) %>% 
        if (is.null(amount)) {
            tot <- dplyr::summarize(myDF()[filtered_data, ], Count=sum(!!sym(countField))) %>% 
                mutate(Contents='Total of Section (A)', .before=Count) %>% 
                rename(!!countField := 'Count') 
        } else {
            if (!is.null(intrt)) {
                #2023-07: Very odd bug. 
                tot <- dplyr::summarize(myDF()[filtered_data, ], Count=sum(!!sym(countField)), total=sum(!!sym(amount)), 
                                 IntRate=sum(!!sym(amount) * WAC, na.rm=TRUE) / max(1, sum(!!sym(amount)))) %>% 
                    mutate(Contents='Total of Section (A)', .before=Count) %>% 
                    rename(!!amount := 'total', !!countField := 'Count', WAC=IntRate) 
            } else {
                tot <- dplyr::summarize(myDF()[filtered_data, ], Count=sum(!!sym(countField)), total=sum(!!sym(amount))) %>% 
                    mutate(Contents='Total of Section (A)', .before=Count) %>% 
                    rename(!!amount := 'total', !!countField := 'Count') 
            }
        }
        out <- Shiny.buildDT(tot, type='print', caption='B. Report Total. This aggregates all records from the Summary Report, after any Filters and/or Search:')
        
        Shiny.formatDT(out, tot, formats=list(list(c('WAC'), 'pct', 2), 
                                              list(c(countField), 'comma', 0),
                                              list(c(amount), 'curr', amtDigits) ))
    })
})
}

#-------------------------------------------------------------------------------

#br 2023-05: With regret, I concluded that I can not really make work the existing module functions work in the way that I wanted:
#    to include or exclude a date or date range input dynamically, based on a function argument. After a bunch of searches I could not
#    find any examples in the documentation or through Google. The Dynamic UI stuff seems a bit complicated but more importantly it 
#    would not address the presence or absence of trending views. As a result I started from the existing modules and customized them 
#    to include the additional date + trending functionality.
# ttl, color = title and color for the Tab
# columns = a vector with the names of the columns to make available
# slct = a numeric vector with the indexes of the columns selected by default
# dateField = the name of the field to be used for inputs and for trending. Assumed to be a date
Shiny.summTrendTabUI <- function(id, ttl, color, columns, slct, whichData='Date Range', plotSeries='Counts', subset='ALL',
                             notes='Use the Inputs, the Filters and the Search box to assemble the report that you need.',
                             freq=c('Daily','Weekly','Monthly'), whichFreq='Daily')
{

tabPanel(
    title = span(ttl, style = paste("color:", color)),
    sidebarLayout(
        # position="right",
        sidebarPanel(width=2, 
                     selectInput(NS(id, "subset"), label = "Subset", choices = subset, selected=subset[1]),
                     checkboxGroupInput(NS(id, "columns"), 'Report Columns:', choices=columns, selected=columns[slct]),
                     selectInput(NS(id, "whichData"), label = "Which Data?", choices = c("Latest", "30 days", "90 days", "YTD", "12M", "Date Range"), selected=whichData),
                     # dateInput(NS(id, "begin"), "Date Range:", value=as.Date(paste0(as.numeric(format(Sys.Date(), '%Y')) - 0, '-01-01'))),  #default to the current Jan 1st
                     # dateInput(NS(id, "end"), label='    to', value=Sys.Date()),
                     conditionalPanel(condition = "input.whichData == 'Date Range'", ns=ns, 
                        dateRangeInput(NS(id, "period"), label="Date Range:",
                                    start=as.Date(paste0(as.numeric(format(Sys.Date(), '%Y')) - 0, '-01-01')),  #default to YTD
                                    end=Sys.Date())),  #format="yyyy-mm", startview="year"
                     selectInput(NS(id, 'freq'), 'Frequency:', choices=freq, selected=whichFreq),
                     selectInput(NS(id, "toplot"), label = "Plot", choices = c('Counts', 'Amounts', 'WAC'), selected=plotSeries),
                     selectInput(NS(id, "chart"), label = "Chart", choices = c('Line', 'Bar'), selected='Line'),
                     selectInput(NS(id, "series"), label = "Series", choices = c('Regular', 'Cumulative'), selected='Regular')
                     # selectInput(NS(id, "periods"), label = "Periods", choices = c('Daily', 'Weekly', 'Monthly'), selected='Regular'),
        ),
        mainPanel(width=10,
                  HTML(paste0("<font color='navy' size=2>", notes, "</font>")),
                  div(DT::dataTableOutput(NS(id, "tbl")), style = "font-size:95%"), br(),
                  fluidRow(column(DT::dataTableOutput(NS(id, "tbltot")), width = 7, style = "font-size:95%")), br(),
                  plotlyOutput(NS(id, "plot"), height=500), br(), br(),
                  div(DT::dataTableOutput(NS(id, "slct")), style = "font-size:95%"), br(), br(),
                  div(DT::dataTableOutput(NS(id, "trending")), style = "font-size:95%")
        )
    )
)

}

#-------------------------------------------------------------------------------

#br 2023-05: Started from the existing SummTabServer Shiny module and added the customizations needed to make it work for trending data.
# DF = the reactive data frame to use. I moved the import outside of the module: so that the same data can be used by
#    multiple modules if needed. Assumed to be a dataset in trending format (with a Date field)
# objname = a belated addition that addresses the scenario where DF is an environment and 'name' is the data frame of interest
# ExcelTtl, ExcelFN = the title and file name of the data downloaded to Excel
# caption = caption for the summary report
# amount, intrt = the names of the fields containing the balance and interest rate. These are also used to calculate the WAC.
#     After an update, they can be NULL - meaning only counts will be produced. This was required for use with the fiVision data.
# amount2 = meant for a second amt field. This emerged when looking at the Flows Of Funds, where the absolute amounts are required
#    for sorting
# count = the name to give to the count field. Could be Records, Accts, etc. Note that the n() function is used. Well, not anymore: 
#    I had to replace n() with sum() to make it work with pre-summarized data where the cases have to be added up. Then I fixed the
#    initial cases (not pre-summarized) by adding a new argument, addCounter
# addCounter = if TRUE, add a new field "count" to the data with all values equal to 1
# countUnique = if TRUE then count the unique values in 'countField'. Hopefully this is the last customization needed out there.....
# oneExcelButton = determines where exporting the raw data to Excel is offered as an option
#
#2023-08: Added another count field. This became needed for the Online Application Paths data with 2 count fields, Pass and Fail.
#    In addition, added the argument pctCountField. If TRUE, then the field Pct'countField' is added, calculated as 
#    countField / (countField + countField2). So this is a very peculiar scenario... implemented for the Online Application
#    Rules data.

Shiny.summTrendTabServer <- function(id, DF, objname=NULL, ExcelTtl, ExcelFN, caption, amount=NULL, amount2=NULL, intrt=NULL, 
                                 subsetField=NULL, countField, countField2=NULL, addCounter=FALSE, countUnique=FALSE,
                                 oneExcelButton=TRUE, dateField, digits=0, sortBy=NULL, showTotal=TRUE, pctCountField=FALSE)
{
moduleServer(id, function(input, output, session) {
    
    #---First retrieve and aggregate the data 
    #BUT. Since this module is for trending data, then why not aggregate that here as well? Then it would be ready for any selections made later.
    #    The output is a list with 2 elements: summary (no date field - totals) and trending
    myDF <- shiny::reactive({
        #Retrieve the data. If name is NULL then DF is assumed to be a reactive data frame - otherwise it is a reactive environment
        sDF <- if (is.null(objname)) DF() else DF()[[objname]]
        if (addCounter) sDF[, countField] <- 1
        
        #If the amount field includes negative values, then set the formatting to that option. This is done here so as to avoid cases where the formatting
        #    changes for different selections, because some groups of periods may include positive values only, while others include negative ones as well.
        amtStyle <- if (min(sDF[, amount], na.rm=TRUE) < 0) 'style.neg' else 'style'  #needed later by the Shiny.formatDT() function
        
        #---Filter by date
        maxd <- max(sDF[, dateField])
        since <- Shiny.getSinceDate(maxd, input$whichData)
        
        #---Calculate the periods if a different frequency is selected
        if (input$freq == 'Weekly') {
            sDF$WeekStarting <- WeekStarting(sDF[, dateField])
            dateField <- 'WeekStarting'
            maxd <- max(sDF[, dateField])
        }
        if (input$freq == 'Monthly') {
            sDF$MonthStarting <- MonthStarting(sDF[, dateField])
            dateField <- 'MonthStarting'
            maxd <- max(sDF[, dateField])
        }
        
        #---Filter by date
        if (input$whichData == 'Date Range') {
            fltDF <- dplyr::filter(sDF, !!sym(dateField) >= input$period[1] & !!sym(dateField) <= input$period[2])
        } else if (input$whichData == 'Latest') {
            fltDF <- dplyr::filter(sDF, !!sym(dateField) == maxd )
        } else {
            fltDF <- dplyr::filter(sDF, !!sym(dateField) >= since & !!sym(dateField) <= maxd)
        }
        
        #---Subset. This is optional - nothing happens if the name of the field to be used for subsetting is not provided,
        #    Or if the subset value is ALL
        if (!is.null(subsetField)) {
            if (tolower(input$subset) != 'all') {
                fltDF <- dplyr::filter(fltDF, tolower( !!sym(subsetField) ) == tolower(input$subset))
            }
        }
        
        #The summary depends on whether an amount field is present. It turns out that there are 3 separate scenarios: just counts, counts + amounts,
        #    and counts + amounts + WAC. Therefore address all of them here...
        if (is.null(intrt) & !is.null(amount)) {  #1. Amounts + Counts
            if (is.null(amount2)) {
                tDF <- group_by_at(fltDF, input$columns) %>% 
                    dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount))) %>%  #n()
                    rename(!!amount := 'total', !!countField := 'N') %>% dplyr::ungroup() %>%
                    mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
                trenDF <- group_by_at(fltDF, c(dateField, input$columns)) %>% 
                    dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount))) %>%
                    rename(!!amount := 'total', !!countField := 'N') %>% dplyr::ungroup()
            } else {
                tDF <- group_by_at(fltDF, input$columns) %>% 
                    dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), total2=sum(!!sym(amount2))) %>%  #n()
                    rename(!!amount := 'total', !!countField := 'N', !!amount2 := 'total2') %>% dplyr::ungroup() %>%
                    mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
                trenDF <- group_by_at(fltDF, c(dateField, input$columns)) %>% 
                    dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), total2=sum(!!sym(amount2))) %>%
                    rename(!!amount := 'total', !!countField := 'N', !!amount2 := 'total2') %>% dplyr::ungroup()
            }
            
        } else if (is.null(amount)) {  #2. Only Counts
            # Note that counting unique values is currently implemented only for this section - for the Online Application paths data
            if (!countUnique) {
                if (!is.null(countField2)) {  #2b. Two Counts fields (e.g. Pass and Fail), Sum Up
                    tDF <- group_by_at(fltDF, input$columns) %>% 
                        dplyr::summarize(N=sum(!!sym(countField)), N2=sum(!!sym(countField2))) %>% rename(!!countField := 'N', !!countField2 := 'N2') %>% dplyr::ungroup() %>% 
                        mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
                    trenDF <- group_by_at(fltDF, c(dateField, input$columns)) %>% 
                        dplyr::summarize(N=sum(!!sym(countField)), N2=sum(!!sym(countField2))) %>% rename(!!countField := 'N', !!countField2 := 'N2') %>% dplyr::ungroup()
                    if (pctCountField) {
                        pctCountFieldName <- paste0('Pct_', countField)
                        tDF <- mutate(tDF, Prptn=!!sym(countField) / (!!sym(countField) + !!sym(countField2))) %>%
                            rename(!!pctCountFieldName := 'Prptn')
                        trenDF <- mutate(trenDF, Prptn=!!sym(countField) / (!!sym(countField) + !!sym(countField2))) %>%
                            rename(!!pctCountFieldName := 'Prptn')
                    }
                    
                } else {  #2a. One Count field, Sum Up. Most frequent scenario
                    tDF <- group_by_at(fltDF, input$columns) %>% 
                        dplyr::summarize(N=sum(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup() %>% 
                        mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
                    trenDF <- group_by_at(fltDF, c(dateField, input$columns)) %>% 
                        dplyr::summarize(N=sum(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup()
                }
            } else {  #2c. Distinct counts - for raw data with repeated values
                tDF <- group_by_at(fltDF, input$columns) %>% 
                    dplyr::summarize(N=n_distinct(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup() %>% 
                    mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
                trenDF <- group_by_at(fltDF, c(dateField, input$columns)) %>% 
                    dplyr::summarize(N=n_distinct(!!sym(countField))) %>% rename(!!countField := 'N') %>% dplyr::ungroup()
            }
            
        } else {  #3. Amounts + Counts + WAC
            tDF <- group_by_at(fltDF, input$columns) %>% 
                dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), WAC=sum(!!sym(amount) * !!sym(intrt)) / sum(!!sym(amount))) %>%
                rename(!!amount := 'total', !!countField := 'N') %>% dplyr::ungroup() %>%
                mutate(Period=paste(range(fltDF[, dateField]), collapse=' to '), .before=input$columns[1])
            trenDF <- group_by_at(fltDF, c(dateField, input$columns)) %>% 
                dplyr::summarize(N=sum(!!sym(countField)), total=sum(!!sym(amount)), WAC=sum(!!sym(amount) * !!sym(intrt)) / sum(!!sym(amount))) %>%
                rename(!!amount := 'total', !!countField := 'N') %>% dplyr::ungroup()
        }
        
        if (!is.null(sortBy)) tDF <- arrange(tDF, desc(!!sym(sortBy)))
        
        #Set up the info in the tooltips
        # tDF$FYI <- paste0('$', formatC(tDF$Bal, format="f", digits=1, big.mark=","), 'M<br>', round(tDF$WAC*100, 2), '%<br>',
        #                  formatC(tDF$Acct, format="f", digits=0, big.mark=","), ' accts<br>', tDF$MaturityMth)
        # if ('Bank' %in% input$columns) tDF$FYI <- paste0(tDF$FYI, '<br>', tDF$Bank)
        # if ('Term' %in% input$columns) tDF$FYI <- paste0(tDF$FYI, '<br>', tDF$Term, ' term')
        # 
        # print(head(tDF))
        # head(tDF, 5000)
        list(summ=tDF, trend=trenDF, amtStyle=amtStyle)
    })
    
    #---Part A. The summary report
    output$tbl <- DT::renderDataTable(server=FALSE, {
        if(nrow(myDF()$summ) == 0) {
            return()
        }
        
        out <- Shiny.buildDT(myDF()$summ, pageLen=15, oneExcelButton, ExcelTtl, ExcelFN, exclude=c('FYI'), caption=caption)
        
        Shiny.formatDT(out, myDF()$summ, formats=list(list(c('WAC', paste0('Pct_', countField)), 'pct', 2), 
                                                      list(c(countField), 'comma', 0),
                                                      list(c(amount, amount2), 'curr', digits),
                                                      list(c(countField), 'style', 'skyblue'),
                                                      list(c(amount), myDF()$amtStyle, 'palegreen'),  #switch to red/green font + gray bars in case of negative values
                                                      list(c('WAC', paste0('Pct_', countField)), 'style', 'lightpink') ))
    })
    
    #---Part B. Calculate the report totals: everything from the main report, after any filters and search, but Without selections (rows clicked).
    #    This part does not have a trending view: only the grand totals are of interest here.
    #2023-05: Showing the totals does not make sense in all cases, for example the Online Applications Path data where the same application IDs
    #    show up many times under multiple rules. Those are counted as unique in section (A) but they should not be summed up here to avoid
    #    confusion. And aggregating the main table from scratch appears to overcomplicate this - so better exclude the total altogether.
    output$tbltot <- DT::renderDataTable(server=FALSE, {
        if (nrow(myDF()$summ) == 0) return()
        if (showTotal) {
            filtered_data <- input$tbl_rows_all
            #2023-05: Stopped including a date field - because it can be deselected. Instead aggregate without assuming the presence of a field
            if (is.null(intrt) & !is.null(amount)) {  #1. Amounts + Counts
                tot <- dplyr::summarize(myDF()$summ[filtered_data, ], Count=sum(!!sym(countField)), total=sum(!!sym(amount))) %>% 
                    mutate(Total='Total of Section (A) after any Filters and/or Search', .before=Count) %>% 
                    mutate(Period=unique(myDF()$Period), .after=Total) %>%
                    rename(!!amount := 'total', !!countField := 'Count') 
                
            } else if (is.null(amount)) {  #2. Only Counts
                tot <- dplyr::summarize(myDF()$summ[filtered_data, ], Count=sum(!!sym(countField))) %>% 
                    mutate(Total='Total of Section (A) after any Filters and/or Search', .before=Count) %>% 
                    mutate(Period=unique(myDF()$Period), .after=Total) %>%
                    rename(!!countField := 'Count') 
                
            } else {  #3. Amounts + Counts + WAC
                tot <- dplyr::summarize(myDF()$summ[filtered_data, ], Count=sum(!!sym(countField)), total=sum(!!sym(amount)), IntRate=sum(!!sym(amount) * WAC, na.rm=TRUE) / sum(!!sym(amount))) %>% 
                    mutate(Total='Total of Section (A) after any Filters and/or Search', .before=Count) %>% 
                    mutate(Period=unique(myDF()$Period), .after=Total) %>%
                    rename(!!amount := 'total', !!countField := 'Count', WAC=IntRate) 
            }
            
            out <- Shiny.buildDT(tot, type='print')
            
            Shiny.formatDT(out, tot, formats=list(list(c('WAC', paste0('Pct_', countField)), 'pct', 3),
                                                  list(c(countField), 'comma', 0),
                                                  list(c(amount, amount2), 'curr', digits) ))
        } else return()
    })
    
    #---Part C and D. Read the rows selected (clicked) from the summary report. Return the selections AND also the trending data
    trend <- shiny::reactive({
        
        sel <- myDF()$summ[input$tbl_rows_selected, , drop=FALSE]
        if (nrow(sel) == 0) return(list(selected=NULL, trend=NULL))
        #2023-08: Wow, big messy bug here. If converted to a data frame, then the indexing insisted on dropping the selection to
        #    a factor when there was only one column. No matter that I had expressly requested otherwise... Finally the solution
        #    was to switch to working with a tibble instead.
        # sel <- as.data.frame(sel)
        # sel <- sel[ , input$columns, drop=FALSE]  # %>% droplevels()
        sel <- select(sel, any_of(input$columns))
        
        #which date field to use
        dateFieldPlt <- if (input$freq == 'Weekly') 'WeekStarting' else if (input$freq == 'Monthly') 'MonthStarting' else dateField
        
        #Now, merge against the trending data
        sltrend <- sortDF(base::merge(myDF()$trend, sel), c(input$columns, dateFieldPlt)) #%>% arrange(!!sym(c(input$columns, dateField)))
        
        #2023-05: Add the cumulative series as well - they can be very helpful for plots
        if (!is.null(amount)) {
            sltrend <- group_by_at(sltrend, input$columns) %>% arrange(!!sym(dateFieldPlt)) %>% 
                mutate(CumulCases=cumsum(!!sym(countField)), CumulAmt=cumsum(!!sym(amount))) %>% dplyr::ungroup()        
        } else {
            sltrend <- group_by_at(sltrend, input$columns) %>% arrange(!!sym(dateFieldPlt)) %>% 
                mutate(CumulCases=cumsum(!!sym(countField))) %>% dplyr::ungroup()
        }
        
        # #Also setup the data for the plots
        # pDF <- slsumm
        # #2023-03: With the Bank now being an ordered factor (for proper sorting in the tables), convert it back to unordered if present, so as to
        # #    avoid the default colors for ordered factors which are not that good
        # if ('Bank' %in% colnames(pDF)) pDF$Bank <- as.factor(as.character(pDF$Bank))
        # 
        sltrend$Subset <- if (length(input$columns) == 1) '' else apply(sltrend[, input$columns[-length(input$columns)] , drop=FALSE], 1, function(x) paste(x, collapse='_'))
        sltrend$Bucket <- sltrend[, input$columns[length(input$columns)], drop=TRUE]
        
        # #add a total for all selections
        # tDF <- group_by(slsumm, Date) %>% dplyr::summarize(Acct=sum(Acct), Balance=sum(Bal), WAC=round(sum(Bal * WAC, na.rm=TRUE) / sum(Bal), 6)) %>% dplyr::ungroup()
        # tDF$Bucket <- 'Total Selected'
        # colnames(tDF)[colnames(tDF) == 'Balance'] <- 'Bal'
        # 
        # #Set up the info in the tooltips
        # tDF$FYI <- paste0('$', formatC(tDF$Bal, format="f", digits=1, big.mark=","), 'M<br>', round(tDF$WAC*100, 2), '%<br>',
        #                  formatC(tDF$Acct, format="f", digits=0, big.mark=","), ' accts<br>', tDF$Date)
        # pDF$FYI <- paste0(pDF$Bucket, '<br>$', formatC(pDF$Bal, format="f", digits=1, big.mark=","), 'M<br>', round(pDF$WAC*100, 2), '%<br>',
        #                  formatC(pDF$Acct, format="f", digits=0, big.mark=","), ' accts<br>', pDF$Date)
        
        list(selected=sel, trend=sltrend, amtStyle=myDF()$amtStyle)  #, pDF=as.data.frame(dplyr::filter(pDF, Date >= since)), tDF=as.data.frame(dplyr::filter(tDF, Date >= since))
    })
    
    output$slct <- DT::renderDataTable(server=FALSE, {
        if (is.null(trend()$selected)) return()
        Shiny.buildDT(trend()$selected, type='print', caption='C. These are categories that you selected above (used to subset the data):')
    })
    
    #---Part D. The trending report, for export to Excel if needed. It's a pity that I simply copied the code from section A - it would have been
    #    nice to modularize it but am sort of unsure how. Also, this copy & paste is happening only once - in the module. So it's not too bad 
    output$trending <- DT::renderDataTable(server=FALSE, {
        if(is.null(trend()$trend)) {
            return()
        }
        out <- Shiny.buildDT(trend()$trend, pageLen=15, oneExcelButton, ExcelTtl, ExcelFN, exclude=c('Subset','Bucket'), caption='D. Trending View')
        
        Shiny.formatDT(out, trend()$trend, formats=list(list(c('WAC', paste0('Pct_', countField)), 'pct', 2), 
                                                        list(c(countField, 'CumulCases'), 'comma', 0),
                                                        list(c(amount, amount2, 'CumulAmt'), 'curr', digits),
                                                        list(c(countField), 'style', 'skyblue'),
                                                        list(c(amount), trend()$amtStyle, 'palegreen'),
                                                        list(c('WAC', paste0('Pct_', countField)), 'style', 'lightpink') ))
    })
    
    #---Part E. The trending charts
    
    output$plot <- renderPlotly({
        if (is.null(trend()$trend)) {
            # plt <- textSlide('This trending chart requires some user selections in section (A) Click on the records to select/deselect.', boxcol='white', wrap=75)
            return()
        } else {
            if (input$toplot == 'Counts') {
                pttl <- if (input$series == 'Cumulative') paste0(countField, ': Cumulative') else countField
                plotVar <- if (input$series == 'Cumulative') 'CumulCases' else countField
                yformat <- 'comma'
            } else if (input$toplot == 'Amounts') {
                pttl <- if (input$series == 'Cumulative') paste0(amount, ': Cumulative') else amount
                plotVar <- if (input$series == 'Cumulative') 'CumulAmt' else amount
                yformat <- 'dollar'
            } else if (input$toplot == 'WAC') {
                pttl <- intrt
                plotVar <- intrt
                yformat <- 'percent'
            }
            
            #which date field to use for plots
            dateFieldPlt <- if (input$freq == 'Weekly') 'WeekStarting' else if (input$freq == 'Monthly') 'MonthStarting' else dateField
            
            if (input$chart == 'Line') {
                plt <- tsline(as.data.frame(trend()$trend), dateFieldPlt, plotVar, ttl=pttl, N=1, clr='Bucket', facet='Subset', laby=plotVar) +
                    scale_color_brewer(palette = "Dark2")
            } else {
                plt <- tsbar(as.data.frame(trend()$trend), dateFieldPlt, plotVar, ttl=pttl, N=1, stack='Bucket', facet='Subset', digits=1, laby=plotVar) +
                    scale_fill_brewer(palette = "Dark2")
            }
            
        }
        plt <- Shiny.formatPlot(plt, yfmt=yformat)
        ggplotly(plt)  #tooltip=c("FYI")
    })
    
})
}

#-------------------------------------------------------------------------------

# 2023-05: Use this module to insert a tab to display a given data frame as-is, without any summaries. 
# ttl, color = title and color for the Tab
# notes = the description to add at the top of the page
Shiny.AsIsTabUI <- function(id, ttl, color, notes='')
{
tabPanel(
    title = span(ttl, style = paste("color:", color)),
    mainPanel(width=10,
              titlePanel(HTML(paste0("<font color='navy' size=2>", notes, "</font>"))),
              div(DT::dataTableOutput(NS(id, "tbl")), style = "font-size:95%"), br(),
              fluidRow(column(DT::dataTableOutput(NS(id, "tbltot")), width = 4, style = "font-size:95%"))
    )
)
}

#-------------------------------------------------------------------------------

# 2023-05: This module produces a DT datatable output for a given reactive data frame. There are no summaries, the data is
# displayed as given. However, a Total report is still produced - to reflect the selections made
# DF = the reactive data frame to use
# columns = which columns to include on the report
# ExcelTtl, ExcelFN = the title and file name of the data downloaded to Excel
# caption = caption for the summary report
# showTotal = gives the ability to include or exclude the Total section - because that may not really make sense (or be needed) with some data
# tofactor = convert all text fields to factors if required. Meant for simple CSV files to be read with read.csv where passing the as.is=FALSE
#    argument to reactiveFileReader() does not work for some reason
# wrap = whether to wrap long text fields
Shiny.AsIsTabServer <- function(id, DF, objname=NULL, columns, ExcelTtl, ExcelFN, caption, amount=NULL, intrt=NULL, count, oneExcelButton=TRUE, 
                            digits=0, showTotal=TRUE, tofactor=FALSE, wrap=FALSE) 
{
moduleServer(id, function(input, output, session) {
    myDF <- shiny::reactive({
        #Retrieve the data. If name is NULL then DF is assumed to be a reactive data frame - otherwise it is a reactive environment
        sDF <- if (is.null(objname)) DF() else DF()[[objname]]
        if (tofactor) sDF <- df2factor(sDF)
        # print(head(sDF))
        sDF
    })
    
    output$tbl <- DT::renderDataTable(server=FALSE, {
        out <- Shiny.buildDT(myDF()[, columns], pageLen=15, oneExcelButton, ExcelTtl, ExcelFN, caption=caption, wrap=wrap)
        Shiny.formatDT(out, myDF(), formats=list(list(c(intrt, 'WAC'), 'pct', 2), 
                                                 list(c(amount), 'curr', digits) ))
    })
    
    #Calculate the report totals: everything from the main report, after any filters and search, but Without selections (rows clicked)
    if (showTotal) {
        output$tbltot <- DT::renderDataTable(server=FALSE, {
            filtered_data <- input$tbl_rows_all
            #2023-05: Stop assuming that the CURR_DT field will always be present
            # tot <- group_by(DF()[filtered_data, ], CURR_DT) %>% 
            tot <- dplyr::summarize(myDF()[filtered_data, ], Count=n(), total=sum(!!sym(amount)), WAC=sum(!!sym(amount) * !!sym(intrt), na.rm=TRUE) / sum(!!sym(amount))) %>% 
                mutate(Contents='Total of Section (A)', .before=Count) %>% 
                rename(!!amount := 'total', !!count := 'Count') %>% dplyr::ungroup()
            
            out <- Shiny.buildDT(tot, type='print', caption='B. Report Total. This aggregates all records from the list above, after any Filters and/or Search:')
            
            Shiny.formatDT(out, tot, formats=list(list(c(intrt, 'WAC'), 'pct', 3),
                                                  list(c(amount), 'curr', digits),
                                                  list(c(count), 'comma', 0)))
        })
    }
})
}

#-------------------------------------------------------------------------------

#br 2023-04
#This is a new module customized for producing and displaying EDA tools such as histograms and percentile plots

Shiny.histogramTabUI <- function(id, ttl, color, columns, slct, notes='')
{
tabPanel(
    title = span(ttl, style = paste("color:", color)),
    sidebarLayout(
        # position="right",
        sidebarPanel(width=2, 
                     selectInput(NS(id, "unit"), 'Unit of Analysis:', choices=c('Account', 'Customer (to follow)'), selected='Account'),
                     radioButtons(NS(id, "plot"), 'Plot Type:', choices=c('Histogram','Percentile'), selected='Histogram'),
                     radioButtons(NS(id, "var"), 'Variable:', choices=columns, selected=columns[slct]),  #selectInput
                     radioButtons(NS(id, "histScale"), 'Scale:', choices=c('Linear','Log'), selected='Linear'),
                     selectInput(NS(id, "groups"), 'Groups:', choices=c('Combine','Compare (to follow)'), selected='Combine'),  #selectInput
                     radioButtons(NS(id, "histShow"), 'Histogram Type:', choices=c('Proportions','Counts'), selected='Proportions'),
                     numericInput(NS(id, "histBins"), 'Histogram Bins:', min=5, max=50, value=15)
        ),
        mainPanel(width=10,
                  actionButton(NS(id, "load"), "Load Data", style="color: #fff; background-color: orange; border-color: #2e6da4"),
                  # <font color='DarkCyan' size=2>Beta release. </font>
                  HTML(base::paste0("<font color='navy' size=2>", notes, "</font>")),
                  div(DT::dataTableOutput(NS(id, "selected")), style = "font-size:95%"),
                  plotlyOutput(NS(id, "histPlot"), height=500, width=900)
        )
    )
)
}

#-------------------------------------------------------------------------------

#br 2023-04 - the server piece of the Histogram module
# dataFile = name of data file
# readFun = the name of the function used to read the data. Could be read.pipe, read_csv or load
# selections = an existing reactive object (currently list) with an element named "sel" which contains a data frame with
#    the selections from the Report tab. This is used to subset the account-level data so as to keep only the slices of interest
# caption = a description of the
# formats = a named vector listing the formats to apply to each field (dollar, percent, comma)
# suffix = a named vector listing the suffix to add to the field names. For example "(THOUSAND $)"
Shiny.histogramServer <- function(id, dataFile, readFun, selections, caption, formats, suffix=NULL)
{
moduleServer(id, 
             function(input, output, session) {
                 #I finally managed to load an Rdata file
                 data <- shiny::reactiveVal(value = NULL)
                 shiny::observeEvent(input$load, {
                     n <- new.env()
                     print("loading Rdata after click")
                     env <- load(dataFile, envir = n)
                     data(n[[names(n)]])
                 })
                 
                 #restrict to the categories selected
                 output$selected <- DT::renderDataTable(server=FALSE, {
                     if (is.null(selections()$slct)) return()
                     Shiny.buildDT(selections()$slct, type='print', caption='These are the categories that you selected on the Report tab (used to subset the data):')
                 })
                 
                 #retrieve the format to apply
                 myfmt <- shiny::reactive({
                     out <- formats[input$var]
                     out
                 })
                 
                 #This is used to clarify the units. For example, balances could be in Thousand $ or Million $
                 var.suffix <- shiny::reactive({
                     out <- if (!is.null(suffix)) suffix[input$var] else ''
                     if (is.na(out)) out <- ''
                     out
                 })
                 
                 output$histPlot <- renderPlotly({
                     if (is.null(selections()$slct)) {
                         plt <- textSlide('First select some deposit bucket(s) on the Report tab.', boxcol='white', wrap=75, txtcol='blue')
                     } else if (is.null(data())) {
                         plt <- textSlide('Click the Load Data button above once and wait. \n\n\nFor efficiency, the file needed here (over 1M records) is not loaded by default: to
                       avoid slowing down the first tabs of this app for everyone.', boxcol='white', wrap=75, txtcol='brown')
                     } else {
                         histData <- base::merge(data(), selections()$slct)
                         #this can happen if Other GL is selected
                         if (nrow(histData) == 0) {
                             plt <- textSlide('No data found.\n\n It sounds like you selected Other GL or CDARS, which are not hosted in DNA. Try other selections.', 
                                              boxcol='white', wrap=75, txtcol='red')
                         } else {
                             avgValue <- mean(histData[, input$var], na.rm=TRUE)
                             medianValue <- median(histData[, input$var], na.rm=TRUE)
                             labavg <- data.frame(x=avgValue, y=0, lab=paste('Avg\n', round(avgValue, 3)))
                             labmed <- data.frame(x=medianValue, y=0, lab=paste('Med\n', round(medianValue, 3)))
                             #Warning. If the LOG scale transformation is requested, then convert all 0 values to 0.0001 - otherwise they will go missing
                             histData[, 'tmp.toPlot'] <- if (input$histScale == 'Log') pmax(0.0001, histData[, input$var]) else histData[, input$var]
                             labttl <- paste0(base::prettyNum(nrow(histData), big.mark=",", scientific=FALSE), ' records (', 
                                              base::prettyNum(length(which(is.na(histData[, input$var]))), big.mark=",", scientific=FALSE), ' missing values)')
                             if (input$plot == 'Histogram') {
                                 plt <- ggplot(histData, aes_string(x='tmp.toPlot')) +
                                     labs(title=paste0(input$var, ' distribution: ', labttl), 
                                          y=input$histShow, x=paste(input$var, var.suffix())) +
                                     geom_vline(xintercept = avgValue, color='blue', size=0.8, linetype='dashed') +
                                     geom_vline(xintercept = medianValue, color='brown', size=0.8, linetype='dashed') +
                                     geom_text(data=labavg, aes(x, y, label=lab), size=3.5, hjust=0, vjust=5, color='blue') +
                                     geom_text(data=labmed, aes(x, y, label=lab), size=3.5, hjust=0, vjust=2, color='brown')
                                 if (input$histShow == 'Counts') {
                                     plt <- plt + geom_histogram(color='gray30', fill="#69b3a2", bins = input$histBins)
                                     yaxfmt <- 'comma'
                                 }
                                 if (input$histShow == 'Proportions') {
                                     plt <- plt + geom_histogram(color='gray30', fill="#69b3a2", bins = input$histBins, aes(y = (..count..)/sum(..count..)))
                                     yaxfmt <- 'percent'
                                 }
                                 plt <- if (input$histScale == 'Log') Shiny.formatPlot(plt, xfmt=myfmt(), transx='log2', yfmt=yaxfmt) else Shiny.formatPlot(plt, xfmt=myfmt(), yfmt=yaxfmt)
                             } else if (input$plot == 'Percentile') {
                                 # plt <- if (input$histScale == 'Log') qpl(pmax(0.0001, histData[, input$var]), log=TRUE) else qpl(histData[, input$var])
                                 plt <- if (input$histScale == 'Log') Shiny.formatPlot(qpl(histData$tmp.toPlot), yfmt=myfmt(), transy='log2')
                                 else Shiny.formatPlot(qpl(histData$tmp.toPlot), yfmt=myfmt())
                                 plt <- plt + labs(title=paste0(input$var, ' distribution: ', labttl), y=paste(input$var, var.suffix()))
                             }
                         }
                     }
                     ggplotly(plt)  #tooltip=c("FYI")
                 })
             }
)
}

#-------------------------------------------------------------------------------

#br 2023-09
#Given a data frame, convert all character vectors to factors: needed for Shiny

df2factor <- function(df)
{
for (v in colnames(df)) if (is.character(df[, v])) df[, v] <- factor(df[, v])
df
}

#-------------------------------------------------------------------------------

#br 2023-09
#Put this snippet of code in its own function, for ease of reference. Thus far I've encountered
#    several examples of Shiny apps that ran just fine on the desktop, while failing completely
#    on the server. Debugging the first case in April 2023 was a complete nightmare - things looked
#    desperate back then, due to persistent and unexplained crashes. It turned out that the problem was
#    the encoding - unexpected characters were fine on Windows, but not on Red Hat. As a solution,
#    convert all character fields to UTF8.

df2UTF <- function(df)
{
# if (!exists('mutate_if')) require(dplyr)
# if (!exists('utf8_encode')) require(utf8)
dplyr::mutate_if(df, is.character, utf8::utf8_encode)
}

#-------------------------------------------------------------------------------

#br 2023-10
#Moved to a separate function some code written back in 2023-03. Given a data frame, a date, date + balance fields,
#    and a vector with fields to aggregate by, calculate the latest Daily and MTD changes for those levels, as of the
#    given date. The assumption is that the data is daily. 
#2023-11: It turns out that an expanded version is needed to also capture the changes since 3M ago and YTD. I was first
#    going to write a new function, but it's better to expand what is already in place.

DMTD.changes <- function(DF, aggrBY, date, dateField, balField, countField, intField, extra=FALSE)
{
#the current summary: currently customized for the GDS data. WARNING: some INT_RATEs may well be NULL
thedata <- dplyr::filter(DF, !!sym(dateField) == date)
#in case there is no data
if (nrow(thedata) == 0) {
   out <- data.frame()
   return(list(data=out, dates=data.frame(Current=date), message=data.frame(Message=paste('No data found for', date))))
}
currDF <- group_by_at(thedata, c(dateField, aggrBY)) %>% 
   dplyr::summarize(Acct=sum(!!sym(countField)), Bal=sum(!!sym(balField)), 
             WAC=round(sum(!!sym(balField) * !!sym(intField), na.rm=TRUE) / max(0.000001, sum(!!sym(balField))), 6), .groups='drop_last') %>% dplyr::ungroup()
#define the dates
alld <- unique(DF[, dateField])  #all dates
#BUT: if an earlier date is selected through the calendar input, then switch to that
alld <- alld[alld <= date]
allm <- unique(format(alld, '%Y%m'))  #all months
prior.day <- max(alld[alld < date])
prior.mth <- max(allm[allm < format(date, '%Y%m')])
prior.ME <- max(alld[format(alld, '%Y%m') == prior.mth])
datedef <- data.frame(Current=date, Prior_Day=prior.day, Prior_MthEnd=prior.ME)  #date definitions

#filter and aggregate the data
prevDF <- dplyr::filter(DF, !!sym(dateField) == prior.day)
prevME.DF <- dplyr::filter(DF, !!sym(dateField) == prior.ME)
#the prior date and prior ME balances
pdDF <- group_by_at(prevDF, c(aggrBY)) %>% dplyr::summarize(PrevDlyBal=sum(!!sym(balField)), .groups='drop_last') %>% dplyr::ungroup()
pmeDF <- group_by_at(prevME.DF, c(aggrBY)) %>% dplyr::summarize(PrevMEBal=sum(!!sym(balField)), .groups='drop_last') %>% dplyr::ungroup()
#Use left joins: so that any new values in the data are kept instead of being discarded
#2023-11: WRONG. Full joins must be used here. As observed with the Branch-level GDS, longer term changes
#    are impacted by things such as branches being closed and thus disappearing from the latest data. As such,
#    the sum of branch-level changes is no longer equal to the sum of Market changes, which is not acceptable.
both <- base::merge(base::merge(currDF, pdDF, all=TRUE), pmeDF, all=TRUE)
#This addresses the scenario of a brand new (or unexpected) category in the data - or those that disappeared
for (v in c('Bal','PrevDlyBal','PrevMEBal')) both[, v] <- NA2zero(both[, v])
#In addition... impute the dates that may be missing because of categories that disappeared from the latest data
both[, dateField][is.na(both[, dateField])] <- date
#With the NAs removed, the changes can be safely calculated
both$Dly_Change <- both$Bal - both$PrevDlyBal
both$MTD_Change <- both$Bal - both$PrevMEBal
both$PrevDlyBal <- both$PrevMEBal <- NULL

#Do not calculate figures as of 3 month-ends ago and year-end unless requested
if (extra) {
   #3 month-ends ago
   prior.mth3 <- format(as.Date(paste0(max(allm), '15'), '%Y%m%d') - 90, '%Y%m')
   prior.ME3 <- max(alld[format(alld, '%Y%m') == prior.mth3])
   #year-end
   ally <- unique(format(alld, '%Y'))
   prior.yr <- max(ally[ally < format(date, '%Y')])
   prior.YE <- max(alld[format(alld, '%Y') == prior.yr])
   #define and then return a data frame with the dates: so that every one is very clear
   datedef <- data.frame(Current=date, Prior_Day=prior.day, Prior_MthEnd=prior.ME, Prior_3MthEnd=prior.ME3, Prior_YearEnd=prior.YE)
   #filter and aggregate the data
   prevME3.DF <- dplyr::filter(DF, !!sym(dateField) == prior.ME3)
   prevYE.DF <- dplyr::filter(DF, !!sym(dateField) == prior.YE)
   pme3DF <- group_by_at(prevME3.DF, c(aggrBY)) %>% dplyr::summarize(PrevME3Bal=sum(!!sym(balField)), .groups='drop_last') %>% dplyr::ungroup()
   pyeDF <- group_by_at(prevYE.DF, c(aggrBY)) %>% dplyr::summarize(PrevYEBal=sum(!!sym(balField)), .groups='drop_last') %>% dplyr::ungroup()
   #join and calculate the changes. MUST use full joins
   both <- base::merge(base::merge(base::merge(base::merge(currDF, pdDF, all=TRUE), pmeDF, all=TRUE), pme3DF, all=TRUE), pyeDF, all=TRUE)
   #This addresses the scenario of a brand new (or unexpected) category in the data - or those that disappeared
   for (v in c('Bal','PrevDlyBal','PrevMEBal','PrevME3Bal','PrevYEBal')) both[, v] <- NA2zero(both[, v])
   #In addition... impute the dates that may be missing because of categories that disappeared from the latest data
   both[, dateField][is.na(both[, dateField])] <- date
   both$Dly_Change <- both$Bal - both$PrevDlyBal  #these 2 are repeated because of the full joins
   both$MTD_Change <- both$Bal - both$PrevMEBal
   both$ME3_Chg <- both$Bal - both$PrevME3Bal
   both$YTD_Chg <- both$Bal - both$PrevYEBal
   both$PrevDlyBal <- both$PrevMEBal <- both$PrevME3Bal <- both$PrevYEBal <- NULL
   both <- rename(both, Dly_Chg=Dly_Change, MTD_Chg=MTD_Change)
}

#return not only the data, but also the date definitions
list(data=both, dates=datedef, message=data.frame())
}

#-------------------------------------------------------------------------------

