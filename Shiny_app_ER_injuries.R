#bogdan romocea 2024-06 - ER Injuries
# This is an R Shiny app based on data that I downloaded from NEISS (National Electronic Injury Surveillance System). 
# Several kinds of features were implemented programmatically. Unlike other dashboarding tools, Shiny apps can be
# largely automated, are high performance, and are fully customizable according to user needs. Note how an app like
# this allows non-technical (and technical!) users to explore the data and determine some of the major features present
# there (counts, trending, seasonality, patterns by age & gender, etc.) - quickly and easily, by clicking several times
# in the browser.
# To see the app you can go to  https://br44114.shinyapps.io/ER_Injuries/ 

 
require(ggplot2) ; require(stringr) ; require(dplyr) ; require(shiny) ; require(gridExtra) ; require(scales)
require(plotly) ; require(DT)
options(scipen=5)
source('sh_func.r')
 
ui <- fluidPage(
  # theme = shinytheme('journal'),
  title='ER Injuries',
  #Note: the total width of the sidebar + main panel = 12 (relative units)
  navbarPage(
    title = span('ER Injuries', style = 'color: red'),
    id = 'main_navbar', 
    introPanel(),   #end of tabPanel
 
    tabPanel(
      title = span('Raw Data', style = 'color:brown'),
      value = 'Raw Data',  
        mainPanel(width=12,
          HTML('<font color=navy size=2>This tab shows a random sample of the raw data. Any change in the sample size will cause the data to be resampled.</font>'),
          numericInput('ssize', label='Sample Size', value=500, step=10, width=275),            
          div(DT::dataTableOutput('rawdata'), style = 'font-size:95%')
        ) 
    ),   #end of tabPanel
 
    tabPanel(
      title = span('Reports', style = 'color:blue'),
      value = 'Reports',  
      sidebarLayout(
        sidebarPanel(width=2, 
          checkboxGroupInput(
            inputId='t1cols',
            label='Report Columns:',
            choices=c('Product_1','Gender','Race','Body_Part','Diagnosis','Disposition'),
            selected=c('Body_Part','Product_1')), 
          selectInput(
            inputId='t1period',
            label='Period:',
            choices=c('Latest','30 days','90 days','YTD','12M','ALL','Date Range'),
            selected='90 days'), 
          conditionalPanel(condition = "input.t1period == 'Date Range'", 
            dateRangeInput(
              inputId='t1dateRange' ,
              label='Date Range:' ,
              start='2024-01-01' ,
              end='2024-07-19' )), 
          selectInput(
            inputId='t1freq',
            label='Frequency:',
            choices=c('Daily','Weekly','Monthly'),
            selected='Daily') 
        ), 
        mainPanel(width=10,
          HTML('<font color=navy size=2>Use the Checkbox Input, the Filters and the Search box to assemble the report that you need.</font>'),
          div(DT::dataTableOutput('tbl'), style = 'font-size:95%'),
          fluidRow(column(DT::dataTableOutput('tbltot'), width = 6, style = 'font-size:95%')), br(),
          div(DT::dataTableOutput('slct'), style = 'font-size:95%')
        ) 
      )  #end of sidebar Layout 
    ),   #end of tabPanel
 
    tabPanel(
      title = span('Trending', style = 'color:magenta'),
      value = 'Trending',  
      sidebarLayout(
        sidebarPanel(width=2, 
          radioButtons(
            inputId='t2plot',
            label='Plot:',
            choices='Cases',
            selected='Cases'), 
          selectInput(
            inputId='t2chart',
            label='Chart:',
            choices=c('Line','Bar'),
            selected='Line'), 
          selectInput(
            inputId='t2series',
            label='Series:',
            choices=c('Actual','Cumulative','Changes','Cumul Chg'),
            selected='Actual'), 
          radioButtons(
            inputId='t2scale',
            label='Scales:',
            choices=c('fixed','free'),
            selected='fixed') 
        ), 
        mainPanel(width=10,
          HTML('<font color=navy size=2>These are the trending charts for the selections that you made on the Report tab. Scroll down for the data table. The size of the first chart is fixed.
The width of the second chart changes if you resize your browser window.</font>'),
          plotlyOutput('plot1a', height=350, width=700),
          plotlyOutput('plot1b', height=600),
          div(DT::dataTableOutput('slctrend'), style = 'font-size:95%')
        ) 
      )  #end of sidebar Layout 
    ),   #end of tabPanel
 
    tabPanel(
      title = span('Narratives', style = 'color:darkgreen'),
      value = 'Narratives',  
        mainPanel(width=12,
          HTML('<font color=navy size=2>This tab allows you to browse the Narratives of the selections that you made on the Reports tab.
For efficiency, the output is limited to 5,000 records.</font>'),
          div(DT::dataTableOutput('prodnarr'), style = 'font-size:95%')
        ) 
    ),   #end of tabPanel
 
    tabPanel(
      title = span('Age & Gender', style = 'color:darkorange'),
      value = 'Age & Gender',  
        mainPanel(width=12,
          HTML('<font color=navy size=3>This tab presents the patterns by Age and Gender for the selections that you made on the Reports tab.</font>'),
          plotlyOutput('plotag', height=600),
          HTML('<font color=navy size=3>Note how on this chart, the lines stop at age 84. That is simply because I could not find US population counts
beyond age 84.</font>'),
          plotlyOutput('plotagrate', height=600),
          fluidRow(column(DT::dataTableOutput('agegender'), width = 6, style = 'font-size:95%')),
        ) 
    ),   #end of tabPanel
 
    tabPanel(
      title = span('Dictionary', style = 'color:purple'),
      value = 'Dictionary',  
        mainPanel(width=12,
          HTML('<font color=navy size=2>This tab presents Dictionaries (most to least frequent individual words) built dynamically from the Narratives of the
   selections that you made on the Reports tab.</font>'),
          div(DT::dataTableOutput('dict'), style = 'font-size:95%')
        ) 
    ),
    
    Shiny.aboutPanel() 
  )   #end of navbarPage
)   #end of UI
 
server <- function(input, output, session) {
  DF <- reactiveFileReader(intervalMillis = 60000, session, filePath = 'data/neiss', readFunc = LoadToEnvironment)
 
  myDF <- shiny::reactive({
    sDF <- DF()[['DF']]
    rawDF <- data.frame() 
    dFld <- 'Date'
    maxd <- max(sDF[['Date']])
    since <- Shiny.getSinceDate(maxd, input$t1period)
    if (input$t1freq == 'Weekly') {
      sDF[['WeekStarting']] <- WeekStarting(sDF[['Date']])
      dFld <- 'WeekStarting'
      maxd <- max(sDF[[dFld]])
    } else if (input$t1freq == 'Monthly') {
      sDF[['MonthStarting']] <- MonthStarting(sDF[['Date']])
      dFld <- 'MonthStarting'
      maxd <- max(sDF[[dFld]])
    } 
    if (input$t1period == 'Date Range') {
      fltDF <- dplyr::filter(sDF, !!sym(dFld) >= input$t1dateRange[1] & !!sym(dFld) <= input$t1dateRange[2])
    } else if (input$t1period == 'Latest') {
      fltDF <- dplyr::filter(sDF, !!sym(dFld) == maxd)
    } else {
      fltDF <- dplyr::filter(sDF, !!sym(dFld) >= since & !!sym(dFld) <= maxd)
    } 
    if (nrow(fltDF) == 0) return(list(summ=data.frame(), trend=data.frame(), raw=rawDF)) 
    summ <- dplyr::group_by_at(fltDF, input$t1cols) %>%
      dplyr::summarize(Records=n(), Cases=sum(Weight), .groups = 'drop_last') %>% dplyr::ungroup() %>%
      dplyr::mutate(Period=paste(range(fltDF[[dFld]]), collapse=' to '), .before=input$columns[1]) %>%
      dplyr::arrange(desc(Cases)) 
    trend <- dplyr::group_by_at(fltDF, c(dFld, input$t1cols)) %>%
      dplyr::summarize(Records=n(), Cases=sum(Weight), .groups = 'drop_last') %>% dplyr::ungroup() %>%
      dplyr::group_by_at(c(input$t1cols)) %>% dplyr::arrange_at(dFld) %>%
      dplyr::mutate(Cases_Cumul = cumsum(Cases), Cases_Chg = Cases - lag(Cases),
        Cases_CChg = cumsum(coalesce(Cases_Chg, 0))) %>% ungroup() 
    rawDF <- fltDF[, c(dFld, input$t1cols, 'Narrative', 'Weight', 'Age', 'Gender')]
    return(list(summ=summ, trend=trend, dateField=dFld, raw=rawDF))
  })
 
  trendSel <- shiny::reactive({
    if (nrow(myDF()[['summ']]) == 0) return(list(trend=data.frame(), totrend=data.frame())) 
    read_data <- input$tbl_rows_selected
    if (is.null(read_data)) return(list(trend=data.frame(), totrend=data.frame()))
    myTB_tmp <- myDF()[['summ']][read_data, input$t1cols, drop=FALSE] %>% merge(myDF()[['trend']], by=input$t1cols) %>%
      dplyr::arrange_at(c(myDF()[['dateField']], input$t1cols))
    myTB_tmptot <- myTB_tmp %>% dplyr::group_by_at(myDF()[['dateField']]) %>% summarize(Cases = sum(Cases) ) %>%
      dplyr::ungroup() %>% dplyr::mutate(Cases_Cumul = cumsum(Cases), Cases_Chg = Cases - lag(Cases),
        Cases_CChg = cumsum(coalesce(Cases_Chg, 0)))
    #Add the Subset and Bucket fields. These are meant to provide a way to fully (if not most appropriately) label all possible charts
    myTB_tmp$Subset <- if (length(input$t1cols) == 1) '' else apply(myTB_tmp[, input$t1cols[-length(input$t1cols)] , drop=FALSE], 1, function(x) paste(x, collapse='_'))
    myTB_tmp$Bucket <- myTB_tmp[, input$t1cols[length(input$t1cols)], drop=TRUE]

    return(list(trend=myTB_tmp, totrend=myTB_tmptot))
  })
 
  sampDF <- shiny::reactive({
    samp <- DF()[['DF']] %>% dplyr::slice_sample(n=input$ssize) 
    return(list(samp=samp, records=input$ssize, total=nrow(DF()[['DF']])))
  })
 
  #update the label on the numeric input to show the total # of records
  observe({
		recs <- sampDF()$total
		# print(recs)
		updateNumericInput(session, 'ssize', label=paste0('Sample Size (out of ', prettyNum(recs, big.mark=','), ' records)'))  #value=500, step=10, width=100
	}) 

  prodNarr <- shiny::reactive({
    if (nrow(myDF()[['summ']]) == 0) return(list(sel=data.frame())) 
    read_data <- input$tbl_rows_selected
    if (is.null(read_data)) return(list(sel=data.frame()))
    COLS <- setdiff(input$t1cols, 'Gender')  #Ha! must exclude Gender, because it's already in the raw data file
    myTB_tmp <- myDF()[['summ']][read_data, COLS, drop=FALSE] %>% merge(myDF()[['raw']], by=COLS) %>%
      dplyr::arrange_at(c(myDF()[['dateField']])) %>%
      dplyr::mutate(across(where(is.factor), forcats::fct_drop)) 
    return(list(sel=head(myTB_tmp, 5000)))
  })
 
  narrDict <- shiny::reactive({
    if (nrow(myDF()[['summ']]) == 0) return(list(sel=data.frame())) 
    read_data <- input$tbl_rows_selected
    if (is.null(read_data)) return(list(sel=data.frame()))
    COLS <- setdiff(input$t1cols, 'Gender')  #Ha! must exclude Gender, because it's already in the raw data file
    myTB_tmp <- myDF()[['summ']][read_data, COLS, drop=FALSE] %>% merge(myDF()[['raw']], by=COLS) %>%
      dplyr::arrange_at(c(myDF()[['dateField']])) %>%
      dplyr::mutate(across(where(is.factor), forcats::fct_drop)) 
    myTB_tmp <- getDict(myTB_tmp[['Narrative']], myTB_tmp[['Weight']], top=1000) %>% dplyr::select(-c('Strings'))
    return(list(sel=myTB_tmp))
  })
 
  ageGender <- shiny::reactive({
    if (nrow(myDF()[['summ']]) == 0) return(list(sel=data.frame())) 
    read_data <- input$tbl_rows_selected
    if (is.null(read_data)) return(list(sel=data.frame()))
    COLS <- setdiff(input$t1cols, 'Gender')  #Warning: Gender must be excluded, in case it was included on the Report
    myTB_tmp <- myDF()[['summ']][read_data, COLS, drop=FALSE] %>% merge(myDF()[['raw']], by=COLS) %>%
      dplyr::arrange_at(c(myDF()[['dateField']])) %>%
      dplyr::mutate(across(where(is.factor), forcats::fct_drop)) 
    myTB_tmp <- myTB_tmp %>% dplyr::group_by(Age, Gender) %>% dplyr::summarize(Cases=round(sum(Weight)), .groups = 'drop_last') %>%
                           ungroup() %>% dplyr::filter(Gender %in% c('MALE','FEMALE')) %>% left_join(DF()[['pop']], by = c('Age', 'Gender')) %>%
                           mutate(CasesPer10k = round((Cases / population) * 1e4, 2))
    return(list(sel=myTB_tmp))
  })
 
  output$tbl <- DT::renderDataTable(server=FALSE, {
    if(nrow(myDF()$summ) == 0) {
      tmp.message <- data.frame(Message = 'No data found. You may need to adjust the filters.')
      return(DT::datatable(tmp.message, rownames=FALSE, options = list(dom = 't')))
    } 
    localTB_tmp <- myDF()$summ 
    excelButtons <- list( list(extend = 'excel', text = 'Excel', title = 'My Report', messageTop = NULL,
      filename = paste0('MyReport', '_', Sys.Date() ))) 
    myTB <- DT::datatable(localTB_tmp,
      class = 'display nowrap compact', filter = 'top', rownames=FALSE, extensions = 'Buttons',  
      options = list(orderClasses=TRUE, pageLength=10, dom = 'Blfrtip', buttons = excelButtons ) ,
      caption = 'A. The report. Click on the rows to Select/Deselect the categories of interest for Trending. If the Frequency is Weekly / Monthly,
then the subsetting is done based on the WeekStarting / MonthStarting dates. The number of Records shows how many rows there are in the input file. The number of Cases
is an estimate for the total number of injuries across the US, extrapolated from the representative sample of hospitals to the entire country based on the weights provided
(last field on the Raw Data tab).') 
    myTB <- myTB %>% DT::formatRound(c('Cases', 'Records'), digits=0) 
      if (!all.NA(localTB_tmp[, 'Cases'])) myTB <- myTB %>% DT::formatStyle('Cases', background = styleColorBar(range(localTB_tmp[, 'Cases'], na.rm=TRUE), 'skyblue'),
        backgroundSize = '100% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') 
    return(myTB)
  })
 
  output$tbltot <- DT::renderDataTable(server=FALSE, {
    if(nrow(myDF()$summ) == 0) {
      return()
    } 
    localTB_tmp <- myDF()$summ 
    read_data <- input$tbl_rows_all
    if (is.null(read_data)) return() 
    myTB_tmp <- localTB_tmp[read_data, , drop=FALSE] %>% group_by(Period) %>% summarize(Records=sum(Records), Cases=sum(Cases)) 
    myTB <- DT::datatable(myTB_tmp,
      rownames=FALSE, options = list(dom = 't') ,
      caption = 'B. Report Total. This aggregates the records from the table above, after any Filters and/or Search:') 
    myTB <- myTB %>% DT::formatRound(c('Cases','Records'), digits=0) 
    return(myTB)
  })
 
  output$slct <- DT::renderDataTable(server=FALSE, {
    if(nrow(myDF()$summ) == 0) {
      return()
    } 
    localTB_tmp <- myDF()$summ 
    read_data <- input$tbl_rows_selected
    if (is.null(read_data)) return() 
    myTB_tmp <- localTB_tmp[read_data, , drop=FALSE] 
    myTB_tmp <- myTB_tmp %>% dplyr::select(-c('Period','Cases','Records'))  
    myTB <- DT::datatable(myTB_tmp,
      rownames=FALSE, options = list(dom = 't') ,
      caption = 'C. These are the categories that you selected above, used to subset the data for the Trending, Narratives, Age & Gender and Dictionary tabs:') 
    return(myTB)
  })
 
  output$slctrend <- DT::renderDataTable(server=FALSE, {
    if(nrow(trendSel()$trend) == 0) {
      return()
    } 
    localTB_tmp <- trendSel()$trend 
    localTB_tmp <- localTB_tmp %>% dplyr::select(- c(Bucket, Subset, ends_with('_Cumul'), ends_with('_Chg'), ends_with('_CChg')))  
    excelButtons <- list( list(extend = 'excel', text = 'Excel', title = 'My Trending Report', messageTop = NULL,
      filename = paste0('MyTrendingReport', '_', Sys.Date() ))) 
    myTB <- DT::datatable(localTB_tmp,
      class = 'display nowrap compact', filter = 'top', rownames=FALSE, extensions = 'Buttons',  
      options = list(orderClasses=TRUE, pageLength=10, dom = 'Blfrtip', buttons = excelButtons ) ,
      caption = 'This is the trending data plotted above. You can export to Excel, filter, sort, etc.') 
    myTB <- myTB %>% DT::formatRound('Cases', digits=0) 
      if (!all.NA(localTB_tmp[, 'Cases'])) myTB <- myTB %>% DT::formatStyle('Cases', background = styleColorBar(range(localTB_tmp[, 'Cases'], na.rm=TRUE), 'skyblue'),
        backgroundSize = '100% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') 
    return(myTB)
  })
 
  output$prodnarr <- DT::renderDataTable(server=FALSE, {
    if(nrow(prodNarr()$sel) == 0) {
      tmp.message <- data.frame(Message = 'First make some selections on the Report tab: click on the row(s) of interest.')
      return(DT::datatable(tmp.message, rownames=FALSE, options = list(dom = 't')))
    } 
    localTB_tmp <- prodNarr()$sel 
    localTB_tmp <- localTB_tmp %>% dplyr::select(-c('Weight','Age','Gender'))  
    excelButtons <- list( list(extend = 'excel', text = 'Excel', title = 'Selected Data', messageTop = NULL,
      filename = paste0('SelectedData', '_', Sys.Date() ))) 
    myTB <- DT::datatable(localTB_tmp,
      class = 'display compact', filter = 'top', rownames=FALSE, extensions = 'Buttons',  
      options = list(orderClasses=TRUE, pageLength=20, dom = 'Blfrtip', buttons = excelButtons ) ) 
    return(myTB)
  })
 
  output$dict <- DT::renderDataTable(server=FALSE, {
    if(nrow(narrDict()$sel) == 0) {
      tmp.message <- data.frame(Message = 'First make some selections on the Report tab: click on the row(s) of interest.')
      return(DT::datatable(tmp.message, rownames=FALSE, options = list(dom = 't')))
    } 
    localTB_tmp <- narrDict()$sel 
    excelButtons <- list( list(extend = 'excel', text = 'Excel', title = 'Selected Data', messageTop = NULL,
      filename = paste0('SelectedData', '_', Sys.Date() ))) 
    myTB <- DT::datatable(localTB_tmp,
      class = 'display nowrap compact', filter = 'top', rownames=FALSE, extensions = 'Buttons',  
      options = list(orderClasses=TRUE, pageLength=20, dom = 'Blfrtip', buttons = excelButtons ) ) 
    myTB <- myTB %>% DT::formatRound('Count', digits=0) 
      if (!all.NA(localTB_tmp[, 'Count'])) myTB <- myTB %>% DT::formatStyle('Count', background = styleColorBar(range(localTB_tmp[, 'Count'], na.rm=TRUE), 'palegreen'),
        backgroundSize = '100% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') 
    return(myTB)
  })
 
  output$rawdata <- DT::renderDataTable(server=FALSE, {
    if(nrow(sampDF()$samp) == 0) {
      return()
    } 
    localTB_tmp <- sampDF()$samp 
    excelButtons <- list( list(extend = 'excel', text = 'Excel', title = 'Sample Raw Data', messageTop = NULL,
      filename = paste0('SampleRawData', '_', Sys.Date() ))) 
    myTB <- DT::datatable(localTB_tmp,
      class = 'display nowrap compact', filter = 'top', rownames=FALSE, extensions = 'Buttons',  
      options = list(orderClasses=TRUE, pageLength=20, dom = 'Blfrtip', buttons = excelButtons ) ) 
    return(myTB)
  })
 
  output$agegender <- DT::renderDataTable(server=FALSE, {
    if(nrow(ageGender()$sel) == 0) {
      tmp.message <- data.frame(Message = 'First make some selections on the Report tab: click on the row(s) of interest.')
      return(DT::datatable(tmp.message, rownames=FALSE, options = list(dom = 't')))
    } 
    localTB_tmp <- ageGender()$sel 
    excelButtons <- list( list(extend = 'excel', text = 'Excel', title = 'Selected Data', messageTop = NULL,
      filename = paste0('SelectedData', '_', Sys.Date() ))) 
    myTB <- DT::datatable(localTB_tmp,
      class = 'display nowrap compact', filter = 'top', rownames=FALSE, extensions = 'Buttons',  
      options = list(orderClasses=TRUE, pageLength=10, dom = 'Blfrtip', buttons = excelButtons ) ,
      caption = 'This is the data plotted above.') 
    myTB <- myTB %>% DT::formatRound('Cases', digits=0) 
      if (!all.NA(localTB_tmp[, 'Cases'])) myTB <- myTB %>% DT::formatStyle('Cases', background = styleColorBar(range(localTB_tmp[, 'Cases'], na.rm=TRUE), 'skyblue'),
        backgroundSize = '100% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') 
    return(myTB)
  })
 
output$plot1a <- renderPlotly({
    if (nrow(trendSel()$totrend) == 0) { 
		plt <- textSlide('First make some selections on the Report tab: click on the row(s) of interest.', boxcol='white', wrap=75) 
   } else {
       laby <- if (input$t2plot == 'Bal') 'Million $' else input$t2plot
       if (input$t2series == 'Actual') {
           suff <- '' ; ttlsuff <- '' ; label <- 'FYI'
       } else if (input$t2series == 'Cumulative') {
           suff <- '_Cumul' ; ttlsuff <- ', Cumulative' ; label <- 'CChanges'
       } else if (input$t2series == 'Changes') {
           suff <- '_Chg' ; ttlsuff <- ', Changes' ; label <- 'Changes'
       } else if (input$t2series == 'Cumul Chg') {
           suff <- '_CChg' ; ttlsuff <- ', Cumulative Changes' ; label <- 'CumulChg'
       }
       plt <- tsline(as.data.frame(trendSel()$totrend), myDF()$dateField, paste0(input$t2plot, suff), myDF()$dateField, 
           ttl=paste0('Total Selections: ', input$t2plot, ttlsuff), N=1, laby=laby) + geom_line(color='steelblue') + geom_point(color='hotpink2')  #label=label
       if (input$t2series %in% c('Changes','Cumul Chg')) plt <- plt + geom_hline(yintercept=0, lty=2, color='gray70')
       pfmt <- if (input$t2plot == 'Bal') 'dollar' else if (input$t2plot == 'WAC') 'percent' else if (input$t2plot %in% c('Acct','Cases')) 'comma'
       plt <- Shiny.formatPlot(plt, yfmt=pfmt)
   }
   ttip <- if (input$t2series == 'Actual') 'FYI' else if (input$t2series == 'Changes') 'Changes' else if (input$t2series == 'Cumul Chg') 'CumulChg'
   return(ggplotly(plt))  #tooltip=ttip
})

 
output$plot1b <- renderPlotly({
    if (nrow(trendSel()$trend) == 0) { 
return() 
   } else {
       laby <- if (input$t2plot == 'Bal') 'Million $' else input$t2plot
       if (input$t2series == 'Actual') {
           suff <- '' ; ttlsuff <- '' ; label <- 'FYI'
       } else if (input$t2series == 'Cumulative') {
           suff <- '_Cumul' ; ttlsuff <- ', Cumulative' ; label <- 'CChanges'
       } else if (input$t2series == 'Changes') {
           suff <- '_Chg' ; ttlsuff <- ', Changes' ; label <- 'Changes'
       } else if (input$t2series == 'Cumul Chg') {
           suff <- '_CChg' ; ttlsuff <- ', Cumulative Changes' ; label <- 'CumulChg'
       }
       plt <- tsline(as.data.frame(trendSel()$trend), myDF()[['dateField']], paste0(input$t2plot, suff), myDF()[['dateField']], facet='Subset', clr='Bucket', fs=input$t2scale,
           ttl=paste0('Trending by Bucket: ', input$t2plot, ttlsuff), N=1, laby=laby)   #label=label
       if (input$t2series %in% c('Changes','Cumul Chg')) plt <- plt + geom_hline(yintercept=0, lty=2, color='gray70')
       pfmt <- if (input$t2plot == 'Bal') 'dollar' else if (input$t2plot == 'WAC') 'percent' else if (input$t2plot %in% c('Acct','Cases')) 'comma'
       plt <- Shiny.formatPlot(plt, yfmt=pfmt)
   }
   ttip <- if (input$t2series == 'Actual') 'FYI' else if (input$t2series == 'Changes') 'Changes' else if (input$t2series == 'Cumul Chg') 'CumulChg'
   return(ggplotly(plt))  #tooltip=ttip
})

 
output$plotag <- renderPlotly({
    if (nrow(ageGender()$sel) == 0) { 
		plt <- textSlide('First make some selections on the Report tab: click on the row(s) of interest.', boxcol='white', wrap=75) 
   } else {
       plt <- ageGender()$sel %>% ggplot(aes(Age, Cases, colour = Gender)) + geom_line() +
                   labs(title='Total estimated number of injuries', subtitle='by Age & Gender, within the categories + period selected',
                   y='Total Estimated Injuries')
       plt <- Shiny.formatPlot(plt, yfmt='comma')
   }
   return(ggplotly(plt))  #tooltip=ttip
})

 
output$plotagrate <- renderPlotly({
    if (nrow(ageGender()$sel) == 0) { 
		plt <- textSlide('First make some selections on the Report tab: click on the row(s) of interest.', boxcol='white', wrap=75) 
   } else {
       plt <- ageGender()$sel %>% ggplot(aes(Age, CasesPer10k, colour = Gender)) + geom_line() +
                   labs(title='Estimated number of injuries per 10K people',
                   y='Estimated injuries per 10,000 people')
       plt <- Shiny.formatPlot(plt, yfmt='comma')
   }
   return(ggplotly(plt))  #tooltip=ttip
})

 
}

shinyApp(ui=ui, server=server)  #options=list(port=7990) 
