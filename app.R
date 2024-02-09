######################################################################
##This is a Shiny web application for generating monthly 
##reports for the CORE facilities at Dalhousie University.
##Created by: Christopher Hughes
######################################################################
######################################################################



######################################################################
##libraries
library('shiny')
library('tidyverse')
library('ggplot2')
library('RColorBrewer')
library('bslib')
library('bsicons')
library('shinyWidgets')
library('DT')
######################################################################
######################################################################



######################################################################
##set up the design elements
cards = list(
  card(
    card_header("Notes for the selected date",
                tooltip(
                  bs_icon("info-circle"),
                  "Notes for the selected date."
                )),
    tableOutput("noteData")
  ),
  navset_card_underline(
    title = "Selected date income",
    nav_panel("Total", plotOutput("incomeDate")),
    nav_panel("Itemized", plotOutput("itemizedIncomeDate"))
  ),
  navset_card_underline(
    title = "Selected date expenses",
    nav_panel("Total", plotOutput("expenseDate")),
    nav_panel("Itemized", plotOutput("itemizedExpenseDate"))
  ),
  navset_card_underline(
    title = "Ranged date income",
    nav_panel("Total", plotOutput("incomeDateRange")),
    nav_panel("Itemized", plotOutput("itemizedIncomeDateRange"))
  ),
  navset_card_underline(
    title = "Ranged date expenses",
    nav_panel("Total", plotOutput("expenseDateRange")),
    nav_panel("Itemized", plotOutput("itemizedExpenseDateRange"))
  ),
  card(
    card_header("Information for selected date plots",
                tooltip(
                  bs_icon("info-circle"),
                  "Details on the selected date plots."
                )),
    textOutput("selectedDateInfo")
  ),
  card(
    card_header("Information for ranged date plots",
                tooltip(
                  bs_icon("info-circle"),
                  "Details on the ranged date plots."
                )),
    textOutput("rangedDateInfo")
  ),
  card(
    card_header("Income table data for ranged date plots",
                tooltip(
                  bs_icon("info-circle"),
                  "Select items in the table to update the itemized plots."
                )),
    DT::dataTableOutput("rangedDateIncomeTable")
  ),
  card(
    card_header("Expense table data for ranged date plots",
                tooltip(
                  bs_icon("info-circle"),
                  "Select items in the table to update the itemized plots."
                )),
    DT::dataTableOutput("rangedDateExpenseTable")
  )
)

##core selector values
coreNames = data.frame('CORE' = c('bms',
                                  'brainTissueBank',
                                  'cmdi',
                                  'electronMicroscopy',
                                  'flow',
                                  'genomics',
                                  'histology',
                                  'zebrafish'))

######################################################################
######################################################################



######################################################################
##build the UI
ui = page_sidebar(
  title = "Cores Monthly Reports",
  fillable = FALSE,
  
  ##sidebar
  sidebar = sidebar(
    width = 275,
    selectInput("coreSelector",
                label = "CORE facility:", 
                choices = coreNames,
                selected = 'bms'),
    #
    dateInput("currentDate",
              label = "Date:",
              value = Sys.Date()),
    #
    dateRangeInput("dateRange",
                   label = "Date range:",
                   start = Sys.Date() - 62, end = Sys.Date())
  ),
  ##page
  layout_columns(
    row_heights = c(0.3,1,0.4,1,1,0.75),
    col_widths = c(12,6,6,12,6,6,6,6,12),
    cards[[6]],cards[[2]], cards[[3]], cards[[7]], cards[[4]], cards[[5]], cards[[8]], cards[[9]], cards[[1]]
  )
)

######################################################################
######################################################################



######################################################################
##define the server logic
server = function(input, output) {
  
  ##read the core facility value
  coreValue = reactive({as.character(input$coreSelector)})
  
  ##baseline input data processing
  inputData = reactive({
    read_tsv('coresMonthlyReportsData.tsv', show_col_types = FALSE,
             col_types = cols(year = col_character(),
                              month = col_character())) %>%
      dplyr::mutate(conDate = as.numeric(paste(year,sub('m(.*)','\\1',month),sep=''))) %>%
      dplyr::filter(core == coreValue())
  })
  
  ######################
  ##below is for the set date analysis
  ##parse the date value
  currentDateValue = reactive({as.character(input$currentDate)})
  currentYear = reactive({sub('(.*)\\-[0-9]+\\-[0-9]+','\\1', currentDateValue())})
  currentMonth = reactive({sub('[0-9]+\\-(.*)\\-[0-9]+','\\1', currentDateValue())})
  currentDate = reactive({as.numeric(paste(currentYear(),currentMonth(),sep=''))})
  
  ##generate the income data
  incomeData = reactive({
    dplyr::filter(inputData(), 
                  (itemCategory == 'income' & conDate == currentDate())
    )
  })
  #
  incomeDatePlot = reactive({
    ggplot(incomeData(), aes(itemCategory, value, fill = itemCategory)) +
      geom_col(linewidth = 1, color = brewer.pal(4,'Greys')[4], width = 0.5) +
      scale_fill_manual(values = c(brewer.pal(4,'BuGn')[3])) +
      labs(x = 'Type', y = 'Value ($CAD)') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            legend.position = "none")
  })
  #
  output$incomeDate = renderPlot(incomeDatePlot())
  #
  incomeItemizedDatePlot = reactive({
    ggplot(incomeData(), aes(itemNote, value, fill = itemNote)) +
      geom_col(linewidth = 1, color = brewer.pal(4,'Greys')[4], width = 0.5) +
      scale_fill_manual(values = c(brewer.pal(12,'Paired'))) +
      labs(x = 'Type', y = 'Value ($CAD)') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            legend.position = "none")
  })
  #
  output$itemizedIncomeDate = renderPlot(incomeItemizedDatePlot())
  
  ##generate the expense data
  expenseData = reactive({
    dplyr::filter(inputData(), 
                  (itemCategory == 'expense' & conDate == currentDate())
    )
  })
  #
  expenseDatePlot = reactive({
    ggplot(expenseData(), aes(itemCategory, value, fill = itemCategory)) +
      geom_col(linewidth = 1, color = brewer.pal(4,'Greys')[4], width = 0.5) +
      scale_fill_manual(values = c(brewer.pal(4,'OrRd')[3])) +
      labs(x = 'Type', y = 'Value ($CAD)') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            legend.position = "none")
  })
  #
  output$expenseDate = renderPlot(expenseDatePlot())
  #
  expenseItemizedDatePlot = reactive({
    ggplot(expenseData(), aes(itemNote, value, fill = itemNote)) +
      geom_col(linewidth = 1, color = brewer.pal(4,'Greys')[4], width = 0.5) +
      scale_fill_manual(values = c(brewer.pal(12,'Paired'))) +
      labs(x = 'Type', y = 'Value ($CAD)') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            legend.position = "none")
  })
  #
  output$itemizedExpenseDate = renderPlot(expenseItemizedDatePlot())
  
  
  
  
  ######################
  ##below is for the ranged date analysis
  ##parse the date range value
  rangeDateStartValue = reactive({as.character(input$dateRange[1])})
  rangeDateStartYear = reactive({sub('(.*)\\-[0-9]+\\-[0-9]+','\\1', rangeDateStartValue())})
  rangeDateStartMonth = reactive({sub('[0-9]+\\-(.*)\\-[0-9]+','\\1', rangeDateStartValue())})
  rangeStart = reactive({as.numeric(paste(rangeDateStartYear(),rangeDateStartMonth(),sep=''))})
  rangeDateEndValue = reactive({as.character(input$dateRange[2])})
  rangeDateEndYear = reactive({sub('(.*)\\-[0-9]+\\-[0-9]+','\\1', rangeDateEndValue())})
  rangeDateEndMonth = reactive({sub('[0-9]+\\-(.*)\\-[0-9]+','\\1', rangeDateEndValue())})
  rangeEnd = reactive({as.numeric(paste(rangeDateEndYear(),rangeDateEndMonth(),sep=''))})
  
  ##generate income data
  incomeRangeData = reactive({
    dplyr::filter(inputData(),
                  (itemCategory == 'income' & conDate >= rangeStart() & conDate <= rangeEnd())
    ) %>%
      dplyr::mutate(conDate = factor(conDate))
  })
  #
  incomeDateRangePlot = reactive({
    ggplot(incomeRangeData(), aes(conDate, value)) +
      geom_col(linewidth = 1, color = brewer.pal(4,'Greys')[4], fill = brewer.pal(4,'BuGn')[3], width = 0.5) +
      labs(x = "Date", y = "Value ($CAD)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.margin=margin(c(1,1,1,1)))
  })
  #
  output$incomeDateRange = renderPlot(incomeDateRangePlot())
  #
  incomeDateRangeItemizedPlot = reactive({
    incomeItemIndex = input$rangedDateIncomeTable_rows_selected
    if(length(incomeItemIndex)){
      incomeItemName = incomeRangeData()[incomeItemIndex,"itemNote"]
      itemFilteredIncomeRangeData = dplyr::filter(incomeRangeData(), grepl(incomeItemName, itemNote))
      ggplot(itemFilteredIncomeRangeData, aes(conDate, value, group = itemNote)) +
        geom_line(color = brewer.pal(4,'BuGn')[3], linewidth = 1) +
        #scale_y_continuous(limits = c(0,max(itemFilteredIncomeRangeData$value)+250), breaks = seq(0,max(itemFilteredIncomeRangeData$value),max(itemFilteredIncomeRangeData$value)/10)) +
        scale_y_continuous(limits = c(0,max(itemFilteredIncomeRangeData$value)+250)) +
        labs(x = paste(incomeItemName), y = "Value ($CAD)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              #axis.title.x = element_blank(),
              axis.title.y = element_text(size = 14),
              legend.position = "none")
    } else {
      ggplot(incomeRangeData(), aes(conDate, value, group = itemNote)) +
        geom_line(color = brewer.pal(4,'BuGn')[3], linewidth = 1) +
        labs(x = "Date", y = "Value ($CAD)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 14),
              legend.position = "none")
    }
  })
  #
  output$itemizedIncomeDateRange = renderPlot(incomeDateRangeItemizedPlot())
  
  ##generate expense data
  expenseRangeData = reactive({
    dplyr::filter(inputData(),
                  (itemCategory == 'expense' & conDate >= rangeStart() & conDate <= rangeEnd())
    ) %>%
      dplyr::mutate(conDate = factor(conDate))
  })
  #
  expenseDateRangePlot = reactive({
    ggplot(expenseRangeData(), aes(conDate, value)) +
      geom_col(linewidth = 1, color = brewer.pal(4,'Greys')[4], fill = brewer.pal(4,'OrRd')[3], width = 0.5) +
      labs(x = "Date", y = "Value ($CAD)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.margin=margin(c(1,1,1,1)))
  })
  #
  output$expenseDateRange = renderPlot(expenseDateRangePlot())
  #
  expenseDateRangeItemizedPlot = reactive({
    expenseItemIndex = input$rangedDateExpenseTable_rows_selected
    if(length(expenseItemIndex)){
      expenseItemName = expenseRangeData()[expenseItemIndex,"itemNote"]
      itemFilteredExpenseRangeData = dplyr::filter(expenseRangeData(), grepl(expenseItemName, itemNote))
      ggplot(itemFilteredExpenseRangeData, aes(conDate, value, group = itemNote)) +
        geom_line(color = brewer.pal(4,'OrRd')[3], linewidth = 1) +
        scale_y_continuous(limits = c(0,max(itemFilteredExpenseRangeData$value)+250)) +
        labs(x = paste(expenseItemName), y = "Value ($CAD)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              #axis.title.x = element_blank(),
              axis.title.y = element_text(size = 14),
              legend.position = "none")
    } else {
      ggplot(expenseRangeData(), aes(conDate, value, group = itemNote)) +
        geom_line(color = brewer.pal(4,'OrRd')[3], linewidth = 1) +
        labs(x = "Date", y = "Value ($CAD)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              #axis.title.x = element_blank(),
              axis.title.y = element_text(size = 14),
              legend.position = "none")
    }
  })
  #
  output$itemizedExpenseDateRange = renderPlot(expenseDateRangeItemizedPlot())
  
  
  
  ######################
  ##below is the table data
  incomeRangeTableData = reactive({
    dplyr::filter(inputData(),
                  (itemCategory == 'income' & conDate >= rangeStart() & conDate <= rangeEnd())
    ) %>%
      dplyr::mutate(conDate = factor(conDate)) %>%
      dplyr::select(-conDate, -month, -year, -core, -value) %>%
      unique()
  })
  #
  output$rangedDateIncomeTable = DT::renderDataTable({datatable(incomeRangeTableData(), selection = 'single', rownames = FALSE)})
  
  ##
  expenseRangeTableData = reactive({
    dplyr::filter(inputData(),
                  (itemCategory == 'expense' & conDate >= rangeStart() & conDate <= rangeEnd())
    ) %>%
      dplyr::mutate(conDate = factor(conDate)) %>%
      dplyr::select(-conDate, -month, -year, -core, -value) %>%
      unique()
  })
  #
  output$rangedDateExpenseTable = DT::renderDataTable({datatable(expenseRangeTableData(), selection = 'single', rownames = FALSE)})
  
  
  
  ######################
  ##below is the notes data
  ##generate notes data
  notesData = reactive({
    dplyr::filter(inputData(), 
                  (itemCategory == 'note' & conDate == currentDate())) %>%
      dplyr::select(itemNote)
    
  })
  #
  output$noteData = renderTable(notesData(), colnames = FALSE)
  
  
  ######################
  ##below is the information for the dates
  selectedDateInfoText = "The selected date plots below are based on the Date dropdown box in the sidebar menu. It will only use information about the month and year, so you can select any date in that month you wish. Use the Total and Itemized tabs to switch between plot types."
  output$selectedDateInfo = renderText(selectedDateInfoText)
  #
  rangedDateInfoText = "The ranged date plots below are based on the Date range dropdown box in the sidebar menu. It will only use information about the month and year, so you can select any date in that month you wish. Use the Total and Itemized tabs to switch between plot types. By default, in the itemized plots it will show all items, which can be a bit messy. If you would like to display specific items, click on the item you are interested in in the table below to update the plot."
  output$rangedDateInfo = renderText(rangedDateInfoText)

}
######################################################################
######################################################################



######################################################################
##run the application
shinyApp(ui, server)


######################################################################
######################################################################