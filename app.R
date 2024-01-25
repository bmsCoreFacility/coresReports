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
######################################################################
######################################################################



######################################################################
##set up the design elements
cards = list(
  card(
    card_header("Notes for selected date",
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
  )
)

##core selector values
coreNames = data.frame('CORE' = c('bms',
                                  'brain tissue bank',
                                  'cmdi',
                                  'electron microscopy',
                                  'flow',
                                  'genomics',
                                  'histology',
                                  'zebrafish',
                                  'combined'))

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
    row_heights = c(0.75,0.75,0.75),
    col_widths = c(6,6,6,6,12),
    cards[[2]], cards[[3]], cards[[4]], cards[[5]], cards[[1]]
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
      dplyr::mutate(conDate = as.numeric(paste(year,month,sep=''))) %>%
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
    ggplot(incomeRangeData(), aes(conDate, value, group = itemNote, fill = itemNote)) +
      geom_col(linewidth = 1, color = brewer.pal(4,'Greys')[4], width = 0.5, position = 'dodge') +
      labs(x = "Date", y = "Value ($CAD)") +
      scale_fill_manual(values = c(brewer.pal(12,'Paired'))) +
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
    ggplot(expenseRangeData(), aes(conDate, value, group = itemNote, fill = itemNote)) +
      geom_col(linewidth = 1, color = brewer.pal(4,'Greys')[4], width = 0.5, position = 'dodge') +
      labs(x = "Date", y = "Value ($CAD)") +
      scale_fill_manual(values = c(brewer.pal(12,'Paired'))) +
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
  output$itemizedExpenseDateRange = renderPlot(expenseDateRangeItemizedPlot())
  
  
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

}
######################################################################
######################################################################



######################################################################
##run the application
shinyApp(ui, server)


######################################################################
######################################################################