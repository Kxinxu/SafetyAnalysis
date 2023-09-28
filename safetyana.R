library(shiny)
library(shinydashboard)
library(haven)
library(dplyr)
library(tidyr)
library(tools)
library(purrr)
library(stats)
library(formattable)
library(DT)
library(openxlsx)

# Define UI ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Safety Analysis",
                  tags$li(
                    class = "dropdown",
                    tags$style(".skin-blue .main-header .navbar { backgroud-color: #2c3e40; }")
                  )),
  dashboardSidebar(id="",
                   collapsed = T,
                   sidebarMenu(
                     menuItem("Adverse Event Summary", tabName = "AES")
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "AES",
              fluidRow(
                tabBox(width = 5,
                       tabPanel("Dataset",
                                fluidRow(
                                  column(12,
                                         fileInput("file1", label = "Upload the Adverse Event Analysis Dataset"),
                                         uiOutput("Sflag"),
                                         uiOutput("Tflag"),
                                         uiOutput("sbjid")
                                  ),
                                  column(12,
                                         radioButtons('pop', label = "Acquire the population of subgroups",
                                                      choices = list("Typing in" = 1, 
                                                                     "Upload the Subject-Level Analysis Dataset" = 2)),
                                         uiOutput("num"),
                                         uiOutput("var"),
                                         uiOutput("val"),
                                         uiOutput("valnum"),
                                         uiOutput("slvar"),
                                         uiOutput("slval"),
                                         uiOutput("slvaln"),
                                         uiOutput("add"),
                                         uiOutput("demo"),
                                         uiOutput("demoval"),
                                         uiOutput("demovaln"),
                                         uiOutput("checkfl"),
                                         uiOutput("fvar"),
                                         uiOutput("fval"),
                                         uiOutput("checkfl1"),
                                         uiOutput("fvar1"),
                                         uiOutput("fval1")
                                         
                                         
                                  ),
                                )
                       ),
                       tabPanel("SOC&PT",
                                fluidRow(
                                  column(6,
                                         checkboxInput("bysoc","Analyze SOC",value = T)),
                                  
                                  column(6,
                                         checkboxInput("bypt","Analyze PT",value = T)),
                                  column(6,
                                         selectInput("soc","Please select the term of SOC", 
                                                     choices = "")
                                  ),
                                  column(6,
                                         selectInput("pt","Please select the term of PT", 
                                                     choices = "")
                                  )
                                )),
                       tabPanel("Criteria",
                                fluidRow(
                                  column(6,
                                         p(h4("Phase"),
                                           checkboxInput("checkp", label = "Check for phase analysis", value = FALSE),
                                           uiOutput("pvar"),
                                           uiOutput("pval")
                                         )
                                  ),
                                  column(6,
                                         p(h4("Relationship"),
                                           checkboxInput("checkr", label = "Check for adverse events related to regimen", value = FALSE),
                                           uiOutput("relt"),
                                           uiOutput("relg")
                                         )
                                  ),
                                  column(6,
                                         p(h4("Serious Event"),
                                           checkboxInput("checksr", label = "Check for serious adverse events", value = FALSE),
                                           uiOutput("set")
                                         )
                                  ),
                                  column(6,
                                         p(h4("Immune Related Event"),
                                           checkboxInput("checkirae", label = "Check for investigator determined immune related adverse events", value = FALSE),
                                           uiOutput("irAE")
                                         )
                                  ),
                                  column(6,
                                         p(h4("Infusion Related Event"),
                                           checkboxInput("checkinfu", label = "Check for information about infusion", value = FALSE),
                                           uiOutput("infusion")
                                         )
                                  ),
                                  column(6,
                                         p(h4("Severity"),
                                           checkboxInput("checks", label = "Check for specific grades of adverse events", value = FALSE),
                                           uiOutput("sele"),
                                           uiOutput("selg"),
                                           uiOutput("allg"),
                                           uiOutput("gt3"),
                                           uiOutput("lw3")
                                         )
                                  ),
                                  column(6,
                                         p(h4("Other criteria"),
                                           checkboxInput("checko", label = "Check for other criteria", value = FALSE),
                                           uiOutput("ovar"),
                                           uiOutput("oval")
                                         )
                                  ),
                                  
                                )
                       )
                ),
                tabBox(width = 7,
                       tabPanel(title = "AE Dataset Preview",
                                fluidRow(
                                  column(12,
                                         DTOutput("dataprez"))
                                )),
                       tabPanel(title = "Subject-level Dataset Preview",
                                fluidRow(
                                  column(12,
                                         DTOutput("datapresl"))
                                )),
                       tabPanel(title = "Output",
                                fluidRow(
                                  #  column(6,
                                  #         p(HTML("<b>Select the information you would like to see in the table<b>"),
                                  #           checkboxInput("checksoc", label = "SOC", value = FALSE),
                                  #           checkboxInput("checkpt", label = "PT", value = FALSE),
                                  #         )
                                  #  ),
                                  column(6,
                                         p(h4("Generate the result table"),
                                           actionButton("action",label = "Get the result"),
                                           downloadButton("rt", "Download the result")
                                         ),
                                         checkboxInput("colo", label = tags$b("Rearrange the column order"), value = F),
                                         uiOutput("level"),
                                         checkboxInput("checkpct", label = tags$b("Specific percentage required"), value = FALSE),
                                         uiOutput("pct")
                                  ),
                                  column(6,
                                         selectInput("col","Sort by",choices = NULL),
                                         checkboxInput("checkasc", label = "Ascending", value = F),
                                         checkboxInput("trans",tags$b("Need translation for SOC and PT")),
                                         uiOutput("transvar")                  
                                  ),
                                  column(12,
                                         tableOutput("pop"),
                                         DT::dataTableOutput("test"))
                                )
                                
                       )
                )
              )
              
              
              
      )
      
      
    )
  )
)








# Define server logic ----
server <- function(input, output) {
  
  #modify the limitation of the size of file uploaded
  options(shiny.maxRequestSize = 30*1024^2)
  
  fmt_num <- function(x, digits, width = digits + 4) {
    formatC(x,
            digits = digits,
            format = "f",
            width = width
    )
  }
  
  count_by <- function(data, # Input data set
                       grp, # Group variable
                       var, # Analysis variable
                       var_label = var, # Analysis variable label
                       id = "USUBJID") { # Subject ID variable
    data <- data %>% rename(grp = !!grp, var = !!var, id = !!id)
    
    left_join(
      count(data, grp, var),
      count(data, grp, name = "tot"),
      by = "grp",
    ) %>%
      mutate(
        pct = fmt_num(100 * n / tot, digits = 1),
        n = fmt_num(n, digits = 0),
        npct = paste0(n, " (", pct, ")")
      ) %>%
      pivot_wider(
        id_cols = var,
        names_from = grp,
        values_from = c(n, pct, npct),
        values_fill = list(n = "0", pct = fmt_num(0, digits = 0))
      ) %>%
      mutate(var_label = var_label)
  }
  
  
  label <- reactive({
    data1 <- input$file1
    adae <- read_sas(data1$datapath)
    label = adae %>% map_chr(attr_getter("label"))
    label
  })
  
  output$transvar <- renderUI({
    if(input$trans){
      varlist <- list(
        selectInput("ver","Select the version of MedDra Dictionary",choices = c("v25.1" = "v25.1",
                                                                                "v25.0" = "v25.0",
                                                                                "v24.1" = "v24.1",
                                                                                "v23.1" = "v23.1",
                                                                                "v23.0" = "v23.0")),
        checkboxInput("ETC","English to Chinese"),
        checkboxInput("CTE","Chinese to English")
        
      )
      do.call(tagList,varlist)
      
    }
    else{NULL}
  })
  
  observeEvent(input$file1,{
    data1 <- input$file1
    adae <- read_sas(data1$datapath)
    idx1 = grep("Organ Class|器官",label())
    soccol = colnames(adae)[idx1]
    soclab = label()[idx1]
    
    idx2 = grep("Dictionary-Derived Term|首选术语",label())
    ptcol = colnames(adae)[idx2]
    ptlab = label()[idx2]
    if(length(idx1)>0){
      updateSelectInput(session  = getDefaultReactiveDomain(),"soc",choices = setNames(soccol,soclab))
    }else{
      updateSelectInput(session  = getDefaultReactiveDomain(),"soc",choices = setNames(names(adae),label()))
    }
    if(length(idx2)>0){
      updateSelectInput(session  = getDefaultReactiveDomain(),"pt",choices = setNames(ptcol,ptlab))
    }else{
      updateSelectInput(session  = getDefaultReactiveDomain(),"pt",choices = setNames(names(adae),label()))
    }
  })
  
  #Generate input file
  adae<-reactive({
    data1 <- input$file1
    adae <- read_sas(data1$datapath)
    # idx1 = grep("Organ Class|器官",label())
    # soccol = colnames(adae)[idx1]
    # soclab = label()[idx1]
    # 
    # idx2 = grep("Dictionary-Derived Term|首选术语",label())
    # ptcol = colnames(adae)[idx2]
    # ptlab = label()[idx2]
    if("USUBJID" %in% colnames(adae)){
      adae
    }
    else{
      adae$USUBJID = adae[[input$sjid]]
    }
    
    #filter valid information
    if("SAFFL" %in% colnames(adae)){
      if(length(input$saffl) != 0 && input$saffl == T){
        adae = subset(adae,SAFFL=="Y")
      }
      else{
        adae
      }
    }
    if("TRTEMFL" %in% colnames(adae)){
      if(length(input$trtemfl) != 0 &&input$trtemfl == T){
        adae = subset(adae,TRTEMFL=="Y")
      }
      else{
        adae
      }
    }
    
    adae["grade"] = "All Grade"
    if(input$soc != "" && input$pt != ""){
      
      #  adae$SOC=adae[[input$soc]]
      #   adae[is.na(adae$SOC),]$SOC <- "Not Coded"
      #  adae$PT =adae[[input$pt]]
      # adae[is.na(adae$SOC),]$PT <- adae[is.na(adae$SOC),]$AETERM
      
      adae$SOC <- ifelse(adae[[input$soc]] == "","Not Coded",adae[[input$soc]])
      adae$PT <- ifelse(adae[[input$pt]] == "",paste(adae$AETERM," (Report Term)"),adae[[input$pt]])
    }
    
    
    
    soc_ref <- read.csv("SOC_All.csv")
    pt_ref <- read.csv("PT_All.csv")
    
    
    if(is.null(input$CTE) == F && input$CTE == T){
      adae <- merge(adae,soc_ref[soc_ref$VER == input$ver,],by.x = "SOC",by.y = "SOC_CH",all.x = T)
      adae <- merge(adae,pt_ref[pt_ref$VER == input$ver,],by.x = "PT",by.y = "PT_CH",all.x = T)
      adae["AESOC"] = adae["SOC_EN"]
      adae["AEDECOD"] = adae["PT_EN"]
    }
    else if(is.null(input$ETC) == F && input$ETC == T){
      adae <- merge(adae,soc_ref[soc_ref$VER == input$ver,],by.x ="SOC",by.y = "SOC_EN",all.x = T)
      adae <- merge(adae,pt_ref[pt_ref$VER == input$ver,],by.x = "PT",by.y = "PT_EN",all.x = T)
      adae["AESOC"] = adae["SOC_CH"]
      adae["AEDECOD"] = adae["PT_CH"]
    }
    else{
      adae["AESOC"] = adae$SOC
      adae["AEDECOD"] = adae$PT
    }
    adae
    
  })
  
  miss_v <- reactive({
    data <- adae()
    if(input$CTE == T){
      miss_obs <- data[c("USUBJID","SOC","PT")][is.na(data$PT_EN)==T,]
    }
    else if(input$ETC == T){
      miss_obs <- data[c("USUBJID","SOC","PT")][is.na(data$PT_CH)==T,]
    }
    miss_obs
  })
  
  output$miss <- renderText({
    if(is.null(miss_v())){
      return(NULL)
    }
    
    paste0("There are ", nrow(miss_v()), " obeservations do not have matching terms")
  })
  
  numTR <- reactive({
    data = adae()
    TR = unique(data[input$var])
    TR = unlist(TR)
    #TR = pull(TR,TRTA)
  })  
  
  ##-------SAFFL AND TRTEMFL----------------
  
  output$Sflag <- renderUI({
    req(input$file1)
    data1 <- input$file1
    adae <- read_sas(data1$datapath)
    if("SAFFL" %in% colnames(adae)){
      checkboxInput("saffl",tags$b("Check for Safety Population Flag"),value = T)
    }
    else
      return(NULL)
  })
  
  output$Tflag <- renderUI({
    req(input$file1)
    data1 <- input$file1
    adae <- read_sas(data1$datapath)
    if("TRTEMFL" %in% colnames(adae)){
      checkboxInput("trtemfl",tags$b("Check for Treatment Emergent Analysis Flag"),value = T)
    }
    else
      return(NULL)
  })
  ##-------USUBJID----------------------------------- 
  
  output$sbjid <- renderUI({
    req(input$file1)
    data1 <- input$file1
    adae <- read_sas(data1$datapath)
    if("USUBJID" %in% colnames(adae)){
      return(NULL)    }
    else{
      selectInput("sjid","Select the variable of unique subject id",choices = setNames(names(adae),label()))
    }
  })
  
  ##-------population count via type in----------------------  
  
  output$var <- renderUI({
    if(is.null(input$file1) || input$pop !=1)     
      return(NULL)
    data1 <- input$file1
    adae <- read_sas(data1$datapath)
    selectInput("var", label = "Select key group for analysis and filtration",setNames(colnames(adae),label()))
  })
  
  output$val <- renderUI({
    if(is.null(input$file1) || input$pop !=1) {
      return(NULL)
    }    
    else{
      checkboxGroupInput("val",label = NULL, choices = setNames(numTR(),numTR()))
    }
    
  })
  
  output$valnum <- renderUI({
    if(is.null(input$file1) || input$pop !=1)     
      return(NULL)
    val = as.vector(input$val)
    map(val,~numericInput(.x,label = paste0("Please type in the population of ",.x),NULL))
  })
  
  
  ##-------population count via adsl----------------------    
  output$num <- renderUI({
    if(input$pop == 2){
      
      #map(numTR(),~numericInput(.x,label = paste0("Please type in the population of ",.x),NULL))
      inputList <- list(
        fileInput("file0", label = h5("Please upload the Subject-Level Analysis Dataset"))
        
      )
      do.call(tagList,inputList)
    }
    
  })
  
  adsl<-reactive({
    if(is.null(input$file0))
      return(NULL)
    data0 <- input$file0
    adsl <- read_sas(data0$datapath)
    
    adsl
  })  
  
  labelsl <- reactive({
    data1 <- input$file0
    adsl <- read_sas(data1$datapath)
    labelsl = adsl %>% map_chr(attr_getter("label"))
    labelsl
  })
  
  output$slvar <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(input$pop != 2){
      return(NULL)
    }
    data1 <- input$file0
    adsl <- read_sas(data1$datapath)
    selectInput("slvar", label = "Select grouping variable 1",setNames(colnames(adsl),labelsl()))
  })
  
  valsl <- reactive({
    data = adsl()
    val = unique(data[input$slvar])
    val = val[rowSums(val == "" | is.na(val)) == 0,]
    val = unlist(val)
    val
  })
  
  output$slval <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(input$pop != 2)
      return(NULL)
    checkboxGroupInput("slval",label = NULL, choices = setNames(valsl(),valsl()))
    
  })
  
  output$slvaln <- renderUI({
    if(is.null(input$file0) || input$pop !=2)     
      return(NULL)
    val = as.vector(input$slval)
    map(val,~textInput(paste("n_",.),label = paste0("Label of ",.),.))
  })
  
  ##-----------adding another subgroup feature-----------------  
  output$add <- renderUI({
    if(is.null(input$file0) || input$pop == 1)     
      return(NULL)
    checkboxInput('add',label = tags$b("Add a subgroup criteria"),value = F)
    
  })
  
  output$demo <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(length(input$pop) != 0 && length(input$add) != 0){
      if(input$add == T){
        data1 <- input$file0
        adsl <- read_sas(data1$datapath)
        selectInput("demo", NULL,setNames(colnames(adsl),labelsl()))
      }
      else
        return(NULL)
    }
    else{
      return(NULL)
    }
    
  })
  
  valdemo <- reactive({
    data = adsl()
    val = unique(data[input$demo])
    val = unlist(val)
  })
  
  output$demoval <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(length(input$pop) != 0 && length(input$add) != 0){
      if(input$add == T){
        checkboxGroupInput("demoval",label = NULL, choices = setNames(valdemo(),valdemo()))
      }
      else
        return(NULL)
    }
    else{
      return(NULL)
    }
  })
  
  output$demovaln <- renderUI({
    if(is.null(input$file0) || input$pop !=2)     
      return(NULL)
    val = as.vector(input$demoval)
    map(val,~textInput(paste("n1_",.),label = paste0("Label of ",.),.))
  })
  ##---------------first filtering variable----------
  
  output$checkfl <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(input$pop != 2)
      return(NULL)
    checkboxInput("tgp",tags$b("Check to filter population subset"),value = F)   
  })
  
  output$fvar <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(length(input$pop) != 0 && length(input$tgp) != 0){
      if(input$tgp == T){
        adsl <- adsl()
        selectInput("tgpvar",NULL,choices = setNames(colnames(adsl),adsl %>% map_chr(attr_getter("label"))))
      }
      else
        return(NULL)
    }
    else{
      return(NULL)
    }
    
  })
  
  output$fval <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(length(input$pop) != 0 && length(input$tgp) != 0){
      if(input$tgp == T){
        data = adsl()
        val = unique(data[[input$tgpvar]])
        checkboxGroupInput("fval",label = NULL, choices = setNames(val,val))
      }
      else
        return(NULL)
    }
    else{
      return(NULL)
    }
  })
  ##---------------second filtering variable----------
  
  output$checkfl1 <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(input$pop != 2)
      return(NULL)
    checkboxInput("tgp1",tags$b("Add a filtering variable"),value = F)   
  })
  
  output$fvar1 <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(length(input$pop) != 0 && length(input$tgp1) != 0){
      if(input$tgp1 == T){
        adsl <- adsl()
        selectInput("tgpvar1",NULL,choices = setNames(colnames(adsl),adsl %>% map_chr(attr_getter("label"))))
      }
      else
        return(NULL)
    }
    else{
      return(NULL)
    }
    
  })
  
  output$fval1 <- renderUI({
    if(is.null(input$file0))     
      return(NULL)
    if(length(input$pop) != 0 && length(input$tgp1) != 0){
      if(input$tgp1 == T){
        data = adsl()
        val = unique(data[[input$tgpvar1]])
        checkboxGroupInput("fval1",label = NULL, choices = setNames(val,val))
      }
      else
        return(NULL)
    }
    else{
      return(NULL)
    }
  })
  ##-----------calculate the population for each group-----
  
  pop <- reactive({
    
    if(input$pop == 1){
      tot = map_int(as.vector(input$val),~input[[.x]] %||% 0)
      pop<-data.frame(TRTA = as.vector(input$val), tot)
    }
    if(input$pop == 2){
      if(is.null(adsl()))
        return(NULL)
      adsl <- adsl() 
      adsl["TRTA"] = adsl[input$slvar]
      slval = as.vector(input$slval)
      adsl <- adsl %>%
        filter(SAFFL %in% c("Y","yes","Yes","是")) %>%
        filter(TRTA %in% slval)
      for (i in slval) {
        adsl[adsl$TRTA == i,]$TRTA = input[[paste("n_",i)]]
      }
      if(input$add == T){
        adsl["demo"] = adsl[[input$demo]]
        adsl = subset(adsl,demo %in% as.vector(input$demoval))
        for (i in as.vector(input$demoval)) {
          adsl[adsl$demo == i,]$demo = input[[paste("n1_",i)]]
        }
        adsl$TRTA = paste(adsl$TRTA,adsl$demo,sep = "+")
      }
      if(input$tgp == T){
        adsl["ScreenedG"] = adsl[input$tgpvar]
        adsl <- subset(adsl,ScreenedG %in%  as.vector(input$fval))
      }
      if(input$tgp1 == T){
        adsl["ScreenedG1"] = adsl[input$tgpvar1]
        adsl <- subset(adsl,ScreenedG1 %in%  as.vector(input$fval1))
      }
      pop <- adsl %>%
        count(TRTA, name = "tot")
    }
    pop
  })
  
  output$pop <- renderTable(pop())
  ##--------------relationship----------------------
  
  output$relt <- renderUI({
    if(input$checkr == FALSE || is.null(input$file1))
      return(NULL)
    else{
      data = adae()
      label = data %>% map(attr_getter("label"))
      idx1 = grep("REL",colnames(data))
      if(length(idx1) != 0){
        col1 = colnames(data)[idx1]
        lab1 = label[idx1]
      }else{
        col1 = colnames(data)
        lab1 = label
      }
      
      selectInput("rel", h5("Please select the relationship you would like to analyze"),
                  choices = setNames(col1,lab1),selected = "AEREL")
      
    }
    
  })
  
  output$relg <- renderUI({
    if(input$checkr == FALSE || is.null(input$rel))
      return(NULL)
    else{
      data = adae()
      rel = unique(data[input$rel])
      rel = rename(rel,"AEREL" = input$rel)
      rel = filter(rel, AEREL != "")
      rel = pull(rel,AEREL)
      checkboxGroupInput("relg", label = NULL, choices = setNames(rel,rel))  
    }
  })
  
  
  ##---------severity---------------------------------------  
  
  output$sele <- renderUI({
    if(input$checks == FALSE || is.null(input$file1))
      return(NULL)
    else{
      data1 <- input$file1
      adae <- read_sas(data1$datapath)
      if(length(grep("severity|grade|Toxicity|Grade|toxicity|Severity",label())) != 0 ){
        idx = grep("severity|grade|Toxicity|Grade|toxicity|Severity",label())
        secol = colnames(adae)[idx]
        selab = label()[idx]
        selectInput("sele", h5("Please the type and levels of severity you would like to analyze"),
                    choices = setNames(secol,selab),selected = NULL)
      }
      else{
        selectInput("sele", h5("Please the type and levels of severity you would like to analyze"),
                    choices = setNames(colnames(adae),label()),selected = NULL)
      }
      
    }
    
  })
  
  output$selg <- renderUI({
    if(input$checks == FALSE)
      return(NULL)
    else{
      data = adae()
      rel = unique(data[input$sele])
      rel = rename(rel, "level" = input$sele)
      rel = filter(rel,level != "")
      rel = sort(pull(rel,level))
      checkboxGroupInput("selg", label = NULL, choices = setNames(rel,rel))
    }
  })
  
  output$gt3 <- renderUI({
    if(input$checks == F)
      return(NULL)
    else{
      checkboxInput("checkgt3", label = "Grade > = 3")
    }
  })
  
  output$lw3 <- renderUI({
    if(input$checks == F)
      return(NULL)
    else{
      checkboxInput("checklw3", label = "Grade < 3")
    }
  })
  
  output$allg <- renderUI({
    if(input$checks == F)
      return(NULL)
    else{
      checkboxInput("checkallg", label = "All Grade",value = T)
    }
  })
  
  ##------------------serious event----------------
  
  output$set <- renderUI({
    if(input$checksr == FALSE || is.null(input$file1))
      return(NULL)
    else{
      data = adae()
      label = data %>% map(attr_getter("label"))
      idx1 = grep(pattern = "Serious|serious|SERIOUS|SAE",label)
      
      if(length(idx1) != 0){
        col1 = colnames(data)[idx1]
        lab1 = label[idx1]
      }else{
        col1 = colnames(data)
        lab1 = label
      }
      
      selectInput("serious", h5("Please select the condition of serious adverse event"),
                  choices = setNames(col1,lab1))
    }
    
  })
  ##----------------------irAE----------------------------------
  output$irAE <- renderUI({
    if(is.null(input$file1))
      return(NULL)
    if(input$checkirae == F)
      return(NULL)
    else{
      data = adae()
      label = data %>% map(attr_getter("label"))
      idx1 = grep(pattern = "ir|irAE|immune",label)
      if(length(idx1) != 0){
        col1 = colnames(data)[idx1]
        lab1 = label[idx1]
      }else{
        col1 = colnames(data)
        lab1 = label
      }
      selectInput("immunere", h5("Please select the variable of immune related adverse event"),
                  choices = setNames(col1,lab1))
    }
    
  })
  
  ##----------------------infusion----------------------------------
  output$infusion <- renderUI({
    if(is.null(input$file1))
      return(NULL)
    if(input$checkinfu == F)
      return(NULL)
    else{
      data = adae()
      label = data %>% map(attr_getter("label"))
      idx1 = grep(pattern = "infusion|Infusion",label)
      if(length(idx1) != 0){
        col1 = colnames(data)[idx1]
        lab1 = label[idx1]
      }else{
        col1 = colnames(data)
        lab1 = label
      }
      selectInput("infusionre", h5("Please select the variable for infusion reaction"),
                  choices = setNames(col1,lab1))
    }
    
  })
  
  ##--------------phase----------------------
  
  output$pvar <- renderUI({
    if(input$checkp == FALSE || is.null(input$file1))
      return(NULL)
    else{
      data = adae()
      label = data %>% map(attr_getter("label"))
      idx1 = grep("Phase",label)
      if(length(idx1) != 0){
        col1 = colnames(data)[idx1]
        lab1 = label[idx1]
      }else{
        col1 = colnames(data)
        lab1 = label
      }
      
      selectInput("pvar", h5("Select the variable of phase"),
                  choices = setNames(col1,lab1))
      
    }
    
  })
  
  output$pval <- renderUI({
    if(input$checkp == FALSE || is.null(input$pvar))
      return(NULL)
    else{
      data = adae()
      rel = unique(data[[input$pvar]])
      rel = rel[rel != ""]
      radioButtons("pval", label = NULL, choices = setNames(rel,rel))  
    }
  })
  
  
  ##--------------other criteria----------------------
  
  output$ovar <- renderUI({
    if(input$checko == FALSE || is.null(input$file1))
      return(NULL)
    else{
      data = adae()
      # idx = grep("REL",colnames(data))
      # relcol = colnames(data)[idx]
      # rellab = label()[idx]
      selectInput("ovar", h5("Select the variable"),
                  choices = setNames(names(data),data %>% map(attr_getter("label"))))
      
    }
    
  })
  
  output$oval <- renderUI({
    if(input$checko == FALSE || is.null(input$ovar))
      return(NULL)
    else{
      data = adae()
      rel = unique(data[[input$ovar]])
      rel = rel[rel != ""]
      checkboxGroupInput("oval", label = NULL, choices = setNames(rel,rel))  
    }
  })
  
  
  
  
  ##---------------specific percentage of occurrence-------------------------  
  # output$pctcol <- renderUI({
  #   if(input$checkpct == F)
  #     return(NULL)
  #   selectInput("pctcol",NULL,choices = "")
  # })
  
  output$pct <- renderUI({
    if(input$checkpct == FALSE)
      return(NULL)
    else{
      numericInput("pct", label = "Please enter the minimum percentage you are interested in",value = 5)
    }
  })
  ##--------------------------data preview----------
  datapre <- reactive({
    data1 <- input$file1
    adae <- read_sas(data1$datapath)
    label = as.data.frame(adae %>% map(attr_getter("label")))
    for (i in names(label)) {
      if(is.null(label[[i]])){
        label[[i]] <- i
      }
    }
    colnames(adae) <- label
    adae
  })
  
  output$dataprez <- renderDT({
    req(input$file1)
    datatable(datapre()[1:50,],options=list(scrollX = T),rownames = F)
  })
  output$datapresl <- renderDT({
    req(input$file0)
    adsl <- adsl()
    label = as.data.frame(adsl %>% map(attr_getter("label")))
    for (i in names(label)) {
      if(is.null(label[[i]])){
        label[[i]] <- i
      }
    }
    colnames(adsl) <- label
    datatable(adsl[1:50,],options=list(scrollX = T),rownames = F)
  })
  
  
  ##----------------------creat the dataset-----------------------------------------------------  
  
  observeEvent(input$action,{  
    
    info <- reactive({
      if(input$pop == 2){
        adae <- adae()
        adsl <- adsl()
        adae <- adae %>%
          left_join(adsl,by = "USUBJID")
        adae
      }
      else{
        adae()
      }
    })
    
    
    
    
    
    adae_gp <- reactive({
      
      adae <- info()
      if(input$pop == 1){
        adae["TRTA"] = adae[input$var]
        adae_gp = subset(adae,TRTA %in% as.vector(input$val))
      }
      else{
        if(input$slvar %in% colnames(adae)){
          adae["TRTA"] = adae[input$slvar]
          adae_gp = subset(adae,TRTA %in% as.vector(input$slval))
        }
        else{
          adae["TRTA"] = adae[paste(input$slvar,".x",sep = "")]
          adae_gp = subset(adae,TRTA %in% as.vector(input$slval))
        }
        for (i in as.vector(input$slval)) {
          adae_gp[adae_gp$TRTA == i,]$TRTA = input[[paste("n_",i)]]
        }
        
      }
      adae_gp
      # }
    })
    
    
    adae_demo <- reactive({
      if(input$pop == 2 && input$add == T){
        adae = adae_gp()
        if(input$demo %in% colnames(adae)){
          adae["demo"] = adae[input$demo]
          adae_demo = subset(adae,demo %in% as.vector(input$demoval))
        }
        else{
          adae["demo"] = adae[paste(input$demo,".x",sep = "")]
          adae_demo = subset(adae,demo %in% as.vector(input$demoval))
        }
        for (i in as.vector(input$demoval)) {
          adae_demo[adae_demo$demo == i,]$demo = input[[paste("n1_",i)]]
        }
        adae_demo$TRTA = paste(adae_demo$TRTA,adae_demo$demo,sep = "+")
        print(adae_demo$TRTA)
        adae_demo
      }
      else
        return(adae_gp())
      
    })
    
    adae_ph <- reactive({
      if(is.null(input$pvar))
        return(adae_demo())
      else{
        adae_demo <- adae_demo()
        adae_demo["PHASE"] = adae_demo[input$pvar]
        adae_ph = subset(adae_demo,PHASE == input$pval)
        adae_ph
      }
    })
    
    
    
    adae_re <- reactive({
      if(is.null(input$relg))
        return(adae_ph())
      else{
        adae_ph <- adae_ph()
        adae_ph["AEREL"] = adae_ph[input$rel]
        relg = as.vector(input$relg)
        adae_re = subset(adae_ph,AEREL %in% c(relg,""))
        adae_re
      }
    })
    
    
    adae_sr <- reactive({
      if(input$checksr == F)
        return(adae_re())
      else{
        adae_sr <- adae_re()
        adae_sr["AESER"] = adae_sr[input$serious]
        adae_sr = subset(adae_sr,AESER %in% "Y")
        adae_sr
      }
    })
    
    adae_irae <- reactive({
      if(input$checkirae == F)
        return(adae_sr())
      else{
        adae_irea = adae_sr()
        adae_irea["AEirAE"] = adae_irea[[input$immunere]]
        adae_irea = subset(adae_irea,AEirAE %in% c("Y","Yes","是"))
        adae_irea
      }
    })
    
    adae_o <- reactive({
      if(input$checko == F)
        return(adae_irae())
      else{
        adae_o = adae_irae()
        adae_o["adae_o"] = adae_o[[input$ovar]]
        adae_o = subset(adae_o,adae_o %in% as.vector(input$oval))
        adae_o
      }
    })
    
    adae_infu <- reactive({
      if(input$checkinfu == F)
        return(adae_o())
      else{
        adae_infu = adae_o()
        adae_infu["inre"] = adae_infu[[input$infusionre]]
        adae_infu = subset(adae_infu,inre %in% c("Y","Yes","y","yes","YES"))
        adae_infu
      }
    })
    
    
    
    adae_sev_all <- reactive({
      if(input$checks == F)
        return(adae_infu())
      else{
        adae_sev_all = adae_infu()
        adae_sev_1 = adae_infu()
        adae_sev_1["AETOXGR"] = adae_sev_1[input$sele]
        selg = as.vector(input$selg)
        adae_sev_1 <- adae_sev_1 %>% 
          group_by(USUBJID) %>%
          arrange(desc(AETOXGR)) %>%
          slice_head(n = 1)
        if(is.null(input$selg) == F){
          for(i in selg){
            df = subset(adae_sev_1,AETOXGR %in% i)
            df$grade = i
            adae_sev_all = bind_rows(adae_sev_all,df)
          }
        }
        adae_sev_1$AETOXGR <- gsub("[^0-9.]","",adae_sev_1$AETOXGR)
        if(input$checkgt3 == T){
          df = subset(adae_sev_1,AETOXGR %in% c("3","4","5"))
          df$grade = "Grade >= 3"
          adae_sev_all = bind_rows(adae_sev_all,df)
        }
        if(input$checklw3 == T){
          df = subset(adae_sev_1,AETOXGR %in% c("1","2"))
          df$grade = "Grade < 3"
          adae_sev_all = bind_rows(adae_sev_all,df)
        }
        adae_sev_all
        
      }
      
    })
    
    adae_sev_soc <- reactive({
      if(input$checks == F)
        return(adae_infu())
      else{
        adae_sev_soc = adae_infu()
        adae_sev_1 = adae_infu()
        adae_sev_1["AETOXGR"] = adae_sev_1[input$sele]
        selg = as.vector(input$selg)
        adae_sev_1 <- adae_sev_1 %>% 
          group_by(USUBJID,AESOC) %>%
          arrange(desc(AETOXGR)) %>%
          slice_head(n = 1)
        if(is.null(input$selg) == F){
          for(i in selg){
            df = subset(adae_sev_1,AETOXGR %in% i)
            df$grade = i
            adae_sev_soc = bind_rows(adae_sev_soc,df)
          }
        }
        adae_sev_1$AETOXGR <- gsub("[^0-9.]","",adae_sev_1$AETOXGR)
        if(input$checkgt3 == T){
          df = subset(adae_sev_1,AETOXGR %in% c("3","4","5"))
          df$grade = "Grade >= 3"
          adae_sev_soc = bind_rows(adae_sev_soc,df)
        }
        if(input$checklw3 == T){
          df = subset(adae_sev_1,AETOXGR %in% c("1","2"))
          df$grade = "Grade < 3"
          adae_sev_soc = bind_rows(adae_sev_soc,df)
        }
        adae_sev_soc
        
      }
      
    })
    
    
    adae_sev_pt <- reactive({
      if(input$checks == F)
        return(adae_infu())
      else{
        adae_sev_pt = adae_infu()
        adae_sev_1 = adae_infu()
        adae_sev_1["AETOXGR"] = adae_sev_1[input$sele]
        selg = as.vector(input$selg)
        adae_sev_1 <- adae_sev_1 %>% 
          group_by(USUBJID,AESOC,AEDECOD) %>%
          arrange(desc(AETOXGR)) %>%
          slice_head(n = 1)
        if(is.null(input$selg) == F){
          for(i in selg){
            df = subset(adae_sev_1,AETOXGR %in% i)
            df$grade = i
            adae_sev_pt = bind_rows(adae_sev_pt,df)
          }
        }
        adae_sev_1$AETOXGR <- gsub("[^0-9.]","",adae_sev_1$AETOXGR)
        if(input$checkgt3 == T){
          df = subset(adae_sev_1,AETOXGR %in% c("3","4","5"))
          df$grade = "Grade >= 3"
          adae_sev_pt = bind_rows(adae_sev_pt,df)
        }
        if(input$checklw3 == T){
          df = subset(adae_sev_1,AETOXGR %in% c("1","2"))
          df$grade = "Grade < 3"
          adae_sev_pt = bind_rows(adae_sev_pt,df)
        }
        adae_sev_pt
        
      }
      
    })
    
    
    
    
    t_ae_1 <- reactive({
      
      adae_all = adae_sev_all()
      adae_soc = adae_sev_soc()
      print(adae_soc[adae_soc$AESOC == "Not Coded",])
      adae_pt = adae_sev_pt()
      pop = pop()
      
      t01 <- adae_all %>%
        group_by(TRTA,grade) %>%
        summarise(n = n_distinct(USUBJID)) %>%
        left_join(pop, by = "TRTA") %>%
        mutate(AESOC="All", AEDECOD=AESOC, order = 0,order_all = 0,
               pct = fmt_num(n / tot * 100, digits = 1),
               pct = paste0(n," (", gsub("\\s+","",pct), ")")
               
        )
      
      # by SOC 
      
      t1 <- adae_soc %>%
        group_by(TRTA,grade,AESOC) %>%
        summarise(n = n_distinct(USUBJID)) %>%
        left_join(pop, by = "TRTA") %>%
        mutate(AEDECOD = AESOC, order =  0,order_all = 1,
               pct = n / tot * 100)
      
      
      
      # by PT
      
      #nsoc=t1 %>%
      #  select(TRTA,TRTAN,grade,AESOC, n)  %>%
      #  rename(nsoc=n) 
      
      
      
      t2 <- adae_pt %>%
        group_by(TRTA,grade,AESOC,AEDECOD) %>%
        summarise(n = n_distinct(USUBJID)) %>%
        left_join(pop, by = "TRTA") %>%
        mutate(order = 1,order_all = 1,
               pct = n / tot * 100)
      if(input$bysoc == T){
        t2$AEDECOD <- paste("    ",t2$AEDECOD)
      }
      
      
      
      t1 <- t1 %>% 
        mutate(pct = fmt_num(pct, digits = 1),
               pct = paste0(n," (", gsub("\\s+","",pct), ")"))
      
      t2 <- t2 %>% 
        mutate(pct = fmt_num(pct, digits = 1),
               pct = paste0(n," (", gsub("\\s+","",pct), ")"))
      
      
      
      
      if(input$bysoc == T){
        if(input$bypt == T){
          t_ae_1 <- bind_rows(t01,t1, t2) 
        }else{
          t_ae_1 <- bind_rows(t01,t1)
        }
      }else{
        if(input$bypt == T){
          t_ae_1 <- bind_rows(t01,t2)
        }else{
          t_ae_1 <- t01
        }
      }
      
      t_ae_1
      
    })  
    
    
    
    
    t_ae_2 <- reactive({
      t_ae_2 <- t_ae_1() %>%
        mutate(TRTA_grade=paste(TRTA,grade,sep="_")) %>%
        pivot_wider(
          id_cols = c(order_all,AESOC, order, AEDECOD),
          names_from = TRTA_grade,
          names_prefix = "pct_",
          values_from = pct,
          values_fill = fmt_num(0, digits = 0)
        ) %>%
        arrange(AESOC, order) %>%
        select(AESOC, AEDECOD, order, starts_with("pct"))
      
      sname = colnames(t_ae_2)
      sname = sname[4:length(sname)]
      sname1 <- gsub("_","  ",sname)
      sname1 <- gsub("pct","",sname1)
      
      updateSelectInput(session  = getDefaultReactiveDomain(),"col",choices = setNames(sname,sname1))
      
      t_ae_2  
    })
    
    tae <- reactive({
      t_ae <- t_ae_2()
      
      t_ae["sort_v"] = t_ae[input$col]
      t_ae["sort_v"] = as.numeric(gsub("\\(.*","",t_ae$sort_v))
      if(input$checkasc == T){
        t_ae <- t_ae %>%
          arrange(AESOC,order,sort_v) %>%
          select(AESOC, AEDECOD, starts_with("pct"))
      }
      else{
        if(input$bysoc == T){
          t_ae <- t_ae %>%
            arrange(AESOC,order,desc(sort_v),AEDECOD) %>%
            select(AESOC, AEDECOD, starts_with("pct"))
          
        }else{
          if(input$bypt == T){
            t_ae <- t_ae %>%
              arrange(order,desc(sort_v),AEDECOD) %>%
              select(AESOC, AEDECOD, starts_with("pct"))
          }
        }
      }
      
      
      
      if(input$checkpct == T){
        t_ae <- t_ae[rowSums(sapply(t_ae[,3:ncol(t_ae)],function(x) as.numeric(gsub(".*\\((.*)\\).*","\\1",x))) > input$pct)>0,]
        t_ae <- t_ae %>%
          select(AESOC,AEDECOD,starts_with("pct"))
      }
      else{
        t_ae
      }
      
      t_ae <- as.data.frame(t_ae)
      colnames(t_ae)[1] <- "SOC"
      colnames(t_ae)[2] <- "PT"
      
      if(!is.null(input$checkallg) && input$checkallg == F){
        t_ae = t_ae[,!grepl("All Grade",colnames(t_ae))]
        if(input$bysoc == T){
          row_n <- which(rowSums(t_ae[,3:ncol(t_ae)] == "   0") != ncol(t_ae)-2)
          t_ae = t_ae[row_n,]
        }else{
          row_n <- which(rowSums(t_ae[,3:ncol(t_ae)] == "0") != ncol(t_ae)-2)
          t_ae = t_ae[row_n,]
          
        }
      }
      
      t_ae
    })
    
    
    
    output$rt <- downloadHandler(
      filename = "AEsummary.xlsx",
      content = function(file){
        t = test1()
        if(input$bysoc == T){
          final_table = t
        }else{
          final_table = t[,2:ncol(t)]
        }
        
        
        write.xlsx(final_table,file,firstRow = T,colWidths = "auto")
      }
    )
    
    
    
    output$level <- renderUI({
      if(input$colo == F)     
        return(NULL)
      data <- test()
      val = colnames(data)[3:ncol(data)]
      # map(val,~numericInput(paste("rank_",.),label = paste0("The position to place ",.),value = NULL))
      # map(val,~actionButton(paste("rank_",.),label = .,icon("arrow-up")))
      rank_list(
        text = "Drag the items in any desired order",
        labels = val,
        input_id = "rank_list")
    })
    
    test1 <- reactive({
      data <- test()
      if(input$colo == F){
        data
      }else{
        if(!is.null(input$rank_list)){
          val = as.vector(input$rank_list)
          val <- paste(" ",val,sep = "")
          data <- data[,c("SOC","PT",val)]
        }else{
          data
        }
        
      }
    })
    
    
    test <- reactive({
      t_ae <- tae()
      pop <- pop()
      colnames(t_ae) <- gsub("_"," ",colnames(t_ae))
      colnames(t_ae) <- gsub("pct","",colnames(t_ae))
      # n = 1
      # if(length(input$checkgt3) != 0 && input$checkgt3 == T){
      #   n = n+1
      # }
      # if(length(input$checklw3) != 0 && input$checklw3 == T){
      #   n = n+1
      # }
      # if(length(input$checkallg) != 0 && input$checkallg == F){
      #   n = n-1
      # }
      # if(length(input$selg) >0){
      #   print(length(input$selg))
      #   n = n+length(input$selg)
      # }
      # print(rep(paste("N = ",pop[["tot"]]),n))
      #!is.null(input$checkallg) && input$checkallg == F
      
      # for (i in 3:n) {
      #   colnames(t_ae)[i] <- paste(colnames(t_ae)[i+2]," N=",pop()[i,2])
      # } 
      
      t_ae
      # if(input$checks == T){
      #   
      #   if(is.null(input$selg) == F && input$checkgt3 == T && input$checklw3 == T){
      #     a=c(" "," ")
      #     b=c(" "," ")
      #     for(i in 1:nrow(pop)){
      #       k=as.character(pop[i,1])
      #       n = length(unique(t_ae$grade[t_ae$TRTA == pop[i,1]]))
      #       g = unique(t_ae$grade[t_ae$TRTA == pop[i,1]])
      #       a = append(a,c(paste(pop[i,1],"  N=",pop[i,2]),replicate(n-1," ")))
      #       b = append(b,g)
      #     }
      #     data.frame(rbind(a,b))
      #   }
      #   
      #   else if(is.null(input$selg) == F && input$checkgt3 == T  && input$checklw3 == F){
      #     a=c(" "," ")
      #     b=c(" "," ")
      #     for(i in 1:nrow(pop)){
      #       n = length(unique(t_ae$grade[t_ae$TRTA == pop[i,1]]))
      #       g = unique(t_ae$grade[t_ae$TRTA == pop[i,1]])
      #       a = append(a,c(paste(pop[i,1],"  N=",pop[i,2]),replicate(n-1," ")))
      #       b = append(b,g)
      #     }
      #     data.frame(rbind(a,b))
      #   }
      #   
      #   else if(is.null(input$selg) == F && input$checkgt3 == F  && input$checklw3 == T){
      #     a=c(" "," ")
      #     b=c(" "," ")
      # 
      #     for(i in 1:nrow(pop)){
      #       n = length(unique(t_ae$grade[t_ae$TRTA == pop[i,1]]))
      #       g = unique(t_ae$grade[t_ae$TRTA == pop[i,1]])
      #       a = append(a,c(paste(pop[i,1],"  N=",pop[i,2]),replicate(n," ")))
      #       b = append(b,g)
      #     }
      #     data.frame(rbind(a,b))
      #   }
      #   
      #   else if(is.null(input$selg) == F && input$checkgt3 == F  && input$checklw3 == F){
      #     a=c(" "," ")
      #     b=c(" "," ")
      #     for(i in 1:nrow(pop())){
      #       n = length(unique(t_ae$grade[t_ae$TRTA == pop[i,1]]))
      #       g = unique(t_ae$grade[t_ae$TRTA == pop[i,1]])
      #       a = append(a,c(paste(pop[i,1],"  N=",pop[i,2]),replicate(n," ")))
      #       b = append(b,c(g,"All Grades"))
      #     }
      #     data.frame(rbind(a,b))
      #   } 
      #   
      #   else if(is.null(input$selg) && input$checkgt3 == F && input$checklw3 == T){
      #     a=c(" "," ")
      #     b=c(" "," ")
      #     for(i in 1:nrow(pop())){
      #       a = append(a,c(paste(pop()[i,1],"  N=",pop()[i,2])," "))
      #       b = append(b,c("Grade < 3","All Grades"))
      #     }
      #     data.frame(rbind(a,b))
      #   }
      #   
      #   else if(is.null(input$selg) && input$checkgt3 == T && input$checklw3 == F){
      #     a=c(" "," ")
      #     b=c(" "," ")
      #     for(i in 1:nrow(pop())){
      #       a = append(a,c(paste(pop()[i,1],"  N=",pop()[i,2])," "))
      #       b = append(b,c("Grade >= 3","All Grades"))
      #     }
      #     data.frame(rbind(a,b))
      #   }
      #   
      #   
      #   else if(is.null(input$selg) && input$checkgt3 == T && input$checklw3 == T){
      #     a=c(" "," ")
      #     b=c(" "," ")
      #     for(i in 1:nrow(pop())){
      #       a = append(a,c(paste(pop()[i,1],"  N=",pop()[i,2])," "," "))
      #       b = append(b,c("Grade < 3","Grade >= 3","All Grades"))
      #     }
      #     data.frame(rbind(a,b))
      #   }
      # }
      # 
      # else{
      #   a=c(" "," ")
      #   for(i in 1:nrow(pop())){
      #     a = append(a,c(paste(pop()[i,1],"  N=",pop()[i,2])))
      #   }
      #   a
      # }
    })   
    
    
    output$test <- DT::renderDataTable({
      if(is.null(input$file1))     
        return(NULL)
      # t = test()
      # if(is.data.frame(t)){
      #   colnames(t) = colnames(tae())
      # }
      # final_table = rbind(t,tae())#
      final_table = test1()
      rownames(final_table)<-NULL
      formattable(final_table,align = "l",header = F,
                  list(SOC = F,area(col = 1:ncol(tae()))~formatter("span",style = ~ style(font.weight = ifelse(final_table[,2] != SOC,"normal","bold"))))) %>%
        as.datatable(options=list(scrollX = T,ordering = F),rownames = F)
    }) 
    
    
    
    
  })  
  
  
  
  tbl_ae_spec <-reactive({
    tbl_ae_spec <- t_ae() %>%
      mutate(AEDECOD = ifelse(AEDECOD == AESOC,
                              AEDECOD, paste0("  ", AEDECOD)
      ))
    
    n_row <- nrow(tbl_ae_spec)
    n_col <- ncol(tbl_ae_spec)
    id <- tbl_ae_spec$AESOC == tbl_ae_spec$AEDECOD
    id <- ifelse(is.na(id), FALSE, id)
    
    text_format <- ifelse(id, "b", "")
    
    tbl_ae_spec %>%
      rtf_title(
        "Analysis of Participants With Specific Adverse Events",
        "(Safety Analysis Population)"
      ) %>%
      rtf_colheader(" | Placebo | Xanomeline Low Dose| Xanomeline High Dose",
                    col_rel_width = c(3, rep(1, 3)),border_top = "",
                    border_bottom = "single"
      ) %>%
      rtf_body(
        col_rel_width = c(1, 3, rep(1, 3)),
        text_justification = c("l", "l", rep("c", 3)),
        text_format = matrix(text_format, nrow = n_row, ncol = n_col),
        page_by = "AESOC",
        pageby_row = "first_row"
      ) %>%
      rtf_footnote("Every subject is counted a single time for each applicable row and column.") %>%
      rtf_encode()
  })
  
  
  
  
  #-------------------------------------------------------------------------------------------------
  
  #--------------------------------#MAPPING#--------------------------------------------------------
  
  
  merge_data <- reactive({
    adae <- adae()
    
    adae
  })
  
  
  
  output$miss <- renderText({
    if(is.null(input$filet) || is.null(input$soct) || is.null(input$ptt))
      return(NULL)
    paste0("The following ", nrow(miss_v()), " obeservations do not have matching terms")
  })
  
  output$miss_d <- DT::renderDataTable({
    if(is.null(input$filet) || is.null(input$soct) || is.null(input$ptt))
      return(NULL)
    DT::datatable(miss_v(),rownames = F)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

