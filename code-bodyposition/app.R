
###################### Shiny App with dynamic meta-analyses of body position effects
# NOTE: Add "ANONYMIZED" after peer-review

# ABOUT text --------------------------------------------------------------
version <- "v0.70"
date <- "05 November, 2021"
about <- HTML(paste("<h3>Dynamic Meta-Analysis of Body Positions Shiny App ", version, "</h3>"
         , "<br/><br/><b>Last Update:</b> ", date
         , "<br/><br/><b>Citation:</b> Röseler, L., Körner, R., & Schütz, A. (2021). Dynamic Meta-Analysis. Retrieved from <a href = https://osf.io/ns65r/>https://osf.io/ns65r/</a>"
         , "<br/><br/><b>Data and Materials:</b> <a href = https://osf.io/vz4a6/>https://osf.io/vz4a6/</a> "
         , "<br/><br/><b>Add Study:</b> Submit new or missing studies here: <a href = https://www.soscisurvey.de/submissions_bodypositions>https://www.soscisurvey.de/submissions_bodypositions</a> "
         , "<br/><br/><b>Contact:</b> Contact us via mail if you have suggestions or found bugs: lukas.roeseler@uni-bamberg.de"
         , "<br/><br/><b>Roadmap:</b>"
         , "<br/>- Submission Portal v2 (including control groups) is now available! If the set of studies that have a control group grows, we will add control group analyses."
         , "<br/>- Download-button for filtered dataset will be added."
         , "<br/>- Download-buttons for plots will be added."
         , "<br/>- Bug fixes for submission portal (e.g., decimal comma may break the whole app, 'other' dv-type leads to study being excluded)."
         , "<br/>- Fix Forest Plot (Error Bars are not displayed due to Plotly bug)."
         , "<br/>- Add info boxes for filter settings."
         , "<br/>- Fix error that is caused by missing values of unverified studies in the forest plot."
         , "<br/>- Fix error that is caused by too view values of unverified studies in the summary plot."
         , "<br/>- Fix check box for design (ws/bs)."
         , "<br/>- Add baseline REML estimate for unfiltered studies."
         , "<br/>- Add Outlier Diagnostics."
         , sep = ""))


# Other texts that are fixed
go <- HTML("<p style=\"color:blue\">Choose your set of studies and click on <b>Analyze data</b> to see the results.</p><br/>")

summary_overview_main <- HTML("<br/><br/><h3>Sample Overview</h3>") # <b></b>
summary_table_main <- HTML("<br/><br/><h3>Effect Size Estimates</h3>") # <b></b>
summary_table_notes <- HTML("<i>Notes:</i> LCL = Lower Confidence Limit, UCL = Upper Confidence Limit, k = number of effects.")

firstvalues <- HTML("<br/><br/><i>Notes:</i> First reported values are selected for analyses.")

qrppb_main <- HTML("<h3>Publication Bias and Questionable Research Practices</h3>")
funnel_main <- HTML("<h3>Funnel Plot of Effects</h3>")

eggers_main <- HTML("<h3>Egger's Test of Funnel Plot Asymmetry</h3>")

pcurve_main <- HTML("<h3>P-Curve of Effects")
pcurve_notes <- HTML("<h5><br/><br/><i>Notes:</i> P-curve can only be plotted if there are 2 or more significant effects.</h5>")

diagnostics_main <- HTML("<h3>Distribution of Effect Sizes")
diagnostics_het <- HTML("<h3>Heterogeneity (REML)<h5>")
diagnostics_notes <- HTML("<h5><br/><br/><i>Notes:</i> More tools for outlier diagnostics will be added soon.</h5>")

scroll <- print("Scroll down to see forest plot.")

# Load packages
library(shiny)
# library("dmetar")
library(meta)
library(metafor)
library(readxl)
library(rlang)
library(pwr)
library(effsize)
library(esc)
library(puniform)
library(xlsx)
library(BSDA)
library(psych)
library(ggplot2)
library(shinycssloaders)
library(dplyr)
library(writexl)
library(car)
library(plotly)
library(robumeta)
library(stringr)
library(poibin)
# source_url('https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/eggers.test.R')
# source_url('https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/SE_from_p.R')
# source_url('https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/pcurve2.R')
source("eggers.test.R")
source("pcurve2.R")
source("sefromp.R")


### Link to dataset works if project is made public
# datalink <- "https://osf.io/wx45p/download"
# ppbase <- readxl::read_xlsx(datalink, sheet = 1)

### Open Dataset from hard drive
ppbase <- readxl::read_xlsx("PPmetadata_processed.xlsx", sheet = 1)

# compute year for references
ppbase$year <- as.numeric(gsub("[^0-9]", "", ppbase$reference))

# Exclusions neecessary for data analysis
ppbase <- ppbase[ppbase$include == TRUE,] # use only complete cases
ppbase <- ppbase[!is.na(ppbase$include),] # use only complete cases
ppbase <- ppbase[ppbase$se != 0, ] # use only cases with se > 0
ppbase$esg <- ppbase$g
ppbase <- ppbase[!is.na(as.numeric(ppbase$culture)), ]
ppbase <- ppbase[!is.na(as.numeric(ppbase$pose_posture)), ]
ppbase <- ppbase[!is.na(as.numeric(ppbase$coverstory)), ]
ppbase$checked <- 1
ppbase$reference_studyno <- paste(ppbase$reference, ", Study ", ppbase$study_no, sep = "")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dynamic Meta-Analysis of Body Position Effects"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("dvtype", label = ("DV Type")
                                , choices = list("Physiology" = 1, "Self-Report" = 2, "Behavior" = 3)
                                , selected = list(1, 2, 3))
            , checkboxGroupInput("prereg", label = ("Preregistration")
                            , choices = list("Preregistered" = 1, "Not preregistered" = 2)
                            , selected = list(1, 2))
            , checkboxGroupInput("poseposture", label = ("Type of Body Positions")
                            , choices = list("Poses" = 1, "Postures" = 2)
                            , selected = list(1, 2))
            , checkboxGroupInput("culture", label = ("Cultural Background")
                            , choices = list("Western" = 1, "Eastern" = 2)
                            , selected = list(1, 2))
            , checkboxGroupInput("coverstory", label = ("Cover story")
                            , choices = list("Yes" = 1, "No" = 2)
                            , selected = list(1, 2))
            # , checkboxGroupInput("design", label = ("Manipulation Design")
            #                    , choices = list("Between subjects" = 1, "Within Subjects" = 2)
            #                    , selected = list(1, 2))
            , checkboxGroupInput("checked", label = ("New and unverified submissions")
                                 , choices = list("Unverified" = 1, "Verified" = 2)
                                 , selected = list(1, 2))
            , sliderInput("outliers", "Exclude based on z-scores", min = -6, max = 6, value = c(-6, 6), step = .1, sep = "")
            , sliderInput("year", "Year of Publication", min = 1982, max = 2023, value = c(1982, 2023), sep = "")
            , actionButton("go", "Analyze data")
            # , downloadButton("downloadData", "Download dataset") # does not work yet (see roadmap)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(type = "tabs"
                        , tabPanel("Summary"
                                   , go
                                   , summary_overview_main
                                   , tableOutput("sample") %>% withSpinner()
                                   , summary_table_main
                                   , plotOutput("forest", width = "100%") %>% withSpinner()
                                   , tableOutput("effectestimate")
                                   , summary_table_notes
                                   )
                        , tabPanel("Forest Plot", go, scroll, plotOutput("foreststudies") %>% withSpinner(), cellArgs = list(style = "vertical-align: top"))
                        , tabPanel("QRP/PB", go, qrppb_main, firstvalues, funnel_main, plotOutput("funnel", width = "100%") %>% withSpinner()
                                   , eggers_main, tableOutput("eggers") %>% withSpinner()
                                   , pcurve_main, plotOutput("pcurve") %>% withSpinner(), pcurve_notes)
                        , tabPanel("Outlier Diagnostics"
                                   , diagnostics_main
                                   , plotlyOutput("violin", height = 500) %>% withSpinner()
                                   , diagnostics_het
                                   , tableOutput("heterogeneity") %>% withSpinner()
                                   , diagnostics_notes)
                        , tabPanel("About/Add Study", about)
                        )
        )
    )
)

# Define server logic
server <- function(input, output) { # , session
    
    # Apply reactive filtering of dataset when clicking on the button
    ppfiltered <- eventReactive(input$go, {
        ppreactive()
    })
    
    ppreactive <- reactive({
        pp <- ppbase
        
        ### MERGE ORIGINAL DATASET WITH NEW SUBMISSIONS ###
        # download dataset
        masp <- read.csv("https://www.soscisurvey.de/submissions_bodypositions/?act=El1WvKL2pjHR4kzUyNXVxaYe"
                         , header = TRUE, sep = "\t", quote = "\"'", dec = ".")
        masp <- masp[7:nrow(masp), ] # exclude test runs and one error
        
        # change id so that there are no duplicates
        masp$CASE <- masp$CASE+1000
        
        # change variable names so that dataset can be merged with meta-analysis dataset
        names(masp) <- c("id1", "SERIAL", "REF", "QUESTNNR", "MODE", "STARTED", "ref_full", "reference", "id2", "nhighlow" , "dv", "year", "pexact", "g", "se", "dataset_link", "ca_mail"
                         , "ncontrol"
                         , "pval_highcontrol", "ghc", "sehc"
                         , "pval_lowcontrol", "glc", "selc"
                         , "nhigh", "nlow"
                         , "ws", "pose_posture", "coverstory", "dvtype", "culture", "preregistered", "TIME001", "TIME_SUM", "MAILSENT", "LASTDATA", "FINISHED", "Q_VIEWER", "LASTPAGE", "MAXPAGE")
        
        # Recode values
        masp$se <- abs(masp$se)
        masp$esg <- as.numeric(as.character(masp$g))
        masp$ws <- ifelse(masp$ws == 1, 1, 0)
        masp$dvtype <- car::recode(masp$dvtype, "1 = 'Physiology'; 2 = 'Behavior'; 3 = 'Self-Reports'")
        masp$preregistered <- ifelse(masp$preregistered == 1, 1, 0)
        masp$culture <-  car::recode(masp$culture, "1 = 'Western'; 2 = 'Eastern'")
        masp$pose_posture <- ifelse(masp$pose_posture == 1, 1, 0)
        masp$coverstory <- ifelse(masp$coverstory == 1, 1, 0)
        masp$checked <- 0 # create new variables (checked items are changed individually)
        masp$dv <- paste(substr(masp$dv, 1, 30), "...", sep = " ")
        masp$id2 <- masp$id2 + 1000
        masp$reference <- paste("[UNVERIFIED]", masp$reference, sep = " ")
        masp$ntotal <- rowSums(cbind(masp$nhighlow, as.numeric(as.character(masp$ncontrol))), na.rm = TRUE)
        
        # Exclude incomplete cases in MASP
        # masp <- masp[complete.cases(masp[, c(10, 11, 15, 41)]), ] # esg: 41, se: 15, n: 10, dv: 11 | ADD dv
        masp <- masp[complete.cases(masp[, c("esg", "se", "nhighlow", "dv")]), ] # esg: 41, se: 15, n: 10, dv: 11 | ADD dv
        # names(masp)
        
        

# Merge dataset with MASP data --------------------------------------------
#### TEMPORARY DISABLED ####
        # pp <- base::merge(x = pp, y = masp, all.x = TRUE, all.y = TRUE)
        
        
        pp$vi <-  metafor::escalc(measure = "SMD"
                                  , yi = pp$esg
                                  , sei = pp$se
                                  , ni = pp$nhighlow
                                  , data = pp)$vi
        pp$zg <- scale(pp$esg)
        
        # Filter by dvtype
        if (!any(input$dvtype == "1")) {
            pp <- pp[pp$dvtype != "Physiology", ]
        }
        if (!any(input$dvtype == "2")) {
            pp <- pp[pp$dvtype != "Self-Reports", ]
        }
        if (!any(input$dvtype == "3")) {
            pp <- pp[pp$dvtype != "Behavior", ]
        }
        
        
        # Filter by culture
        if (!any(input$culture == "1")) {
          pp <- pp[pp$culture != 0, ]
        }
        if (!any(input$culture == "2")) {
          pp <- pp[pp$culture != 1, ]
        }
        
        
        # Filter by pose vs. posture
        if (!any(input$poseposture == "1")) {
          pp <- pp[pp$pose_posture != 0, ]
        }
        if (!any(input$poseposture == "2")) {
          pp <- pp[pp$pose_posture != 1, ]
        }
        
        # Filter by cover story
        if (!any(input$coverstory == "1")) {
          pp <- pp[pp$coverstory != 0, ]
        }
        if (!any(input$coverstory == "2")) {
          pp <- pp[pp$coverstory != 1, ]
        }
        
        # Filter by Year
        pp <- pp[pp$year >= input$year[1], ]
        pp <- pp[pp$year <= input$year[2], ]
        
        # Filter by zscore
        pp <- pp[pp$zg >= input$outliers[1], ]
        pp <- pp[pp$zg <= input$outliers[2], ]
        
        # Filter by preregistrations
        if (!any(input$prereg == 1)) {
            pp <- pp[pp$preregistered != 1, ]
        } 
        if (!any(input$prereg == 2)) {
          pp <- pp[pp$preregistered != 0, ]
        }
        
        # # Filter by design (ws,)
        # if (!any(input$design == "1")) {
        #   pp <- pp[pp$wsdesign != 0, ]
        # }
        # if (!any(input$design == "2")) {
        #   pp <- pp[pp$wsdesign != 1, ]
        # }
        
        # Filter by checked studies (concerns new studies only)
        if (!any(input$checked == "1")) {
          pp <- pp[pp$checked != 0, ]
        }
        if (!any(input$checked == "2")) {
          pp <- pp[pp$checked != 1, ]
        }
        
        # remove missing values
        pp <- pp[!is.na(pp$esg), ]

        
        pp
    })
    
    
    
    
    

# Data for forest plot and table ------------------------------------------

    estimatesfiltered <- eventReactive(input$go, {
      estimatesreactive()
    })
    
    estimatesreactive <- reactive({
      pp <- ppfiltered()
      
      metapp_total <- metafor::rma.mv(yi = esg
                                      , V = vi
                                      , random = ~1 | id2/id1 # id2 identifies studies that included multiple dvs
                                      , tdist = TRUE # knapp-hartung adjustment
                                      , data = pp
                                      , method = "ML") # REML failed to converge in tests
      
      # pp_agg <- tryCatch(aggregate(esg ~ reference_studyno, data = pp, FUN = "mean"), silent = TRUE)
      
      pp_agg <- tryCatch(aggregate(esg ~ reference_studyno, data = pp, FUN = "mean")
                         , error = function() {data.frame("reference_studyno" = as.numeric()
                                                , "esg" = as.numeric())})
      
      if (nrow(pp_agg) > 0) {
        # pp_agg$se <- aggregate(se ~ reference_studyno, data = pp[NULL, ], FUN = "mean")$se
        pp_agg$se <- try(aggregate(se ~ reference_studyno, data = pp, FUN = "mean")$se, silent = TRUE)
        pp_agg$ntotal <- try(aggregate(ntotal ~ reference_studyno, data = pp, FUN = "min")$ntotal, silent = TRUE)
        pp_agg$reference <- try(aggregate(reference ~ reference_studyno, data = pp, FUN = "min")$reference, silent = TRUE)
        pp_agg$vi <- metafor::escalc(measure = "SMD"
                                     , yi = pp_agg$esg
                                     , sei = pp_agg$se
                                     , ni = pp_agg$nhighlow
                                     , data = pp_agg)$vi
      } else {
        pp_agg$se <- as.numeric()
        pp_agg$ntotal <- as.numeric()
        pp_agg$reference <- as.numeric()
        pp_agg$vi <- as.numeric()
      }
      
      reference <- pp_agg$reference_studyno
      
      metapp_metagen_mean <- metagen(TE = esg,
                                     seTE = se,
                                     data = pp_agg,
                                     studlab = pp_agg$reference_studyno,
                                     comb.fixed = FALSE,
                                     comb.random = TRUE,
                                     method.tau = "ML", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
                                     hakn = TRUE,
                                     prediction = TRUE,
                                     sm = "SMD")
      
      pppos <- pp[!is.na(pp$p_position),]
      pp_first <- pppos[(pppos$p_position == 1 | pppos$p_position == 3) ,] # use only first or only p-value
      pp_last  <- pppos[(pppos$p_position == 2 | pppos$p_position == 3) ,] # use only last  or only p-value
      
      pppcurve_first <- data.frame("TE" = pp_first$esg, "seTE" = pp_first$se, "studlab" = pp_first$reference, "n" = pp_first$nhighlow, "id" = pp_first$id2, "nhighlow" = pp_first$nhighlow) # CHANGED: use only one p-value per sample (p-values must be statistically independent)
      pppcurve_last <- data.frame("TE" = pp_last$esg, "seTE" = pp_last$se, "studlab" = pp_last$reference, "n" = pp_last$nhighlow, "id" = pp_last$id2, "nhighlow" = pp_last$nhighlow) 
      pcurve_estimates1 <- try(pcurve(pppcurve_first, effect.estimation = TRUE, N = pppcurve_first$nhighlow, dmin = 0, dmax = 1), silent = TRUE)
      pcurve_estimates2 <- try(pcurve(pppcurve_last, effect.estimation = TRUE, N = pppcurve_last$nhighlow, dmin = 0, dmax = 1), silent = TRUE)
      
      
      
      taf_agg <- metafor::trimfill(metapp_metagen_mean)
      
      punif_agg <- puniform::puni_star(yi = pp_agg$esg, vi = pp_agg$se^2
                                       , alpha = .05
                                       , side = "right", method = "ML", boot = FALSE)
      
      
      # PET PEESE for aggregated data
      pp_agg$vi <- metafor::escalc(measure = "SMD"
                                   , yi = pp_agg$esg
                                   , sei = pp_agg$se
                                   , ni = pp_agg$nhighlow
                                   , data = pp_agg)$vi
                    
      
      pet_agg <- lm(esg~sqrt(vi), data = pp_agg, weights=1/vi)
      
      
      peese_agg <- lm(esg~vi, data = pp_agg, weights=1/vi)
      
      # RVE
      pp$reference_studyno <- paste(pp$reference, ", Study ", pp$study_no, sep = "")
      rve <- robumeta::robu(esg ~ 1, data = pp, studynum = reference_studyno, var.eff.size = se*sqrt(nhighlow), small = FALSE)
      
      
      # Hedges-Vevea selection model
      hvsm <- weightr::weightfunct(pp_agg$esg, pp_agg$vi, steps = c(0.025, 1), fe = FALSE)
      hvsm$output_adj
      # Generate table and plot
      
      estimates_explo_agg <- data.frame("Model" = as.character(), "g" = as.numeric(), "LCL" = as.numeric(), "UCL" = as.numeric(), "k" = as.numeric()
                                        , stringsAsFactors = FALSE)
      estimates_explo_agg[1,] <- c("Random-Effects Multilevel Model", metapp_total$b, metapp_total$ci.lb, metapp_total$ci.ub, metapp_total$k)
      estimates_explo_agg[2, ] <- c(("Robust Variance Estimation"), as.numeric(rve$reg_table$b.r), rve$reg_table$CI.L, rve$reg_table$CI.U, length(rve$k))
      estimates_explo_agg[3,] <- c("Trim-and-fill", taf_agg$TE.random, taf_agg$lower.random, taf_agg$upper.random, taf_agg$k)
      estimates_explo_agg[4,] <- c("P-uniform star", punif_agg$est, punif_agg$ci.lb, punif_agg$ci.ub, punif_agg$k)
      estimates_explo_agg[5, ] <- c(("Hedges-Vevea Selection Model"), as.numeric(hvsm$output_adj$par[2]), hvsm$ci.lb_adj[2], hvsm$ci.ub_adj[2], hvsm$k)
      estimates_explo_agg[6,] <- c("P-Curve (first value)", pcurve_estimates1$dEstimate, NA, NA, pcurve_estimates1$kAnalyzed)
      estimates_explo_agg[7,] <- c("P-Curve (last value)", pcurve_estimates2$dEstimate, NA, NA, pcurve_estimates2$kAnalyzed)
      estimates_explo_agg[8, ] <- c(("Precision Effect Test"), as.numeric(pet_agg$coefficients[1]), confint(pet_agg)[1, 1], confint(pet_agg)[1, 2], pet_agg$df+2)
      estimates_explo_agg[9, ] <- c(("Precision Effect Estimate using Standard Error"), as.numeric(peese_agg$coefficients[1]), confint(peese_agg)[1, 1], confint(peese_agg)[1, 2], peese_agg$df+2)
      estimates_explo_agg$g <- as.numeric(estimates_explo_agg$g)
      estimates_explo_agg$LCL <- as.numeric(estimates_explo_agg$LCL)
      estimates_explo_agg$UCL <- as.numeric(estimates_explo_agg$UCL)
      
      estimates_explo_agg[, 2:4] <- round(estimates_explo_agg[, 2:4], digits = 3)
      
      estimates_explo_agg$Model <- factor(estimates_explo_agg$Model, levels = estimates_explo_agg$Model)
      
      print(estimates_explo_agg)
      
    })    
    


# Aggregated values meta-analysis ----------------------------------------------

    
    metapp_agg <- eventReactive(input$go, {
      metapp_agg_reactive()
    })
    
    
    metapp_agg_reactive <- reactive({
    pp <- ppfiltered()
    
    
    pp_agg <- tryCatch(aggregate(esg ~ reference_studyno, data = pp[, ], FUN = "mean")
                       , error = function(x) {data.frame("reference_studyno" = as.numeric()
                                               , "esg" = as.numeric())})
    
    if (nrow(pp_agg) > 0) {
      # pp_agg$se <- aggregate(se ~ reference_studyno, data = pp[NULL, ], FUN = "mean")$se
      pp_agg$se <- try(aggregate(se ~ reference_studyno, data = pp, FUN = "mean")$se, silent = TRUE)
      pp_agg$ntotal <- try(aggregate(ntotal ~ reference_studyno, data = pp, FUN = "min")$ntotal, silent = TRUE)
      pp_agg$reference <- try(aggregate(reference ~ reference_studyno, data = pp, FUN = "min")$reference, silent = TRUE)
      pp_agg$vi <- metafor::escalc(measure = "SMD"
                                   , yi = pp_agg$esg
                                   , sei = pp_agg$se
                                   , ni = pp_agg$nhighlow
                                   , data = pp_agg)$vi
    } else {
      pp_agg$se <- as.numeric()
      pp_agg$ntotal <- as.numeric()
      pp_agg$reference <- as.numeric()
      pp_agg$vi <- as.numeric()
    }
    
    # pp_agg <- try(aggregate(esg ~ reference_studyno, data = pp, FUN = "mean"), silent = TRUE)
    # pp_agg$se <- try(aggregate(se ~ reference_studyno, data = pp, FUN = "mean")$se, silent = TRUE)
    # pp_agg$ntotal <- try(aggregate(ntotal ~ reference_studyno, data = pp, FUN = "min")$ntotal, silent = TRUE)
    # pp_agg$reference <- try(aggregate(reference ~ reference_studyno, data = pp, FUN = "min")$reference, silent = TRUE)
    metapp_metagen_mean <- metagen(TE = esg,
                                   seTE = se,
                                   data = pp_agg,
                                   studlab = paste(reference),
                                   fixed = FALSE,
                                   random = TRUE,
                                   method.tau = "ML", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
                                   hakn = TRUE,
                                   prediction = TRUE,
                                   sm = "SMD")  
    metapp_metagen_mean
    })
    
    
    
    
    


    output$sample <- renderTable({
      pp <- ppfiltered()
      # Results -----------------------------------------------------------------
      # Number of references
      nstudies <- length(unique(pp$reference))
      # Number of effects
      neffects <- length(pp$reference)
      # Number of independent studies
      nindependent <- length(unique(pp$reference_studyno))
      # Number of participants
      nsample <- sum(aggregate(nhighlow ~ id2, data = pp, FUN = "min")$nhighlow) # total sample size
      
      print(paste("The current dataset contains", nstudies, "articles,", nindependent, "independent studies and", neffects, "effects.", sep = " "))
      overview <- data.frame("Articles" = nstudies
                             , "Studies" = nindependent
                             , "Effects" = neffects
                             , "Samplesize" = round(nsample, digits = 0))
      
    })
    
    
    
    # Heterogeneity -----------------------------------------------------------
    
    output$heterogeneity <- renderTable({
    pp <- ppfiltered()
    metapp_total <- metafor::rma.mv(yi = esg
                                    , V = vi
                                    , random = ~1 | id2/id1 # id2 identifies studies that included multiple dvs
                                    , tdist = TRUE # knapp-hartung adjustment
                                    , data = pp
                                    , method = "ML") # REML failed to converge in tests    
    
    het <- data.frame("Sigma2_Level1" = metapp_total$sigma2[1]
                      , "Sigma2_Level2" = metapp_total$sigma2[2]
                      , "Tau" = metapp_total$tau2
                      , "Q" = round(metapp_total$QE, digits = 2)
                      , "Q_p" = ifelse(round(metapp_total$QEp, digits = 3) == 0, "< .001", round(metapp_total$QEp, digits = 3))
                      )
    
    print(het)
    })
    
    


# FOREST PLOT METHODS -----------------------------------------------------
    output$forest <- renderPlot({
        
        estimates_explo_agg <- estimatesfiltered()
        
        ggplot() + geom_point(data = estimates_explo_agg, aes(y = g, x = Model), stat = "identity") +
          geom_abline(slope = 0, intercept = 0, linetype = 2) + ylab("Hedges's g") +
          geom_errorbar(data = estimates_explo_agg, aes(x = Model, ymin = LCL, ymax = UCL), stat = "identity") +
          theme_bw() + coord_flip() + scale_x_discrete(limits = rev(levels(estimates_explo_agg$Model))) +
          theme(text = element_text(size = 20))
          # scale_y_continuous(limits = c(-.6, 1.2)) + 
        
    })

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

# TABLE -------------------------------------------------------------------
    output$effectestimate <- renderTable({
        
       estimates <- estimatesfiltered()
        
        print(estimates) # PRINT TABLE
    })
    
    
    
    

# FOREST PLOT FOR ALL INCLUDED STUDIES ------------------------------------
    output$foreststudies <- renderPlot({
        
        pp <- ppfiltered()
        pp$reference_studyno <- paste(pp$reference, ", Study ", pp$study_no, sep = "")
        
        
        # ## NEW (GGPLOT)
        # pp$reference_studyno <- as.factor(pp$reference_studyno)
        # 
        # pp$esg_upper <- pp$esg + qnorm(.975)*pp$se / sqrt(pp$nhighlow)
        # pp$esg_lower <- pp$esg - qnorm(.975)*pp$se / sqrt(pp$nhighlow)
        # 
        # plot <- ggplot(pp, aes(x = esg, y = reference_studyno)) + 
        #   geom_vline(xintercept = 0, col = "dark grey", lwd = 1) +
        #   geom_point() +
        #   geom_errorbar(aes(xmin = esg_lower, xmax = esg_upper), width = .5) +
        #   scale_y_discrete(limits = rev(levels(pp$reference_studyno))) + guides(color = "none") +
        #   theme_bw() + ylab("Reference") + xlab("Hedges's g")
        # plot
        
        
        rve <- robumeta::robu(esg ~ 1, data = pp, studynum = reference_studyno, var.eff.size = se, small = FALSE)

        robumeta::forest.robu(rve, es.lab = "dv", study.lab = "reference_studyno"
                              , "Effect size" = esg)
        
        ## OLD 2
        # pp$reference_study <- paste(pp$reference, ", Study ", pp$study_no, ", ", pp$dv, sep = "")
        # xxx change model later
        
        # 
        # metapp_metagen <- metagen(TE = esg,
        #                           seTE = se,
        #                           data = pp,
        #                           studlab = paste(reference_study),
        #                           comb.fixed = FALSE,
        #                           comb.random = TRUE,
        #                           method.tau = "ML", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
        #                           hakn = TRUE,
        #                           prediction = TRUE,
        #                           sm = "SMD") # standardized mean difference (Hedges's g)      
        # 
        # meta::forest(metapp_metagen
        #              , rightcols = c("g", "ci")
        #              # , sortvar = -TE
        #              , leftcols = c("studlab")
        #              , overall.hetstat = FALSE
        #              , prediction = FALSE
        #              , xlab = "Hedges's g"
        #              , smlab = "Hedges's g"
        #              , xlim = c(-2, 3), overall = FALSE)
        
        # forestdf <- data.frame("Effect" = metapp_metagen$studlab
        #                        , "g" = metapp_metagen$TE
        #                        , "UCL" = metapp_metagen$lower
        #                        , "LCL" = metapp_metagen$upper)
        # 
        # fp <- ggplot(forestdf, aes(x = g, y = Effect, xmin = LCL, xmax = UCL)) +
        #   geom_vline(xintercept = 0, linetype = 3) +
        #   geom_point() + geom_errorbar(aes(xmin = LCL, xmax = UCL, y = Effect, width = .03, size = .01)) +
        #   xlab("Hedges's g") +
        #   ylab("") +
        #   theme_classic()
        # # scale_x_continuous(limits = c(-2, 3)) +
        # 
        # ggplotly(fp, width = 900, height = 3700, tooltip = "all")
        # BUG: error bars disappear; this bug seems to be known https://github.com/ropensci/plotly/issues/1571
        
    }
    # ,  height = function() {
    #   session$clientData$foreststudies }
    , height = 7500, width = 900 # , height = 7500, width = 900
    )
    
    
    

# FUNNEL PLOT -------------------------------------------------------------

    output$funnel <- renderPlot({
        
        metapp_metagen_mean <- metapp_agg()
        
        funnelplot <- metafor::funnel(metapp_metagen_mean, xlab = "Hedges's g", studlab = FALSE, contour = .95, col.contour = "light grey")
        funnelplot
        
        
    })
    
    output$eggers <- renderTable({
      
      metapp_metagen_mean <- metapp_agg()
      
      # eggers <- eggers.test(x = metapp_metagen_mean)
      # names(eggers) <- c("Intercept",	"95% CI",	"t",	"p")
      eggers <- meta::metabias(metapp_metagen_mean, k.min = 3, method = "linreg")
      eggers_table <- data.frame("Intercept" = eggers$estimate, "Tau²" = eggers$tau
                                 , "t" = eggers$statistic
                                 ,"p" = ifelse(round(eggers$p.value, 3) == 0, "< .001", round(eggers$p.value, 3)))
      print(eggers_table[1, ])
    })


# PCURVE ------------------------------------------------------------------
    output$pcurve <- renderPlot({
        
        pp <- ppfiltered()
        
        pppcurve <- data.frame("TE" = pp$esg, "seTE" = pp$se, "studlab" = pp$reference, "n" = pp$nhighlow, "id" = pp$id2, "nhighlow" = pp$nhighlow) # CHANGED: use only one p-value per sample (p-values must be statistically independent)
        
        pppcurve_first <- pppcurve[!duplicated(pppcurve$id),]
        pcurve_estimates1 <- try(pcurve(pppcurve_first, effect.estimation = FALSE, N = pppcurve_first$nhighlow, dmin = 0, dmax = 1), silent = TRUE)
        
        pcurve_estimates1 <- ifelse(substr(pcurve_estimates1, 1, 5) == "Error", 0, pcurve_estimates1)
        
        pcurve_estimates1
    })    
    
    

# VIOLIN PLOTLY -------------------------------------------------------------
    output$violin <- renderPlotly({
      
      pp <- ppfiltered()
      pp <- pp[!is.na(pp$zg), ]
      
      gm <- mean(pp$g)
      gsd <- sd(pp$g)
      
      violinplot <- ggplot(data = pp, aes(x = 1, y = g)) + 
        xlab("") + ylab("Effect size") + geom_violin(fill = rgb(100/255, 180/255, 1, .5)) + theme_bw() +
        scale_y_continuous(name = "Hedges's g"
                           , sec.axis = sec_axis(~./gsd-gm, name = "Standardized g")) +
        # geom_jitter(shape = 16, position = position_jitter(0)) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        geom_boxplot(width = .25) +
        theme(text = element_text(size = 10))
      
      ggplotly(violinplot, tooltip = pp$reference_studyno) %>% 
        config(modeBarButtons = list(list("toImage")), displaylogo = FALSE)
    })    

    


# DOWNLOAD ----------------------------------------------------------------

    data_list <- reactive({
      list(
        dataset = ppfiltered()
        , summary = as.data.frame(estimatesfiltered())
        # , filters = c(input$dvtype1, input$dvtype2, input$dvtype3, input$prereg, input$year[1], input$year[2])
        , filters = as.data.frame(c(input$dvtype, input$prereg, input$poseposture, input$culture, input$coverstory, input$year))
      )
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function() {paste(Sys.Date(), '-PowerPosing-Metadata', version, '.xlsx', sep='')},
      content = function(file) {write_xlsx(data_list())}
    )
} 
    
    

    

# Run the application 
shinyApp(ui = ui, server = server)
