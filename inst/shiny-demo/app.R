library(shiny)
library(shinyhelper)
library(ggplot2)
library(mclust)
library(clustersimulation)

ui <- navbarPage(
    title = HTML(
        "Cluster analysis, power and type-1 error <p style='font-size:12px;font-weight:bold'>(app version 1.0)</p>"
    ),
    id = "pages",
    
    #----------------------------- PAGE 1: GENERAL INSTRUCTIONS
    
    tabPanel(
        title = "Instructions",
        br(),
        # a capo
        h1("Introduction to the topic", align = "left"),
        br(),
        p(
            "We suggest that, when performing any cluster analysis, it is fundamental to take into account two main
                  considerations: do we have enough power to find clusters if there are any? is there a high
                  chance of detecting multiple clusters even if they do not exist (i.e., running into the type I error)?
                  Often non-normal data distributions can lead to misleading results when using mixture models, and the same
                  may happen with correlated data when using distance-based clustering methods such as k-means.
                  This shiny app serves as a guide to help you find an answer to both questions in
                  an easy and functional way, especially if you are not familiar with the R code."
        ),
        br(),
        h4("Functionalities of the Shiny app", align = "left"),
        br(),
        p(
            "This app gives you two main possibilities: to specify some crucial parameters (N, correlations, skewness, kurtosis)
                  based on your own prior personal/bibliographical knowledge, or to set them using an already existing dataset that you can upload.
                  Then, in last panel, you will be able to calculate the power and/or type I error, based on the
    parameters given as input, the sample size, and for the power an estimated Cohen’s d.
                     Since we focus mostly on the risk of type I error, here we assume that correlations, skewness and kurtosis may depend
                  on characteristics of the data and measures, not on multiple clusters being there.
                  If you can really assume that correlations and normality violations in your data only reflect the existence of underlying clusters, this app may not be of interest for you."
        ),
    br(),
    h3(
        "Do you have your data or do you want to simulate from scratch?",
        align = "left"
    ),
    br(),
    p(
        "We can start now. If you already have data you can upload them.
                   Otherwise, you can simulate everything from scratch and add your
                   desired parameters.
                     "
    ),
    sidebarPanel(
        actionButton(inputId = "p1p2a", label = "I want to upload my data"),
        actionButton(inputId = "p1p2b", label = "I want to specify the parameters")
    )
    ),
    #----------------------------- END PAGE 1
    
    #----------------------------- PAGE 2A: DATA UPLOAD
    tabPanel(
        title = "Data Upload",
        br(),
        # a capo
        h1("Data upload", align = "left"),
        # titoletto di primo livello
        br(),
        sidebarPanel(
            h3("Instructions"),
            p(
                "If you want to upload your data, please make sure your data
                         follow these requirements. If it doesn't, the process might not start.
                         "
            ),
            tags$ul(
                tags$li("Format: .xlsx"),
                tags$li("First line variable names"),
                tags$li("Include only cluster indicators"),
                tags$li("Include only continuous/numeric variables"),
                tags$li("Use maximum 20 variables for computation limits"),
                tags$li("Be careful about missing values. Rows with at least 1 missing value will be excluded!")
            ),
            fileInput("yourfile", "Choose Excel File", accept = c(".xls", ".xlsx")),
            h4("Do you like your data?"),
            helper(
                actionButton(inputId = "p2ap3", label = "YES! Continue to simulation "),
                type = "inline",
                content = "If you click this button you can directly go to the simulation page and run the
                                           simulation using the same correlation matrix, skewness, and kurtosis of you variables.
                                           You will be able to change the sample size in the simulatino page, if you want",
                size = "m"
            ),
            helper(
                actionButton(inputId = "p2ap2b", label = "NO! I prefer different parameters"),
                type = "inline",
                content = "If you click this button, the parameters of your data WILL BE DELETED and you won't
                                           be able to use them for the simulation. You will be directed to a new page where you
                                           can specify the parameters you prefer",
                size = "m"
            )
        ),
        mainPanel(
            h3("Your data look like this"),
            tableOutput("yourtable"),
            plotOutput("yourplot")
        )
    ),
    
    #----------------------------- END PAGE 2A
    
    
    #----------------------------- PAGE 2B: DATA SPECIFICATION
    
    tabPanel(
        title = "Data Specification",
        br(),
        # a capo
        h1("Instructions for the parameter specification ", align =
               "left"),
        # titoletto di primo livello
        br(),
        p(
            "To simulate your data we need different information that include
                   your desired sample size, the hypothesized correlations between your
                   study variables, the hypothesized skewness and kurtosis of your variables,
                   and the number of indicators you want to simulate. For correlations, skewness,
                   and kurtosis, we ask you to provide a range of plausible parameters. We will
                   randomly sample parameters within the specified ranges. The output you will see
                   is just one of the infinite combinations of parameters that will be sampled.
                     "
        ),
        sidebarPanel(
            p(
                "The app will sample 1 random value within the range limits. If you don't want to sample from a range and prefer a single parameter, overlap the minimum and maximum values."
            ),
            sliderInput(
                "indicators",
                "Indicators/number of variables",
                min = 1,
                max = 12,
                value = 3,
                step = 1
            ),
            helper(
                sliderInput(
                    "correlations",
                    "Pearson r:",
                    min = 0.0,
                    max = .90,
                    value = c(.1, .3),
                    step = 0.05
                ),
                type = "inline",
                content = "For each bivariate correlation we will sample a value of r from a uniform
                                distribution of values within the specified limits.
                                If the limits overlap, the same value of r will be given to each correlation",
                size = "m"
            ),
            helper(
                sliderInput(
                    "skewness",
                    "Skewness:",
                    min = -.90,
                    max = .90,
                    value = c(.0, .5),
                    step = 0.05
                ),
                type = "inline",
                content = "We will assign skewness to each variable by sampling from a uniform distribution of values
                                within the specified limits.
                                If the limits overlap, all variables will have approximately the same skewness",
                size = "m"
            ),
            helper(
                sliderInput(
                    "kurtosis",
                    "Kurtosis:",
                    min = -.90,
                    max = .90,
                    value = c(.0, .3),
                    step = 0.05
                ),
                type = "inline",
                content = "We will assign kurtosis to each variable by sampling from a uniform distribution of values
                                within the specified limits.
                                If the limits overlap, all variables will have approximately the same kurtosis",
                size = "m"
            ),
            helper(
                actionButton("generateData", "1-Generate Data"),
                type = "inline",
                content = "Click this button to generate your data and store the parameters for the simulation",
                size = "m"
            ),
            helper(
                actionButton(inputId = "p2bp3", label = "2-Go to simulation"),
                type = "inline",
                content = "Click this button if you want to use these parameters for the simulation.
                                If you go back and upload your data, the parameters you entered will be ignored in the simulation",
                size = "m"
            )
        ),
        mainPanel(
            htmlOutput("sigmacheck"),
            tags$head(
                tags$style("#sigmacheck{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }")
            ),
            h3(
                "This is how data sampled from these parameters may look like (N = 10,000)"
            ),
            p(
                "During the simulation all parameters will be sampled, hence at each iteration they will differ
                    from those that will appear here."
            ),
            p(
                "If you prefer (almost) exact parameters, specify single values and not ranges (not suggested)."
            ),
            tableOutput("yourtable2"),
            plotOutput("yourplot2")
        )
    ),
    
    #----------------------------- END PAGE 2B
    
    
    #----------------------------- PAGE 3: DATA SIMULATION
    
    tabPanel(
        title = "Simulation",
        br(),
        # a capo
        h1("It's time to simulate!", align = "left"),
        # titoletto di primo livello
        br(),
        p(
            "Based on the parameters you entered or the parameters we obtained from your data,
            we will now simulate similar data under H0 (i.e., there is only one population/cluster)
            to detect Type-1 risk, or the risk to find clusters when they do not exist,
            and under H1 (i.e., there are two cluster in the population)
            if you also want to know what is your power to detect two clusters
            when your data have the specified parameters and the clusters really exist.
                     "
        ),
        p(
            "If you select 'Type1', we will only simulate data under H0 and Cohen's ds will be set to 0."
        ),
        p(
            "If you select 'Power', we will also simulate data under H1 and Cohen's ds will be sampled from the
            preferred range. In this case we will count how many times the cluster analys exactly detects 2 clusters"
        ),
        sidebarPanel(
            helper(
                selectInput(
                    "option_choice",
                    "Select an Option:",
                    choices = c("Mclust", "Kmeans")
                ),
                type = "inline",
                content = "If you select the kmeans option we will run the simulation using the stats::kmeans() function.
                                If you select the mclust, we will use mclust::Mclust() function",
                size = "m"
            ),
            selectInput(
                "simType",
                "Select type of analysis:",
                choices = c("Type1", "Power")
            ),
            sliderInput(
                "samplesize",
                "Sample size (total N):",
                100,
                min = 20,
                max = 2000,
                step = 20
            ),
            helper(
                sliderInput(
                    "cohensd",
                    "Cohen's d (meaningful only when computing Power):",
                    min = 0.0,
                    max = 1.5,
                    value = c(.1, .3),
                    step = 0.05
                ),
                type = "inline",
                content = "If you run a power analysis, the two clusters will differ of d in each variable.
                                d will be sampled from a uniform distribution within the specified limits.
                                If the limits overlap, the groups will equally differ in every variable",
                size = "m"
            ),
            p(
                "Cohen's d will be automatically set to 0 for Type-1 error simulations"
            ),
            #sliderInput("niter","# of iterations:",min=10,max=1000,value=150,step=10),
            helper(
                sliderInput(
                    "maxtime",
                    "Maximum computation time (in seconds):",
                    min = 10,
                    max = 300,
                    value = 10,
                    step = 10
                ),
                type = "inline",
                content = "If the number of indicators and the number of participants are high, the simulation will
                                take a long time (possibly several hours in some instances) to run enough replications.",
                size = "m"
            ),
            p("We will simulate data until time expires"),
            actionButton("startPower", "Start simulation"),
            # l'input è un pulsante che avvia una funzione (che verrà avviata in "reazione" al pulsante nella parte server)
        ),
        mainPanel(
            h3("Results of the simulation"),
            h6(
                "ATTENTION: if you have many indicators and a large sample size, the simulation might be slow
                   and the system crash for computation limits. Please download the R code and run your own simulation."
            ),
            tableOutput("powertable"),
            h6(
                "-----------------------------------------------------------"
            ),
            htmlOutput("power"),
            h6(
                "-----------------------------------------------------------"
            ),
            htmlOutput("power2"),
            br(),
            plotOutput("powerplot", width = "50%")
        )
    )
    
    
    #----------------------------- END PAGE 3
    
)

#------------------------------------
#----------------------------- END UI
#------------------------------------

server <- function(input, output, session) {
    options(digits = 2)
    observe_helpers()
    
    #----------------------------- PAGE 1
    
    observeEvent(input$p1p2a, {
        updateNavbarPage(session = session,
                         inputId = "pages",
                         selected = "Data Upload")
    })
    observeEvent(input$p1p2b, {
        updateNavbarPage(session = session,
                         inputId = "pages",
                         selected = "Data Specification")
    })
    
    #----------------------------- PAGE 2A
    
    df1 <- reactive({
        req(input$yourfile)
        as.data.frame(readxl::read_excel(input$yourfile$datapath))
    })
    udata <- reactiveVal(NULL)
    observe({
        req(df1())
        udata(clustersimulation:::.prep_user_data_shiny(df1(), type = "df1"))
    })
    output$yourtable <- function() {
        req(udata())
        cormat <- data.frame(udata()$CorT)
        sumstat <- data.frame(t(udata()$sums))
        tt1 <- cbind(cormat, sumstat)
        rownames(tt1) <- names(udata()$mus)
        tt1$`missing (%)` <- udata()$sumNA
        clustersimulation:::shiny_table(tt1, c("min", "max", "q25", "q50", "q75"))
    }
    output$yourplot <- renderPlot({
        req(df1())
        gg <- clustersimulation:::plot_summary(df1())
        return(gg)
    })
    
    
    #----------------------------- Move to page 2B or 3
    
    
    observeEvent(input$p2ap3, {
        updateNavbarPage(session = session,
                         inputId = "pages",
                         selected = "Simulation")
    })
    observeEvent(input$p2ap2b, {
        updateNavbarPage(session = session,
                         inputId = "pages",
                         selected = "Data Specification")
    })
    
    #----------------------------- PAGE 2B
    
    output$sigmacheck <- renderText({
        N <- 10000
        S <-
            gen_sigma(input$indicators,
                      input$correlations[1],
                      input$correlations[2])
        skewness <-
            runif(input$indicators, input$skewness[1], input$skewness[2])
        kurtosis <-
            runif(input$indicators, input$kurtosis[1], input$kurtosis[2])
        perror <-
            round(clustersimulation:::.check_sigma(
                Sigma = S,
                skew = skewness,
                kurt = kurtosis
            ),
            2)
        perror <-
            ifelse(
                perror > .50,
                paste(
                    "<b>These parameters generate a non-definite matrix. Please change them, or the app may crash."
                ),
                ""
            )
        return(perror)
    })
    df2 <- eventReactive(input$generateData, {
        N <- 10000
        correlations <-
            runif(1, input$correlations[1], input$correlations[2])
        indicators <- input$indicators
        S <-
            gen_sigma(indicators, input$correlations[1], input$correlations[2])
        skewness <-
            runif(indicators, input$skewness[1], input$skewness[2])
        kurtosis <-
            runif(indicators, input$kurtosis[1], input$kurtosis[2])
        df2 <-
            sim_data(
                n = N,
                d = 0,
                Sigma = S,
                skew = skewness,
                kurt = kurtosis
            )
        # TODO qui mettere che si estrae udata e quindi i parametri che vengono visti a schermo
    })
    observe({
        req(df2())
        udata(clustersimulation:::.prep_user_data_shiny(df2(), type = "df2"))
    })
    # sim_params <- reactive({
    #     list(rmin = input$correlations[1],
    #          rmax = input$correlations[2],
    #          nind = input$indicators,
    #          skewmin = input$skewness[1],
    #          skewmax = input$skewness[2],
    #          kurtmin = input$kurtosis[1],
    #          kurtmax = input$kurtosis[2])
    # })
    # simulate and plot data
    output$yourtable2 <- function() {
        cdata <- clustersimulation:::.prep_user_data_shiny(df2(), type = "df2")
        cormat <- data.frame(cdata$CorT)
        sumstat <-
            data.frame(Skewness = cdata$skews, Kurtosi = cdata$kurts)
        tt2 <- cbind(cormat, sumstat)
        rownames(tt2) <- names(cdata$mus)
        clustersimulation:::shiny_table(tt2)
    }
    
    output$yourplot2 <- renderPlot({
        gg2 <- clustersimulation:::plot_summary(df2())
        # dfx = data.frame(df2())
        # print(head(dfx))
        # gg2 = corrplot(cor(dfx))
        return(gg2)
    })
    
    #----------------------------- Move from page 2B to 3
    
    observeEvent(input$p2bp3, {
        updateNavbarPage(session = session,
                         inputId = "pages",
                         selected = "Simulation")
    })
    
    #----------------------------- PAGE 3
    
    # Change N depending on udata()
    observe({
        if (!is.null(udata())) {
            updateNumericInput(session,
                               "samplesize",
                               "N = your sample size",
                               value = udata()$n)
        } else {
            updateNumericInput(session, "samplesize", "N", value = 50) # Set your default value here
        }
    })
    observeEvent(input$samplesize, {
        if (!is.null(udata())) {
            current_udata <- udata()  # Get the current value of udata
            current_udata$n <- input$samplesize
            udata(current_udata)
        }
    })
    simRes <- eventReactive(input$startPower, {
        # -- INIT THE PROGRESS BAR
        # https://shiny.posit.co/r/articles/build/progress/
        
        # progress object
        
        progress <- shiny::Progress$new()
        progress$set(
            message = paste0("Computing data to estimate ", input$simType),
            value = 0
        )
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        
        updateProgress <-
            function(value = NULL,
                     detail = NULL) {
                if (is.null(value)) {
                    value <- progress$getValue()
                    value <-
                        value + (progress$getMax() - value) / 5
                }
                progress$set(value = value, detail = detail)
            }
        
        # -- END PROGRESS BAR
        
        # -- SIMULATION FUNCTION
        
        # TODO  # TODO Da tommi: ho aggiunto rand nel testo. Non so se sia corretto
        
        niter <- 10000#input$niter
        maxt <- input$maxtime
        method <-
            ifelse(tolower(input$option_choice) == "mclust",
                   "mclust",
                   "kmeans")
        type <-
            ifelse(input$simType == "Type1", "type1", "power")
        nclust <- ifelse(type == "type1", 1, 2)
        n <- input$samplesize
        dmin <- input$cohensd[1]
        dmax <- input$cohensd[2]
        #Run the simulation
        run_simulation(
            niter = niter,
            maxt = maxt,
            type = type,
            method = method,
            progress = FALSE,
            updateProgress = updateProgress,
            udata = udata(),
            nclust = nclust,
            n = n,
            dmin = dmin,
            dmax = dmax
            # rmin = sim_params()$rmin,
            # rmax = sim_params()$rmax,
            # nind = sim_params()$nind,
            # skewmin = sim_params()$skewmin,
            # skewmax = sim_params()$skewmax,
            # kurtmin = sim_params()$kurtmin,
            # kurtmax = sim_params()$kurtmax
        )
    })
    simRes2 <- eventReactive(input$startPower, {
        # -- INIT THE PROGRESS BAR
        # https://shiny.posit.co/r/articles/build/progress/
        
        # progress object
        
        progress2 <- shiny::Progress$new()
        progress2$set(
            message = paste0("Computing data to estimate Type1 error"),
            value = 0
        )
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress2$close())
        
        updateProgress <-
            function(value = NULL,
                     detail = NULL) {
                if (is.null(value)) {
                    value <- progress2$getValue()
                    value <-
                        value + (progress2$getMax() - value) / 5
                }
                progress2$set(value = value, detail = detail)
            }
        
        # -- END PROGRESS BAR
        
        # -- SIMULATION FUNCTION
        
        niter <- 10000#input$niter
        maxt <- input$maxtime
        method <-
            ifelse(tolower(input$option_choice) == "mclust",
                   "mclust",
                   "kmeans")
        type <-
            ifelse(input$simType == "Type1", "type1", "power")
        nclust <- ifelse(type == "type1", 1, 2)
        n <- input$samplesize
        dmin <- input$cohensd[1]
        dmax <- input$cohensd[2]
        #Run the second simulation
        
        if (type == "power") {
            run_simulation(
                niter = niter,
                maxt = maxt,
                type = "type1",
                method = method,
                progress = FALSE,
                updateProgress = updateProgress,
                udata = udata(),
                nclust = nclust,
                n = n,
                dmin = dmin,
                dmax = dmax
                # rmin = sim_params()$rmin,
                # rmax = sim_params()$rmax,
                # nind = sim_params()$nind,
                # skewmin = sim_params()$skewmin,
                # skewmax = sim_params()$skewmax,
                # kurtmin = sim_params()$kurtmin,
                # kurtmax = sim_params()$kurtmax
            )
        }
    })
    output$powertable <- renderTable({
        req(simRes())
        method <- simRes()$method
        type <- simRes()$type
        pt <- data.frame(
            Method = method,
            Iterations = ifelse(
                type == "type1",
                simRes()$niter,
                paste0(simRes()$niter, "/", simRes2()$niter)
            ),
            Indicators = simRes()$params$nind,
            SampleSize = simRes()$params$n,
            Correlations = paste0(
                "[",
                round(simRes()$params$rmin, 2),
                "; ",
                round(simRes()$params$rmax, 2),
                "]"
            ),
            Skewness = paste0(
                "[",
                round(simRes()$params$skewmin, 2),
                "; ",
                round(simRes()$params$skewmax, 2),
                "]"
            ),
            Kurtosi = paste0(
                "[",
                round(simRes()$params$kurtmin, 2),
                "; ",
                round(simRes()$params$kurtmax, 2),
                "]"
            ),
            power = ifelse(type == "power", round(simRes()$power, 2), "--"),
            T1error = ifelse(
                type == "type1",
                round(simRes()$type1, 2),
                ifelse(type == "power", round(simRes2()$type1, 2), "--")
            )
        )
        return(pt)
    }, caption = "Results with fewer than 1,000 iterations may not be stable.")
    output$power <- renderText({
        req(simRes())
        simResText <- sprintf(
            "
                  During the desired time we managed to simulate %s random datasets with %s indicators and %s observations.
                  Correlations between variables ranged between %.2f and %.2f.
                  Skewness ranged between %.2f and %.2f.
                  Kurtosis ranged between %.2f and %.2f.
                  We applied the cluster analysis with the %s method to each dataset and calculated the probability of incurring in an error.
                    ",
            simRes()$niter,
            simRes()$params$nind,
            simRes()$params$n,
            simRes()$params$rmin,
            simRes()$params$rmax,
            simRes()$params$skewmin,
            simRes()$params$skewmax,
            simRes()$params$kurtmin,
            simRes()$params$kurtmax,
            simRes()$method
        )
        simResText <- ifelse(
            simRes()$type == "type1",
            sprintf(
                "TYPE I ERROR — %s Your estimated probability of type-1 errors is %.2f",
                simResText,
                round(simRes()$type1, 2)
            ),
            sprintf(
                "POWER ANALYSIS — %s
                                             Effect size/cluster separation (Cohen's d)
                                             range between %.2f and %.2f across indicators.
                                             Your estimated power to detect two clusters is %.2f
                                             with an Adjusted Rand index of %.2f",
                simResText,
                round(simRes()$params$dmin, 2),
                round(simRes()$params$dmax, 2),
                round(simRes()$power, 2),
                simRes()$m_rand_index
            )
        )
        return(simResText)
    })
    output$power2 <- renderText({
        req(simRes2())
        simRes2Text <- sprintf(
            "
                  During the desired time we managed to simulate %s random datasets with %s indicators and %s observations.
                  Correlations between variables ranged between %.2f and %.2f.
                  Skewness ranged between %.2f and %.2f.
                  Kurtosis ranged between %.2f and %.2f.
                  We applied the cluster analysis with the %s method to each dataset and calculated the probability of incurring in an error.
                    ",
            simRes2()$niter,
            simRes2()$params$nind,
            simRes2()$params$n,
            simRes2()$params$rmin,
            simRes2()$params$rmax,
            simRes2()$params$skewmin,
            simRes2()$params$skewmax,
            simRes2()$params$kurtmin,
            simRes2()$params$kurtmax,
            simRes2()$method
        )
        simRes2Text <-
            sprintf(
                "TYPE I ERROR — %s Your estimated probability of type-1 errors is %.2f",
                simRes2Text,
                round(simRes2()$type1, 2)
            )
        return(simRes2Text)
    })
    output$powerplot <- renderPlot({
        req(simRes())
        # Simulate a vector with different letters
        Ncluster <-
            as.factor(simRes()$nc[simRes()$nc != 0])
        NCdata <- data.frame(ClusterN = Ncluster)
        Cluster_counts <- table(NCdata$ClusterN)
        count_data <-
            data.frame(ClusterN = names(Cluster_counts),
                       count = as.numeric(Cluster_counts))
        powerplot2 <-
            ggplot2::ggplot(count_data, aes(
                x = count,
                y = factor(1),
                fill = ClusterN
            )) +
            ggplot2::geom_bar(stat = "identity",
                              position = "stack",
                              width = 0.5) +  # Adjust width as needed
            ggplot2::labs(title = "Frequency of clusters individuated",
                          x = "Frequency",
                          y = NULL) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
                plot.background = ggplot2::element_rect(fill = "white"),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                axis.text = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(),
                legend.position = "bottom",
                legend.title = ggplot2::element_blank()
            )
        return(powerplot2)
        
    }, height = 200, width = 500)
    
}


#----------------------------- RUN THE SHINY
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))