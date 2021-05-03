library(shiny)
library(RSQLite)
library(savR)
library(tryCatchLog)
library(gridExtra)
library(ggiraph)
library(shinycssloaders)
library("reshape2")
library("plotly") # wann require?
require("knitr")

opts_knit$set(root.dir = "~/path/to/folder/")
options(spinner.color = "#5499C7", size = 0.5)

# List all tables of database
get_table_name <- function(database, index) {
    table_name <- dbListTables(database)[index]
    return(table_name)
}

# Get one table of database by table index
get_table <- function(database, index) {
    db_tables <- dbListTables(database)
    table <- dbReadTable(database, db_tables[index])
    return(table)
}

# Get list of savR objects by list of run names
get_savR_objs <- function(run_names) {
    savR_objs <- c()
    for (i in run_names) {
        tryCatch({
            savR_objs[[i]] <- savR(i)
        },
        error = function(e) {
            #add message for user if needed
            TRUE
        }) 
    }
    return(savR_objs)
}

# Example data
example_data <-
    data.frame(
        date = c(
            "2020-02-04",
            "2020-03-25",
            "2020-03-27",
            "2020-09-01",
            "2020-10-23"
        ),
        name = c(
            "20200204_FS10000749_6_BPA73104-0927",
            "20200325_FS10000749_12_BPL20316-1811",
            "200327_M02885_0317_000000000-J3F9B",
            "200901_M02885_0346_000000000-J8CWM",
            "201023_NB551623_0090_AHGH27AFX2"
        )
    )

ui <- fluidPage(
    navbarPage("Name of App",
        tabPanel(
            "Run Information",
            titlePanel("Run Information"),
            uiOutput("table_name"),
            tableOutput("run_data") %>% withSpinner(),
            h3("Example Data"),
            tableOutput("example_data"),
            verbatimTextOutput("example_savR") %>% withSpinner()
        ),
        tabPanel(
            "Corrected Intensitities",
            titlePanel("Corrected Intensities"),
            h4("Corrected intensities per run over date"),
            plotOutput("corr_intensities_per_run", click = "select_run_corr_int") %>% withSpinner(),
            uiOutput("title_intensities_over_cycle"),
            plotOutput("intensities_over_cycle", click = "select_cycle_corr_int") %>% withSpinner(hide.ui = FALSE),
            plotlyOutput("intensities_over_cycle_plotly") %>% withSpinner(hide.ui = FALSE),
            uiOutput("title_intensity_per_cycle_per_base"),
            plotOutput("intensity_per_cycle_per_base") %>% withSpinner(hide.ui = FALSE)
        ),
        tabPanel(
            "Quality Metrics",
            titlePanel("Quality Metrics"),
            h4("Averaged qualities per run over cycle"),
            plotOutput("quality_over_cycle", click = "select_run_quality") %>% withSpinner(),
            h4("Girafe plot - Averaged qualities per run over cycle"),
            girafeOutput("quality_over_cycle_girafe") %>% withSpinner(),
            uiOutput("title_quality_per_lane_per_tile"),
            plotOutput("quality_per_lane_per_tile") %>% withSpinner(hide.ui = FALSE)
        )
))

server <- function(input, output) {
    tryCatch({
        # Get data from database file
        db <- dbConnect(SQLite(), "mf2_database.db")
        # Get a table name by index
        table_name <<- get_table_name(db, 1)
        run_data <- get_table(db, 1)
    },
    error = function(e) {
        TRUE
    }) #message for user
    
    # Example data
    data <- example_data
    savR_objs <- get_savR_objs(data$name)
    
    output$table_name <- renderUI({
        tagList(
            tags$h3("mf2_database.db"),
            tags$h4(table_name)
        )
    })
    
    output$run_data <- renderTable({
        run_data
    })
    
    output$example_data <- renderTable({
        example_data
    })
    
    # Example Data
    output$example_savR <- renderPrint({
        print(savR_objs)
    })
    
    # --- Corrected intensities ---
    
    # Combine avg_cor_A, avg_cor_C, avg_cor_G and avg_cor_T for one run
    combine_avg_corr_int <- function(corr_int) {
        return(c(corr_int[["avg_cor_A"]], corr_int[["avg_cor_C"]], corr_int[["avg_cor_G"]], corr_int[["avg_cor_T"]]))
    }
    
    # Return run user selected by clicking on violin plot
    get_run_selected_corr_int <- function(corr_intensities, x) {
        req(x)
        rows <- round(x) == as.numeric(as.factor(corr_intensities$dates))
        row <- data$date == corr_intensities[rows,]$dates[[1]]
        name_selected <<- data[row,]$name
        run <- savR_objs[[name_selected]]
        return(run)
    }
    
    # Calculate average value per cycle for ACGT
    calc_avg_base_value_per_cycle <- function(run) {
        corr_int <- correctedIntensities(run)
        cycles <- c(1:cycles(run))
        bases <- c() 
        cols <- c("avg_cor_A", "avg_cor_C", "avg_cor_G", "avg_cor_T")
        for (i in cycles) {
            rows <- corr_int$cycle == cycles[[i]]
            for (j in 1:4) {
                col <- cols[[j]]
                corr_int_per_base <- corr_int[rows,][[col]]
                avg_corr_int_per_base <- mean(corr_int_per_base)
                if (j > length(bases)) {
                    bases[[j]] <- avg_corr_int_per_base
                }
                else {
                    bases[[j]] <- c(bases[[j]], avg_corr_int_per_base)
                }
            }
        }
        return(data.frame(
            cycle = cycles,
            A = bases[[1]],
            C = bases[[2]],
            G = bases[[3]],
            T = bases[[4]]
        ))
    }
    
    output$corr_intensities_per_run <- renderPlot({
        combined_avg_corr_int <- c()
        dates <- c()
        for (i in 1:length(savR_objs)) {
            tryCatch({
                corr_int <- correctedIntensities(savR_objs[[i]])
                qualities <- combine_avg_corr_int(corr_int)
                combined_avg_corr_int <- c(combined_avg_corr_int, qualities)
                rows <- data$name == names(savR_objs[i])
                date <- data[rows,]$date
                dates <- c(dates, rep(date, length(qualities)))
            },
            error = function(e) {
                TRUE
            }) #message for user
        }
        
        avg_corr_intensities <<- data.frame(dates = dates, intensities = combined_avg_corr_int)
        ggplot(avg_corr_intensities, 
               aes(
                   x = dates, 
                   y = intensities
               )) + geom_violin(trim = FALSE) + labs(x = "Date", y = "Intensity")
    })
    
    output$title_intensities_over_cycle <- renderUI ({
        req(input$select_run_corr_int$x)
        rows <- round(input$select_run_corr_int$x) == as.numeric(as.factor(avg_corr_intensities$dates))
        tags$h4(paste0("Corrected intensities over cycle for: ", avg_corr_intensities[rows,]$dates[[1]]))
    })
    
    output$intensities_over_cycle <- renderPlot({
        req(input$select_run_corr_int$x)
        run_selected <<- get_run_selected_corr_int(avg_corr_intensities, input$select_run_corr_int$x)
        base_int_over_cycle <<- calc_avg_base_value_per_cycle(run_selected)
        base_int_over_cycle <- melt(base_int_over_cycle, id = ("cycle"), variable.name = "base")
        
        ggplot(base_int_over_cycle,
            aes(
                x = cycle,
                y = value,
                group = base,
                color = base
            )) + geom_line() + labs(x = "Cycle", y = "Intensity")
    })
    
    output$intensities_over_cycle_plotly <- renderPlotly({
        req(input$select_run_corr_int$x)
        p <- plot_ly(
                base_int_over_cycle,
                x = ~ cycle,
                y = ~ A,
                name = "A",
                type = "scatter",
                mode = "lines",
                line = list(width = 1),
                source = "plotly"
            )
        p <- p %>% add_trace(
                y = ~ C,
                name = "C",
                mode = "lines",
                line = list(width = 1)
            ) %>% add_trace(
                y = ~ G,
                name = "G",
                mode = "lines",
                line = list(width = 1)
            ) %>% add_trace(
                y = ~ T,
                name = "T",
                mode = "lines",
                line = list(width = 1)
            )
        p <- p %>% layout(hovermode = "x unified", xaxis = list(title = "Cylce"), yaxis = list(title = "Intensity")) %>% event_register("plotly_click")
    })

    output$title_intensity_per_cycle_per_base <- renderUI({
        req(input$select_cycle_corr_int$x)
        cycle <- round(input$select_cycle_corr_int$x)
        
        # Use with plotly
        #req(event_data("plotly_click", source = "plotly")$x[[1]])
        #cycle <- event_data("plotly_click", source = "plotly")$x[[1]]
        
        tags$h4(paste0("Intensities for A, C, G and T for cycle: ", cycle))
    })
    
    output$intensity_per_cycle_per_base <- renderPlot({
        req(input$select_cycle_corr_int$x)
        cycle <- round(input$select_cycle_corr_int$x)
        
        # Use with plotly
        #req(event_data("plotly_click", source = "plotly")$x[[1]])
        #cycle <- event_data("plotly_click", source = "plotly")$x[[1]]
        
        if (cycle < length(base_int_over_cycle$cycle) & cycle > 0) {
            plot_A <- plotIntensity(run_selected, as.integer(cycle), "A")
            plot_C <- plotIntensity(run_selected, as.integer(cycle), "C")
            plot_G <- plotIntensity(run_selected, as.integer(cycle), "G")
            plot_T <- plotIntensity(run_selected, as.integer(cycle), "T")
            grid.arrange(plot_A, plot_C, plot_G, plot_T, ncol = 2)
        }
    })
    
    
    # --- Quality metrics ---
    
    # Calculate average quality over cycle for one single run
    calc_avg_quality_per_run <- function(run) {
        quality_metrics <- qualityMetrics(run)
        cycles <- c(1:cycles(run))
        index_Q1 <- grep("^Q1$", colnames(quality_metrics))
        avg_quality <- c()
        for (i in cycles) {
            rows <- quality_metrics$cycle == cycles[[i]]
            quality <- quality_metrics[rows, index_Q1:(index_Q1 + 49)]
            quality <- colSums(quality)
            
            sum <- 0
            num_reads <- sum(quality)
            
            for (j in 1:50) {
                sum <- sum + (quality[[j]] * j)
            }
            
            avg_quality <- c(avg_quality, sum / num_reads)
        }
        return(avg_quality)
    }
    
    # Calculate average quality over cycle for each run in savR list
    calc_avg_quality_over_cycle <- function() {
        avg_quality_over_cycle <- data.frame(cycle = c(1:618)) #for MiSeq
        for (i in 1:length(savR_objs)) {
            tryCatch({
                avg_quality_run <- calc_avg_quality_per_run(savR_objs[[i]])
                row <- data$name == names(savR_objs[i])
                date <- data[row,]$date
                avg_quality_over_cycle[date] <- avg_quality_run
            },
            error = function(e) {
                TRUE
            }) #message
        }
        avg_quality_over_cycle <- melt(avg_quality_over_cycle, id = ("cycle"), variable.name = "date")
        return(avg_quality_over_cycle)
    }
    
    # Return date user selected by clicking on line plot
    get_date_selected_quality <- function(input) {
        cycle <- round(input$x)
        quality <- round(input$y)
        
        rows <- avg_qualities$cycle == cycle
        qualities <- avg_qualities[rows,]
        col <- qualities$value
        pos <- which(abs(col - quality) == min(abs(col - quality)))
        
        if (abs(col[[pos]] - quality) <= 2) {
            date_selected <- qualities$date[[pos]]
            return(date_selected)
        }
    }
    
    # Return qualities per tile of one lane 
    get_quality_per_tile <- function(run, lane) {
        rows <- qualityMetrics(run)$lane == lane
        quality_metrics <- qualityMetrics(run)[rows,]
        num_tiles <- unique(quality_metrics$tile)
        
        index_Q1 <- grep("^Q1$", colnames(quality_metrics))
        tile <- c()
        qualities <- c()
        
        for (i in 1:length(num_tiles)) {
            rows <- quality_metrics$tile == num_tiles[[i]]
            quality <- quality_metrics[rows, index_Q1:(index_Q1 + 49)]
            quality <- colSums(quality)
            
            # divide by 10000 to reduce amount of data
            for (j in 1:50) {
                tile <- c(tile, rep(num_tiles[[i]], round(quality[[j]] / 10000)))
                qualities <- c(qualities, rep(j, round(quality[[j]] / 10000)))
            }
        }
        
        quality_per_tile <- data.frame(tile = tile, quality = qualities)
        return(quality_per_tile)
    }
    
    output$quality_over_cycle <- renderPlot({
        avg_qualities <<- calc_avg_quality_over_cycle()
        ggplot(avg_qualities,
               aes(
                   x = cycle,
                   y = value,
                   group = date,
                   color = date
               )) + geom_line() + labs(x = "Cycle", y = "Quality")
    })
    
    output$quality_over_cycle_girafe <- renderGirafe({
        p <- ggplot(avg_qualities,
                   aes(
                       x = cycle,
                       y = value,
                       group = date,
                       color = date
                   )) + geom_line_interactive(aes(tooltip = date, data_id = date)) + labs(x = "Cycle", y = "Quality")
        girafe(
            ggobj = p,
            width_svg = 24,
            height_svg = 8,
            options = list(
                opts_selection(type = "single", css = "stroke-width:2;"),
                opts_hover_inv(css = "opacity:0.1;"),
                opts_hover(css = "stroke-width:2;")
            )
        )
    })
    
    output$title_quality_per_lane_per_tile <- renderUI({
        req(input$select_run_quality)
        date_selected <- get_date_selected_quality(input$select_run_quality)
        tags$h4(paste0("Qualities per lane over tile for: ", date_selected))
        
        # Use with girafe
        #req(input$quality_over_cycle_girafe_selected)
        #tags$h4(paste0("Qualties per lane over tile for: ", input$quality_over_cycle_girafe_selected))
    })
    
    output$quality_per_lane_per_tile <- renderPlot({
        req(input$select_run_quality)
        date_selected <- get_date_selected_quality(input$select_run_quality)
        rows <- data$date == date_selected
        run_selected <- data[rows,]$name
        run <- savR_objs[[run_selected]]
        num_lanes <- unique(qualityMetrics(run)$lane)
        if (num_lanes < 2) {
            quality_per_tile <- get_quality_per_tile(run, num_lanes)
            ggplot(quality_per_tile,
                   aes(
                       x = as.factor(tile),
                       y = quality
                   )) + geom_violin(trim = FALSE) + labs(x = "Tile", y = "Quality")
        }

        # Use with girafe
        #req(input$quality_over_cycle_girafe_selected)
        #date_selected <- input$quality_over_cycle_girafe_selected
        #rows <- data$date == date_selected
        #run_selected <- data[rows,]$name
        #run <- savR_objs[[run_selected]]
        #num_lanes <- unique(qualityMetrics(run)$lane)
        #if (num_lanes < 2) {
        #    quality_per_tile <- get_quality_per_tile(run, num_lanes)
        #    ggplot(quality_per_tile,
        #           aes(
        #              x = as.factor(tile), 
        #              y = quality
        #           )) + geom_violin(trim = FALSE) + labs(x = "Tile", y = "Quality")
        #}
    })
}

# Run the application
shinyApp(ui = ui, server = server)
