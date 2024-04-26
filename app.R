library(shiny)
library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(png)
library(markdown)
library(stringr)
library(ggpubr)
library(ggrepel)
library(DT)
library(quarto)

source("includes.R")
source("tab1.R", local = TRUE)
source("tab2.R", local = TRUE)
source("tab3.R", local = TRUE)

##############################################################################
## Setup UI ##################################################################

ui <- fluidPage(
  titlePanel("Craniofacial Growth"),
  
  tabsetPanel(
    
    # About tab
    tabPanel("About",
             includeMarkdown("about.md")
    ), # End tab panel
    
    # Single measurement, single time point
    tab1, # End tab panel
    
    # Multiple traits, single time point
    tab2, # End tab panel
    
    # Multiple traits, single time point
    tab3, # End tab panel
    
    # Citations and funding
    tabPanel("Citations and Funding",
             includeMarkdown("citations.md")
    ) # End tab panel
    
  ) # End tabsetPanel
) # End fluidPage

##############################################################################
## Define server logic #######################################################

server <- function(input, output, session) {
  
  #############################################################################
  # Single measurement analysis ###############################################
  
  # Creates qs() reactive function with data for percentiles
  qs <- eventReactive(input$update, {
    # Load percentiles dataset
    fname <- file.path(
      "Trait_Percentiles",
      paste0(if_else(input$sex == "Female", "Female_", "Male_"),
             trait_to_abbrev(input$measurement),
             "_percentiles.Rds"))
    readRDS(fname) |> 
      select(-years, -months) # Drop these columns so t() later
    # doesn't cause a problem
  })
  
  # Creates reactive age() with age as decimal
  age <- eventReactive(input$update, {
    yrmo_to_years(input$age_yrs, input$age_months)
    })
  
  int_measure <- eventReactive(input$update, {
    as.numeric(input$measure)
    })
  
  pick_measurement <- eventReactive(input$update, {
    input$measurement
    })
  
  select_sex <- eventReactive(input$update, {
    input$sex
  })
  
  # Percentile plot ##########################################################
  
    # Create percentiles plot using qs() and age()
    output$plot <- renderPlot({
      
      q <- qs() |> 
        slice(seq(1, nrow(qs()), by = 10)) |> 
        dplyr::select(age, all_of(percentiles_to_plot))
      
      trait_string <- paste0(select_sex(), ": ", pick_measurement())
      
      obs <- tibble(age = age(),
                    measure = int_measure())
      
      # Find percentile
      measure <- int_measure()
      age_rnd <- round(age(), 2)
      q_obs <- qs() |> 
        filter(age == age_rnd) |> 
        select(-age) |> 
        t()
      percentile <- which.min(abs(q_obs - measure))
      
      # Plot
      p <- plot_percentile(q = q, trait_string = trait_string) +
        geom_point(data = obs, aes(x = age, y = measure),
                   color = red_color,
                   size = 3) +
        labs(subtitle = paste0("Percentile: ", percentile))
      
      if (percentile == 1 | percentile == 99) {
        p <- p +
          annotate("text", -Inf, Inf,
                   label = "Note: Extreme value for measurement",
                   hjust = 0, vjust = 1,
                   color = red_color,
                   size = 8)
      }
      p
    }, res = 125)
  
    
    # Plot head schematic with line for measurement #############################
    output$traitfig <- renderPlot({
      
      plot_schematic(pick_measurement())
      
    }, res = 125)
  
  # Download pdf containing the plot
  output$download1 <- downloadHandler(
    filename = "SMreport.pdf",
    content = function(file)
      {
      rmarkdown::render(
        input = "singlemeasure.rmd",
        output_format = "pdf_document",
        params = list(
          sex = input$sex,
          measurement = input$measurement,
          measure = input$measure,
          age = yrmo_to_years(input$age_yrs, input$age_months),
          trait_string = paste0(input$sex, ": ", input$measurement)
        )
      )

      file.copy("singlemeasure.pdf", file)
      }
  )
  
  #############################################################################
  # Multiple measurement analysis #############################################
  
  # Reactive elements:
  #   - age_multi()
  #   - qs_multi_1()
  #   - qs_multi_2()
  #   - qs_multi_3()
  
  age_multi <- eventReactive(input$update1, {
    (yrmo_to_years(input$age_yrs_multi,input$age_months_multi))
    })
  
  qs_multi_1 <- eventReactive(input$update1, {
    fname <- file.path(
      "Trait_Percentiles",
      paste0(if_else(input$sex_multi == "Female", "Female_", "Male_"),
             trait_to_abbrev(input$trait_multi_1),
             "_percentiles.Rds"))
    readRDS(fname) |> 
      select(-years, -months)
  })
    
  qs_multi_2 <- eventReactive(input$update1, {
    fname <- file.path(
      "Trait_Percentiles",
      paste0(if_else(input$sex_multi == "Female", "Female_", "Male_"),
             trait_to_abbrev(input$trait_multi_2),
             "_percentiles.Rds"))
    readRDS(fname) |> 
      select(-years, -months)
  })
    
  qs_multi_3 <- eventReactive(input$update1, {
    fname <- file.path(
      "Trait_Percentiles",
      paste0(if_else(input$sex_multi == "Female", "Female_", "Male_"),
             trait_to_abbrev(input$trait_multi_3),
             "_percentiles.Rds"))
    readRDS(fname) |> 
      select(-years, -months)
  })
  
  int_measure_1 <- eventReactive(input$update1, {
    input$measure_multi_1
  })
  
  int_measure_2 <- eventReactive(input$update1, {
    input$measure_multi_2
  })
  
  int_measure_3 <- eventReactive(input$update1, {
    input$measure_multi_3
  })
  
  choose_to_measure_1 <- eventReactive(input$update1, {
    input$trait_multi_1
  })
  
  choose_to_measure_2 <- eventReactive(input$update1, {
    input$trait_multi_2
  })
  
  choose_to_measure_3 <- eventReactive(input$update1, {
    input$trait_multi_3
  })
  
  get_sex_multi <- eventReactive(input$update1, {
    input$sex_multi
  })
    
  # Generate output DT
  output$multi_trait_table <- renderDT({
    
    # Find percentiles
    measure_1 <- as.numeric(int_measure_1())
    age_1_rnd <- round(age_multi(), 2)
    q_obs_1 <- qs_multi_1() |>
      filter(age == age_1_rnd) |>
      select(-age) |>
      t()
    pct_1 <- which.min(abs(q_obs_1 - measure_1))
    
    measure_2 <- as.numeric(int_measure_2())
    age_2_rnd <- round(age_multi(), 2)
    q_obs_2 <- qs_multi_2() |>
      filter(age == age_2_rnd) |>
      select(-age) |>
      t()
    pct_2 <- which.min(abs(q_obs_2 - measure_2))
    
    measure_3 <- as.numeric(int_measure_3())
    age_3_rnd <- round(age_multi(), 2)
    q_obs_3 <- qs_multi_3() |>
      filter(age == age_3_rnd) |>
      select(-age) |>
      t()
    pct_3 <- which.min(abs(q_obs_3 - measure_3))
    
    
    out <- tibble(
      Sex = rep(get_sex_multi(), times = 3),
      `Age (yr)` = rep(age_multi(), times = 3) |>
        round(2),
      Measurement = c(choose_to_measure_1(),
                choose_to_measure_2(),
                choose_to_measure_3()),
      `Measurement (mm)` = c(int_measure_1(),
                             int_measure_2(),
                             int_measure_3()),
      Percentile = c(pct_1, pct_2, pct_3))
    
    datatable(out,
              filter = "none",
              rownames = FALSE,
              class = 'cell-border stripe',
              options = list(paging = FALSE,
                             searching = FALSE,
                             info = FALSE)) |>
      formatStyle('Percentile',  color = 'firebrick4',
                  fontWeight = 'bold')
  })
  
  #Download the pdf tracking multiple traits
  output$download2 <- downloadHandler(
    filename = "MMreport.pdf",
    content = function(file)
    {
      rmarkdown::render(
        input = "multiplemeasure.rmd",
        output_format = "pdf_document",
        params = list(
          sex = input$sex,
          measurement1 = input$trait_multi_1,
          measurement2 = input$trait_multi_2,
          measurement3 = input$trait_multi_3,
          measure1 = as.numeric(input$measure_multi_1),
          measure2 = as.numeric(input$measure_multi_2),
          measure3 = as.numeric(input$measure_multi_3),
          age =  yrmo_to_years(input$age_yrs_multi,input$age_months_multi)
        )
      )
      
      file.copy("multiplemeasure.pdf", file)
    }
  )
  
  
  ############################################################################
  # Multiple time point analysis #############################################
  
  # Reactive elements:
  #   - age_time_1()
  #   - age_time_2()
  #   - age_time_3()
  #   - qs_time()
  
  # Get decimal ages for 3 timepoints
  age_time_1 <- eventReactive(input$update2, {
    yrmo_to_years(input$age_yrs_time_1,
                  input$age_months_time_1)
  })
  
  age_time_2 <- eventReactive(input$update2, {
    yrmo_to_years(input$age_yrs_time_2,
                  input$age_months_time_2)
  })
  
  age_time_3 <- eventReactive(input$update2, {
    yrmo_to_years(input$age_yrs_time_3,
                  input$age_months_time_3)
  })
  
  # Retrieve quantiles
  qs_time <- eventReactive(input$update2, {
    fname <- file.path(
      "Trait_Percentiles",
      paste0(if_else(input$sex_time == "Female", "Female_", "Male_"),
             trait_to_abbrev(input$trait_time),
             "_percentiles.Rds"))
    readRDS(fname) |> 
      select(-years, -months)
  })
  
  measuretime_1 <- eventReactive(input$update2, {
    input$measure_time_1
  })
  
  measuretime_2 <- eventReactive(input$update2, {
    input$measure_time_2
  })
  
  measuretime_3 <- eventReactive(input$update2, {
    input$measure_time_3
  })
  
  get_sex_time <- eventReactive(input$update2, {
    input$sex_time
  })
  
  get_trait_time <- eventReactive(input$update2, {
    input$trait_time
  })
  
  # Generate output DT
  output$multi_time_table <- renderDT({
    
    # Find percentiles
    measure_1 <- as.numeric(measuretime_1())
    age_1_rnd_time <- round(age_time_1(), 2)
    q_obs_1_time <- qs_time() |>
      filter(age == age_1_rnd_time) |>
      select(-age) |>
      t()
    pct_1_time <- which.min(abs(q_obs_1_time - measure_1))
    
    measure_2 <- as.numeric(measuretime_2())
    age_2_rnd_time <- round(age_time_2(), 2)
    q_obs_2_time <- qs_time() |>
      filter(age == age_2_rnd_time) |>
      select(-age) |>
      t()
    pct_2_time <- which.min(abs(q_obs_2_time - measure_2))
    
    measure_3 <- as.numeric(measuretime_3())
    age_3_rnd_time <- round(age_time_3(), 2)
    q_obs_3_time <- qs_time() |>
      filter(age == age_3_rnd_time) |>
      select(-age) |>
      t()
    pct_3_time <- which.min(abs(q_obs_3_time - measure_3))
    
    
    out_time <- tibble(
      Sex = rep(get_sex_time(), times = 3),
      Measurement = rep(get_trait_time(), times = 3),
      `Age (yr)` =
        c(age_time_1(),
          age_time_2(),
          age_time_3()) |>
        round(2),
      `Measurement (mm)` = c(measuretime_1(),
                             measuretime_2(),
                             measuretime_3()),
      Percentile = c(pct_1_time, pct_2_time, pct_3_time))
    
    datatable(out_time,
              filter = "none",
              rownames = FALSE,
              class = 'cell-border stripe',
              options = list(paging = FALSE,
                             searching = FALSE,
                             info = FALSE)) |>
      formatStyle('Percentile',  color = 'firebrick4',
                  fontWeight = 'bold')
  })
  
  # Generate multi-timepoint plot
  output$multi_time_plot <- renderPlot({
    
    q_multi_time_to_plot <- qs_time() |> 
      slice(seq(1, nrow(qs_time()), by = 10)) |> 
      dplyr::select(age, all_of(percentiles_to_plot))
    
    # Find percentiles
    measure_1 <- as.numeric(measuretime_1())
    age_1_rnd_time <- round(age_time_1(), 2)
    q_obs_1_time <- qs_time() |>
      filter(age == age_1_rnd_time) |>
      select(-age) |>
      t()
    pct_1_time <- which.min(abs(q_obs_1_time - measure_1))
    
    measure_2 <- as.numeric(measuretime_2())
    age_2_rnd_time <- round(age_time_2(), 2)
    q_obs_2_time <- qs_time() |>
      filter(age == age_2_rnd_time) |>
      select(-age) |>
      t()
    pct_2_time <- which.min(abs(q_obs_2_time - measure_2))
    
    measure_3 <- as.numeric(measuretime_3())
    age_3_rnd_time <- round(age_time_3(), 2)
    q_obs_3_time <- qs_time() |>
      filter(age == age_3_rnd_time) |>
      select(-age) |>
      t()
    pct_3_time <- which.min(abs(q_obs_3_time - measure_3))
    
    # Format data for plotting
    pcts_time <- tibble(
      Age = c(age_time_1(),
              age_time_2(),
              age_time_3()),
      Percentile = c(measure_1, measure_2, measure_3)
    )
    
    plot_percentile(q = q_multi_time_to_plot,
                    trait_string = get_trait_time()) +
      geom_line(data = pcts_time,
                aes(x = Age, y = Percentile),
                color = red_color, linewidth = 1) +
      geom_point(data = pcts_time,
                 aes(x = Age, y = Percentile),
                 color = red_color, size = 2.5)
  }, res = 125)

#Download the report with multiple time points
output$download3 <- downloadHandler(
  filename = "MTreport.pdf",
  content = function(file)
  {
    rmarkdown::render(
      input = "multipletime.rmd",
      output_format = "pdf_document",
      params = list(
        sex = input$sex_time,
        age1 = yrmo_to_years(input$age_yrs_time_1,
                             input$age_months_time_1),
        age2 = yrmo_to_years(input$age_yrs_time_2,
                             input$age_months_time_2),
        age3 = yrmo_to_years(input$age_yrs_time_3,
                             input$age_months_time_3),
        measuretime1 = as.numeric(input$measure_time_1),
        measuretime2 = as.numeric(input$measure_time_2),
        measuretime3 = as.numeric(input$measure_time_3),
        trait = input$trait_time
      )
    )
    
    file.copy("multipletime.pdf", file)
  }
)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
