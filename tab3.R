# Single trait, multiple time points

tab3 <- tabPanel("Multiple Timepoints",
         fluidRow(
           column(4,
                  selectInput("sex_time",
                              label = "Sex",
                              choices = c("Female", "Male"),
                              selected = "Male")),
           column(4,
                  selectInput("trait_time",
                              label = "Measurement",
                              choices = traits,
                              selected = "Sella-Basion")),
           column(4)
         ),
         fluidRow(
           column(4,
                  selectInput("age_yrs_time_1",
                              label = "Age 1 (yr)",
                              choices = 4:25,
                              selected = 9)),
           fluidRow(
             column(4,
                    selectInput("age_months_time_1",
                                label = "Age 1 (mo)",
                                choices = 0:11,
                                selected = 0)),
             fluidRow(
               column(4,
                      textInput("measure_time_1",
                                label = "Observed measurement 1 (mm)",
                                value = 33.4)))
           )
         ),
         fluidRow(
           column(4,
                  selectInput("age_yrs_time_2",
                              label = "Age 2 (yr)",
                              choices = 4:25,
                              selected = 12)),
           fluidRow(
             column(4,
                    selectInput("age_months_time_2",
                                label = "Age 2 (mo)",
                                choices = 0:11,
                                selected = 0)),
             fluidRow(
               column(4,
                      textInput("measure_time_2",
                                label = "Observed measurement 2 (mm)",
                                value = 35.8)))
           )
         ),
         fluidRow(
           column(4,
                  selectInput("age_yrs_time_3",
                              label = "Age 3 (yr)",
                              choices = 4:25,
                              selected = 16)),
           fluidRow(
             column(4,
                    selectInput("age_months_time_3",
                                label = "Age 3 (mo)",
                                choices = 0:11,
                                selected = 0)),
             fluidRow(
               column(4,
                      textInput("measure_time_3",
                                label = "Observed measurement 3 (mm)",
                                value = 36)))
           )
         ),
         fluidRow(
           column(4,
                  actionButton("update2", "Calculate")),
           column(4,
                  downloadButton("download3", "Download Report"))
         ),
         includeHTML("Multi_time.html"),
         fluidRow(
           column(12,
                  DT::dataTableOutput("multi_time_table"))
         ),
         fluidRow(column(12,
                         plotOutput("multi_time_plot", height = "500px"))
         )
)
