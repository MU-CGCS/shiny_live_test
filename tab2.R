# Multiple traits, single time point

tab2 <- tabPanel("Multiple Measurements",
                 fluidRow(
                   column(4,
                          selectInput("sex_multi",
                                      label = "Sex",
                                      choices = c("Female", "Male"))),
                   column(4,
                          selectInput("age_yrs_multi",
                                      label = "Years",
                                      choices = 4:25,
                                      selected = 12)),
                   column(4)
                 ),
                 fluidRow(column(4),
                          column(4,
                                 selectInput("age_months_multi",
                                             label = "Months",
                                             choices = 0:11)),
                          column(4)
                 ),
                 fluidRow(column(4,
                                 selectInput("trait_multi_1",
                                             label = "Measurement 1",
                                             choices = traits,
                                             selected = "ANS-PNS")),
                          column(4,
                                 textInput("measure_multi_1",
                                           label = "Observed measurement (mm)",
                                           value = "45")),
                          column(4)
                 ),
                 fluidRow(column(4,
                                 selectInput("trait_multi_2",
                                             label = "Measurement 2",
                                             choices = traits,
                                             selected = "Articulare-Pogonion")),
                          column(4,
                                 textInput("measure_multi_2",
                                           label = "Observed measurement (mm)",
                                           value = "90")),
                          column(4)
                 ),
                 fluidRow(column(4,
                                 selectInput("trait_multi_3",
                                             label = "Measurement 3",
                                             choices = traits,
                                             selected = "Nasion-Basion")),
                          column(4,
                                 textInput("measure_multi_3",
                                           label = "Observed measurement (mm)",
                                           value = "100")),
                          column(4)
                 ),
                 fluidRow(
                   column(4,
                          actionButton("update1", "Calculate")),
                    column(4,
                          downloadButton("download2", "Download Report"))
                 ),
                 includeHTML("Multi_trait.html"),
                 fluidRow(column(12,
                                 DT::dataTableOutput("multi_trait_table"))
                 )
)
