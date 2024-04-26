# Single measurement, single time point

tab1 <- tabPanel("Single Measurement Percentile",
         fluidRow(
           column(4,
                  selectInput("sex",
                              label = "Sex",
                              choices = c("Female", "Male")),
                  fluidRow(
                    column(6, selectInput("age_yrs",
                                          label = "Years",
                                          choices = 4:25,
                                          selected = 12)),
                    column(6, selectInput("age_months",
                                          label = "Months",
                                          choices = 0:11))
                  )
           ),
           column(8,
                  selectInput("measurement",
                              label = "Measurement",
                              choices = traits,
                              selected = "Nasion-Basion"),
                  textInput("measure",
                            label = "Observed measurement (mm)",
                            value = "100")
           )
         ),
         
         fluidRow(
           column(4,
                  actionButton("update", "Calculate")),
           column(4,
                  downloadButton("download1", "Download Report"))
         ),
         
         fluidRow(
           column(8,
                  plotOutput("plot", height = "500px")),
           column(4,
                  plotOutput("traitfig", height = "500px"))
         )
)

