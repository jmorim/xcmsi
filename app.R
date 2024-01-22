library(shiny)
library(xcms)

options(shiny.maxRequestSize=900 * 1024^2)

ui = fluidPage(
  titlePanel("xcmsi"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Upload file', accept = c('.mzML', '.mzXML')),
      sliderInput("ppm",
                  "ppm tolerance: ",
                  min = 1,
                  max = 100,
                  value = 20),
      sliderInput('peakwidth',
                  'peak width: ',
                  min = 0,
                  max = 60,
                  value = c(5, 10),
                  step = 0.1
                  ),
      sliderInput('snthresh',
                  'snthresh: ',
                  min = 0,
                  max = 1000,
                  value = 10),
      sliderInput('noise',
                  'noise: ',
                  min = 0,
                  max = 10000,
                  value = 1000),
      sliderInput('prefilter_peaks',
                  'prefilter peaks: ',
                  min = 1,
                  max = 10,
                  value = 3),
      sliderInput('prefilter_int',
                  'prefilter intensity: ',
                  min = 0,
                  max = 10000,
                  value = 1000)
    ),
    mainPanel(
      plotOutput("chromPlot"),
      tableOutput('peakList')
    )
  )
);

server = function(input, output, session) {
  print(getOption('shiny.maxRequestSize'))
  data = reactive({
    inFile = input$file
    if (is.null(inFile))
      return(NULL)
    return(readMSData(inFile$datapath, mode = 'onDisk'))
  })

  params = reactive({
    CentWaveParam(
      snthresh = input$snthresh,
      noise = input$noise,
      ppm = input$ppm,
      peakwidth = input$peakwidth,
      prefilter = c(input$prefilter_peaks, input$prefilter_int)
    )
  })

  chromatograms = reactive({
    if (is.null(data()))
      return(NULL)
    chromatogram(data())
  })

  peaks = reactive({
    if (is.null(chromatograms()))
      return(NULL)
    return(findChromPeaks(chromatograms(), param = params()))
  })

  output$chromPlot = renderPlot({
    if (is.null(peaks()))
      return(NULL)
    plot(peaks())
  })

  output$peakList = renderTable({
    if (is.null(peaks()))
      return(NULL)
    chromPeaks(peaks())
  })
}

shinyApp(ui, server)