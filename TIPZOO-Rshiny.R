
### code by Emmanuel Discamps https://tipzoo.cnrs.fr
# TIPZOO-Rshiny v1
# last edit: 20th January 2025
### there's still much to include & debug, don't hesitate to contact me!

#checking if you have the appropriate packages for TIPZOO-RS
require(readxl) 
require(Rmisc) 
require(dplyr)
require(DescTools) 
require(ggplot2) 
require(reshape2) 
require(DT) 
require(gridExtra)
library(cowplot) 
require(patchwork) 
require(shinyWidgets)
require(openxlsx) 
library(ggrepel)
library(DT)
library(shiny)
library(shinydashboard)
library(raster)



#tabitem INTRO
  
ui_tabINTRO <-  fluidPage(
br(),
img(src='tipzoo-logo.png', height = "100px", align = "center"), p("TIPZOO-Rshiny v1, released 20th January 2025"),

           br(),
           p("TIPZOO is a software solution for data recording and analysis of fossil bone collections, specifically designed for zooarchaeologists’ needs."),
           "TIPZOO-Rshiny was designed to help in the analysis of data recorded with TIPZOO-FMP, available at", a(href="https://tipzoo.cnrs.fr/", "https://tipzoo.cnrs.fr/"),
           br(),br(),           br(),
           strong("Click on the menu elements on the left to explore the software."),
br(),
strong("Note that, at any moment, you can click the button in the blue title bar to hide the menu and gain some screen space!"),

           br(),
br(),
br(),
           p("The application was mainly developped by Emmanuel Discamps, with some input (code snippets & reference datasets) from C. Dancette, A. De Roux, S. Graichi, E. Regis-Franzke & M. Thomas (thanks again for their help!)"),
           
           p("If you have any questions, please contact us as at emmanuel.discamps(at)cnrs.fr, or write & subscribe to the TIPZOO mailing list ", a(href="https://framalistes.org/sympa/subscribe/tipzoo", "here.")),
br(),br(),
strong("A quick frequently asked questions:"),br(),
em("Q: what do I need to use this program?"),br(),
"To start using TIPZOO-Rshiny, you need to import the FragmentData.xlsx file exported from TIPZOO-FMP (v5 or newer). First, go to your FileMaker TIPZOO-FMP file, click *Export data* and use the *Export all data for TIPZOO-Rshiny* button. Then, go back to TIPZOO-Rshiny, and go to the *Importing data* panel",
br(),br(),
em("Q: OK, but what if I am using an older version of TIPZOO (before v5)?"),br(),
"The best practice would be to update your TIPZOO-FMP file, following the steps listed on ", a(href="https://tipzoo.cnrs.fr/download/version-history", "this page"), ". If you don't want to update your TIPZOO-FMP file, you can also go to FileMaker and export all the fields to a single Excel file (using File>Export records and selecting all the fields from all related tables). For this to work, note that you might have to rename the columns *Td_f p* and *Td_f s* of the exported Excel file for, respectively, *Td_fp* and *Td_fs*.",
br(),br(),
em("Q: But, wait, I don't even have a TIPZOO database! What can I do?"),br(),
"If you go to *Importing data* panel, a test dataset is provided in order for you to explore the capabilities of TIPZOO-Rshiny. If you like it, consider switching to TIPZOO-FMP for data entry. Else, you can have a look at the structure of the TestData.xlsx file and reformat your data for use in TIPZOO-Rshiny... but I don't recommend it! TIPZOO-Rshiny needs perfectly formatted data."

  )  #end intro










###################################################### UI: IMPORT DATA ########################################################################
ui_tab_import <- fluidPage(
  
  strong("Importing Data", style = "font-size:30px;"),
  br(),
  strong("To start using TIPZOO-RShiny, you need to import the FragmentData.xlsx file exported from TIPZOO below:"),br(),br(),

  hr(),
  fileInput("fragmentdata_file", "First, upload your data (.xlsx)", multiple = FALSE),
  em("If you just want to explore the app, you can import a test dataset:"), 
  checkboxInput("import_testdata", "Use test dataset instead of my own", value = FALSE),
  hr(),
  selectInput("selected_group",
              "Secondly, select the group on which you want to base your analysis (for example, use SPATIAL::USfield to use field layers):",
              choices = c("SPATIAL::USfield", "SPATIAL::Group1", "SPATIAL::Group2", "SPATIAL::Group3", "SPATIAL::Square_field", "SPATIAL::Dec", "SPATIAL::Year", "SPECIES::Taxon",
                          "SKEL::Anat_Class", "SKEL::Anat"),
              width = "100%"),
  
  hr(),
  p(textOutput("site_name_text")),
  hr(),
  
  
  h5(strong("View of the imported data that will be used in the application:")),
  tabsetPanel(
    tabPanel("Group summary",
             DT::dataTableOutput("groupdata_table")
    ),
    tabPanel("SpeciesData table",
             DT::dataTableOutput("speciesdata_table")
    ),
    tabPanel("TaphoData table",
             DT::dataTableOutput("taphodata_table")
    ),
    tabPanel("NDEData table",
             DT::dataTableOutput("ndedata_table")
    ),
    tabPanel("CutData table",
             DT::dataTableOutput("cutdata_table")
    ),
    tabPanel("FragmentData table (with group)",
             DT::dataTableOutput("fragmentdata_imported_table"),
    ),
    tabPanel("FragmentData table (raw)",
             DT::dataTableOutput("fragmentdata_table")
    )
   ), #end tabPanel import   
    
    
    br()
)








###################################################### UI: SPECIES ########################################################################

ui_tab_species <- fluidPage(
  
  strong("Species abundance", style = "font-size:30px;"),
  
  p("Once you imported your data in the *Importing Data* panel, select the groups and taxa you want to analyse in the sidebar."),
  selectInput("species_codes",
              "",
              choices = list("All available codes" = "SpeciesCodes.csv",
                             "English codes only" = "SpeciesCodes_EN.csv",
                             "French codes only" = "SpeciesCodes_FR.csv"
              )),
  # actionButton("button_species", "Perform species analysis"),
  
  tabsetPanel(
    tabPanel("Faunal lists (table)",
             downloadButton("export_species_table", "Download as an Excel sheet"),
             dataTableOutput("species_table")
    ),
    tabPanel("Plot one taxon",
             br(),
             selectInput("species_plot_selected", "Select the taxon you want to plot (%NISP)", choices = ""), 
             em("The %NISP is calculated relative to all the taxa included in the analysis. Make sure to use the sidebar (*Taxa to analyse*) to remove any unwanted taxa (e.g. NID, MAM2, etc.) from the %NISP calculations."), br(),br(),

             plotOutput("species_plot"),
             downloadButton("saveplot_species", "Download Plot")
             
    ))
  

  
  
)


###################################################### UI: TAPHO ########################################################################


ui_tab_tapho <- fluidPage(
  strong("Bone taphonomy", style = "font-size:30px;"),
  
             p("Once you imported your data in the *Importing Data* panel, select the groups and taxa you want to analyse in the sidebar, and click the button below:"),
             actionButton("button_tapho", "Perform/update taphonomic analyses"),br(),
  em("If you later change your group/taxa sidebar selections, remember to click that button again to update results!"),
             hr(),
  strong(textOutput("tapho_selected_records_text")),
  textOutput("tapho_selected_group_text"),
  textOutput("tapho_selected_taxa_text"),
hr(),

             tabPanel(
               br(),
               tabsetPanel(

                 ### TAPHO FIGURES

                 tabPanel("Figures",
                          br(),
                          selectInput("label_language_figtapho",
                                      "You can change the label language below:",
                                      choices = list("English" = "EN",
                                                     "French" = "FR" )),




                          tabsetPanel(
                            tabPanel("Readibility",
                                     br(),
                                     plotOutput("barRead_output"),
                                     downloadButton("saveplot_barRead", "Download Plot")
                                     
                            ),
                            tabPanel("Bone color",
                                     br(),
                                     plotOutput("barColor"),
                                     downloadButton("saveplot_barColor", "Download Plot")
                                                          ),

                            #Tapho Accu

                            tabPanel(("Bone accumulator"),
                                     br(),
                                     "Select the variable you want to plot:",
                                     br(),
                                     selectInput("var_taphoaccu", label = NULL ,
                                                 choices = list("Cut marks (Cut)" = "Cut",
                                                                "Carnivore marks (Ctot)" = "Ctot",
                                                                "Digested remains (Dig)" = "Dig",
                                                                "Percussion/pressure marks (Ptot)" = "Ptot",
                                                                "Burnt bones (Burnt)" = "Burnt")),
                                     plotOutput("plot_taphoaccu"),
                                     downloadButton("saveplot_taphoaccu", "Download Plot")
                            ),

                            # Partie Tapho nat
                            tabPanel(("Taphonomic alterations"),
                                     br(),
                                     "Select the variable you want to plot:",
                                     br(),
                                     selectInput("var_taphonat",label = NULL,
                                                 choices = list("Exfo" = "Exfo",
                                                                "DenD" = "DenD",
                                                                "DenE" = "DenE",
                                                                "Sheet1" = "Sheet1",
                                                                "Sheet2" = "Sheet2",
                                                                "Abra1" = "Abra1",
                                                                "Abra2" = "Abra2",
                                                                "CirE" = "CirE",
                                                                "Crack1" = "Crack1",
                                                                "Crack2" = "Crack2",
                                                                "Conc1" = "Conc1",
                                                                "Conc2" = "Conc2",
                                                                "Conc3" = "Conc3",
                                                                "Black1" = "Black1",
                                                                "Black2" = "Black2",
                                                                "Black3" = "Black3",
                                                                "Tramp" = "Tramp",
                                                                "TrampUnsure" = "TrampUnsure",
                                                                "Cor1" = "Cor1",
                                                                "Cor2" = "Cor2")),
                                     plotOutput("plot_taphonat"),
                                     downloadButton("saveplot_taphonat", "Download Plot")
                            ),

                            #multiplot accu
                            tabPanel("Multiplot Accumulation",
                                     br(),
                                     "Select the variables you want to include in the multiplot:",
                                     br(),
                                     pickerInput("multi_var_taphoaccu", label = NULL ,

                                                 choices = c("Cut", "Ctot", "Dig", "Ptot", "Burnt"),
                                                 options = list(`actions-box` = TRUE, `none-selected-text` = "Variables to plot"),
                                                 multiple = TRUE,
                                                 selected = NULL),
                                     br(),
                                     downloadButton("download_multiplot_accu", "Download Multiplot PDF"),
                                     plotOutput("multiplot_taphoaccu")
                            ),


                            #multiplot nat

                            tabPanel("Multiplot Natural",
                                     br(),
                                     "Select the variables you want to include in the multiplot:",
                                     br(),
                                     pickerInput("multi_var_taphonat", label = NULL ,

                                                 choices = c("Exfo", "DenD", "DenE", "Sheet1", "Sheet2", "Abra1", "Abra2", "CirE", "Crack1", "Crack2", "Conc1", "Conc2", "Conc3", "Black1", "Black2", "Black3", "Tramp", "TrampUnsure", "Cor1", "Cor2"),
                                                 options = list(`actions-box` = TRUE, `none-selected-text` = "Variables to plot"),
                                                 multiple = TRUE,
                                                 selected = NULL),
                                     br(),
                                     downloadButton("download_multiplot_nat", "Download Multiplot PDF"),
                                     plotOutput("multiplot_taphonat")
                            )


                          ),#end tabsetPanel tapho figs

),
                          tabPanel("Raw tables",
                                   br(),
                                   downloadButton("export_tapho_tables", "Download all tables below in one Excel file"),
                                   br(),                                   br(),
                                   
                                   
                                   "*Accu* refers to bone surface modifications pertaining to the study of bone accumulating agents.",
                                   br(),
                                   "*Nat* to bone surface modifications more in link with natural agents affecting bone surfaces.",
                                   br(),
                                   "*Fracture types* reports NISP numbers for bone fractures (green, dry, recent) as well as the percentage of green breaks relative to ancient breaks (last column)",
                                   br(),
                                   "*Concretion types* reports NISP numbers for concretion types",
                                   br(),
                                   "*Bone color* reports NISP numbers",
                                   br(),                                   br(),
                                   
                                   "The *Accu* and *Nat* tables report, for each type of alteration (rows) and by group (columns):",
                                   br(),                                   
                                   "- the percentage of altered remains (%NISP) (*Percent*)",
                                   br(),                                   
                                   "- the number of remains (NISP) bearing that alteration (*n*)",
                                   
                                   br(),
                                   "- the total number of observed remains (*Ntot*)",

                                   br(),                                   br(),

                                   br(),
                                   br(),
                                   tabsetPanel(
                                     tabPanel("Accu Percent", DT::dataTableOutput("table_dfTaphoAccu_p")),
                                     tabPanel("Accu n", DT::dataTableOutput("table_dfTaphoAccu_n")),
                                     tabPanel("Accu Ntot", DT::dataTableOutput("table_dfTaphoAccu_Ntot")),
                                     tabPanel("Nat Percent", DT::dataTableOutput("table_dfTaphoNat_p")),
                                     tabPanel("Nat n", DT::dataTableOutput("table_dfTaphoNat_n")),
                                     tabPanel("Nat Ntot", DT::dataTableOutput("table_dfTaphoNat_Ntot")),
                                     tabPanel("Fracture types", DT::dataTableOutput("table_TableFract")),
                                     tabPanel("Concretion types", DT::dataTableOutput("table_TableConcType")),
                                     tabPanel("Bone color", DT::dataTableOutput("table_TableBoneColor")),
                                     tabPanel("Raw dataset", DT::dataTableOutput("table_raw_dataset"))
                                   )
                          )






                 ), #end tabsetPanel tapho





               )


             ) #end bone taphonomy


  









###################################################### UI: NDE ########################################################################

ui_tab_nde <- fluidPage(
  strong("Skeletal-part analysis", style = "font-size:30px;"),
  
  p("Once you imported your data in the *Importing Data* panel"),
    strong("First, select the groups and taxa you want to analyse in the sidebar (note that selecting the appropriate taxa is important here e.g. RANG and MAM2)"),br(),br(),
  selectInput("ndecodes_selected", label = "Then, select the NDE codelist (i.e. quantity of bones present in a complete skeleton) that you want to use:*", 
              choices = list("Reindeer, all NDE (Morin et al. & new ones from TIPZOO), including measurements" = "ref_data/NDEcodes_RANGwithmes.csv",
                             "Reindeer, all NDE (Morin et al. & new ones from TIPZOO), WITHOUT measurements" = "ref_data/NDEcodes_RANGnomes.csv",
                             "Bovinae, all NDE (Morin et al. & new ones from TIPZOO), including measurements" = "ref_data/NDEcodes_BBwithmes.csv",
                             "Bovinae, all NDE (Morin et al. & new ones from TIPZOO), WITHOUT measurements" = "ref_data/NDEcodes_BBnomes.csv",
                             "Horse, all NDE (Morin et al. & new ones from TIPZOO), WITHOUT measurements" = "ref_data/NDEcodes_CABnomes.csv",
                             "Caprinae, all NDE (Morin et al. & new ones from TIPZOO), including measurements" = "ref_data/NDEcodes_CAPRIwithmes.csv",   
                             "Caprinae, all NDE (Morin et al. & new ones from TIPZOO), WITHOUT measurements" = "ref_data/NDEcodes_CAPRInomes.csv",   
                             "Red deer, all NDE (Morin et al. & new ones from TIPZOO), WITHOUT measurements" = "ref_data/NDEcodes_CELnomes.csv",
                             "Suidae, all NDE (Morin et al. & new ones from TIPZOO), WITHOUT measurements" = "ref_data/NDEcodes_SUInomes.csv"
                             ),
              width = "100%"
  ),
  strong("and click that button:"),  actionButton("button_nde", "Perform/update skeletal-part analysis"),br(),
    em("* check the ref_data_README.rtf file in ref_data folder for more information on NDE codelists"),br(),br(),
em("If you later change your group/taxa sidebar selections, remember to click that button again to update results!"),
  hr(),
  strong(textOutput("nde_selected_records_text")),
  textOutput("nde_selected_group_text"),
  textOutput("nde_selected_taxa_text"),
  hr(),
  
  selectInput("list_ndedata_group",
              "Choose the specific group that you want to analyse:", 
              choices = ""),

  br(),
  tabsetPanel(
    tabPanel("Skeletal-part representation",
             selectInput("skeleton", label = "Select your skeleton:", choices = list("Reindeer" = "shapefiles/rang_polygons.shp", 
                                                                                     "Bison" = "shapefiles/bison_polygons.shp",
                                                                                     "Horse" = "shapefiles/equus_ferus_polygons.shp", 
                                                                                     "Ibex" = "shapefiles/capra_ibex_polygons.shp", 
                                                                                     "Red deer" = "shapefiles/cervus_polygons.shp",                                                                                                                                                                    "Roe deer" = "shapefiles/capreo_polygons.shp", 
                                                                                     "Wild boar" = "shapefiles/sus_scrofa_polygons.shp")
             ),
             plotOutput("skeletal_representation"),
             downloadButton("skeletal_plot", "Download"), br(),
             em("As of now, these figures are far from perfect: I suggest exporting the plot, opening the pdf in Inkscape/Illustrator, add a legend (the 4 colors correspond to %MAU 0-25%, 25-50%, 50-75% and 75-100%) and re-order the polygons (e.g. scapula not on top). I'll work on it in the future!")

            
    ),
    
    tabPanel("Bone density analysis",
            br(),
             selectInput("bonedensity_ref",
                        label = "Select the bone density reference data that you want to use:",
                         choices = list("Choose one" = "",
                                        "Bison (Kreutzer 1992)" = "ref_data/BoneDensity_Bison-Kreutzer92.csv",
                                        "Equid (Lam et al. 1999)" = "ref_data/BoneDensity_Equid-Lametal99.csv",
                                        "Rangifer (Lam et al. 1999)" = "ref_data/BoneDensity_RANG-Lametal99.csv",
                                        "Sus domesticus (Ioannidou 2003)" = "ref_data/BoneDensity_SUD-Ioannidou03.csv",
                                        "Ovis aries (Ioannidou 2003)" = "ref_data/BoneDensity_OVIA-Ioannidou03.csv"
                         ),
                        width = "100%"
                        ),
             tabsetPanel(
             tabPanel("Analysis by layer",
                      br(),
                      strong("Information on sample size:"),
                      textOutput("total_MAU_density"),
                      textOutput("max_MAU_density"),
                      br(),
                      strong("Spearman correlation test:"),
                      verbatimTextOutput("cor_test_BoneDensity"),
                      br(),
                      strong("Plot:"),
                      plotOutput("BoneDensityPlot"),
                      br(),
                      downloadButton("saveplot_density", "Download Plot"),
                      br()
                                        ),
             tabPanel("Full table",
                      DT::dataTableOutput("table_ndedata_analysis_boneDensityData"), 
                      
                      "lol") 
             )
    ),

    tabPanel("Utility analysis",
             br(),
             selectInput("index_ref",
                         "Select the utility index that you want to use:",
                         choices = list("Choose one" = "",
                                        "Rangifer marrow UMI (Morin 2007)" = "ref_data/MarrowUMI_RANG.csv",
                                        "Rangifer marrow cavity volume in mL (Binford 1978)" = "ref_data/MarrowCav_RANG.csv",
                                        "Rangifer food SFUI (Metcalfe & Jones 1988)" = "ref_data/FoodSFUI_RANG.csv",
                                        "Horse marrow cavity volume in mL (Outram & Rowley-Conwy 1998)" = "ref_data/MarrowCav_CAB.csv",
                                        "Horse meat weight (Outram & Rowley-Conwy 1998)" = "ref_data/MeatWeight_CAB.csv",
                                        "Sus food SFUI (Rowley-Conwy et al. 2002)" = "ref_data/FoodSFUI_SUS.csv"
                         ),
                         width = "100%"),
             tabsetPanel(

               tabPanel("Analysis by layer",
                        br(),
                        strong("Information on sample size:"),
                        textOutput("total_MAU_index"),
                        textOutput("max_MAU_index"),
                        br(),
                        strong("Spearman correlation test:"),
                        verbatimTextOutput("cor_test_Index"),
                        br(),
                        strong("Plot:"),

                        plotOutput("IndexPlot"),
                        downloadButton("saveplot_index", "Download Plot"),
                    
                        br()),
               tabPanel("Full table",
                        DT::dataTableOutput("table_ndedata_analysis_indexData"),

                        "lol")
             )
    ),
    
    tabPanel("Raw tables (use for export)",
             br(),
             downloadButton("export_nde_tables", "Download all tables below in one Excel file"),
             br(),                                   br(),
             
             tabsetPanel(
               
               
               tabPanel("NDEData",
                        DT::dataTableOutput("table_ndedata_analysis_NDEData")),
               tabPanel("MAUfromNDE",
                        DT::dataTableOutput("table_ndedata_analysis_MAUfromNDE")),
               tabPanel("MNIfromNDE",
                        DT::dataTableOutput("table_ndedata_analysis_MNIfromNDE")),
               tabPanel("MAUbyElement",
                        DT::dataTableOutput("table_ndedata_analysis_MAUbyElement")),
               tabPanel("MAUbyElementPortion",
                        DT::dataTableOutput("table_ndedata_analysis_MAUbyElementPortion")),
               tabPanel("pMAUbyElement",
                        DT::dataTableOutput("table_ndedata_analysis_pMAUbyElement")),
               tabPanel("pMAUbyElementPortion",
                        DT::dataTableOutput("table_ndedata_analysis_pMAUbyElementPortion"))
              
             )
    )
  )
)









###################################################### UI: CUT ########################################################################
ui_tab_cut <- fluidPage(
  strong("Cut-mark analysis", style = "font-size:30px;"),
  
  p("Once you imported your data in the *Importing Data* panel, select the groups and taxa you want to analyse in the sidebar, then"),
  selectInput("cutcodes_selected", label = "Select the cut-marks codelist you want to use:", 
              choices = list("English (Soulier & Costamagno 2017)" = "ref_data/CutCodesEN.csv",
                             "French (Soulier & Costamagno 2017)" = "ref_data/CutCodesFR.csv"),
              width = "100%"
  ),
  "and click that button:",
  actionButton("button_cut", "Perform/update cut-mark analysis"),br(),
  em("If you later change your group/taxa sidebar selections, remember to click that button again to update results!"),
  hr(),
  strong(textOutput("cut_selected_records_text")),
  textOutput("cut_selected_group_text"),
  textOutput("cut_selected_taxa_text"),
  hr(),
  
  downloadButton("export_cut_tables", "Download all tables below in one Excel file"),
  
  tabsetPanel(
    tabPanel("CutData analysis",
             DT::dataTableOutput("cutdata_analysis_table")),
    
    tabPanel("CutData long analysis",
             DT::dataTableOutput("cutdatalong_analysis_table"))
  )
)


ui <- dashboardPage(

  dashboardHeader(title = "TIPZOO-Rshiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "tab1"),
      menuItem("Importing data", tabName = "tab_import"),
      menuItem("Species abundance", tabName = "tab_species"),
      menuItem("Bone taphonomy", tabName = "tab_tapho"),
      menuItem("Skeletal-part analysis", tabName = "tab_nde"),
      menuItem("Cut-mark analysis", tabName = "tab_cut"),
      pickerInput("list_sidebar_group", "Groups to analyse:", choices = "", multiple = T,  options = list(  `actions-box` = TRUE, dropupAuto = FALSE, size = 10, windowPadding = "[100,0,0,0]")),
      pickerInput("list_sidebar_taxa", "Taxa to analyse:", choices = "", multiple = T,  options = list(  `actions-box` = TRUE, dropupAuto = FALSE, size = 10, windowPadding = "[100,0,0,0]")), #options necessary to that it does not cause display probem with the top bar
      br()
    )
  ),
  dashboardBody(
    
    # controlling horizontal lines
    tags$head(
      tags$style(HTML("hr {border-top: 2px solid #000000;}"))
    ),
    
    tabItems(
      tabItem("tab1",
              ui_tabINTRO
      ),
      tabItem("tab_import",
              ui_tab_import
      ),
      tabItem("tab_species",
              ui_tab_species
      ),      
      tabItem("tab_tapho",
              ui_tab_tapho
      ),    
      tabItem("tab_nde",
              ui_tab_nde
      ),
      tabItem("tab_cut",
                      ui_tab_cut
      ),
      tabItem("rawdata",
              "rawtable",
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)




  ###################################################### SERVER: IMPORT DATA ########################################################################
server <- function(input,output,session) {

  #creating empty df
  df_fragmentdata<-reactiveVal(NULL)
  df_fragmentdata_imported<-reactiveVal(NULL)
  df_speciesdata<-reactiveVal(NULL)
  df_taphodata<-reactiveVal(NULL)
  df_cutdata<-reactiveVal(NULL)
  df_ndedata<-reactiveVal(NULL)
  df_groupdata<-reactiveVal(NULL)
  
  ## extracting the site name
  site_name<-reactiveVal(NULL)
  
  observe({
    inFile <- input$fragmentdata_file
    if (!is.null(inFile)){
      data <- read_excel(inFile$datapath, sheet=1, col_types = "text", n_max = 1)
      name <- data[1,"SiteName"]
      site_name(name)
    }
    if (input$import_testdata == "TRUE"){
      site_name("Test site")
    }
  })
  
  output$site_name_text<- renderText({
    ifelse(!is.null(site_name()),paste("You successfully imported data for the site of",site_name()),"No data imported")
  })
  
  
  # list of columns and coltypes necessary for SpeciesData
  speciesdata_colnames <- c("pk_ID", "fieldID", "SubID", "Size", "PlottedScreen", "Obs", 
                            "SPATIAL::Square_field", "SPATIAL::Dec", "SPATIAL::USfield", 
                            "SPATIAL::Group1", "SPATIAL::Group2", "SPATIAL::Group3", "SPATIAL::X", 
                            "SPATIAL::Y", "SPATIAL::Z", "SPATIAL::Year", "SPECIES::Taxon", 
                            "SPECIES::CheckImprove", "SPECIES::ObsTaxon", "SKEL::Anat_Class", 
                            "SKEL::Anat", "SKEL::Anat_Detail", "SKEL::Frag", "SKEL::AgeCort", 
                            "SKEL::Spongy", "RemID", "REFITS::RemType", "SPECIES::fk_ID")
  
  speciesdata_coltypes <- c("numeric","guess","numeric",rep("text",9),rep("numeric",3),"guess",rep("text",9),"guess","text","numeric")
  
  # list of columns and coltypes necessary for TaphoData
  taphodata_colnames <- c("pk_ID", "fieldID", "SubID", "Size", "PlottedScreen", "Obs", 
                          "SPATIAL::Square_field", "SPATIAL::Dec", "SPATIAL::USfield", 
                          "SPATIAL::Group1", "SPATIAL::Group2", "SPATIAL::Group3", "SPATIAL::X", 
                          "SPATIAL::Y", "SPATIAL::Z", "SPATIAL::Year", "SPECIES::Taxon", 
                          "SKEL::Anat_Class", "SKEL::Anat", "SKEL::Anat_Detail", "SKEL::Frag", 
                          "SKEL::AgeCort", "SKEL::Spongy", "Burnt", "BoneColor", "TAPHO::Conc", 
                          "TAPHO::ConcType", "TAPHO::Cut", "TAPHO::Fract1", "TAPHO::Fract2", 
                          "TAPHO::Ret", "TAPHO::Scrape_Asso", "TAPHO::Sheet", "TAPHO::Tramp", 
                          "TAPHO::Abra", "TAPHO::BoneTool", "TAPHO::C_cren", "TAPHO::C_furr", 
                          "TAPHO::C_pit", "TAPHO::C_punct", "TAPHO::C_scoop", "TAPHO::C_scor", 
                          "TAPHO::CorDig", "TAPHO::Chop", "TAPHO::CirE", "TAPHO::Exfo", 
                          "TAPHO::Crack", "TAPHO::Black", "TAPHO::P_cort", "TAPHO::P_flake", 
                          "TAPHO::P_tooth", "TAPHO::P_scar", "TAPHO::P_mark", "TAPHO::P_oppo", 
                          "TAPHO::P_peel", "TAPHO::P_spong", "TAPHO::Read", "TAPHO::Rodent", 
                          "TAPHO::DenD", "TAPHO::DenE", "TAPHO::Scrape", "TAPHO::P_SUM", 
                          "TAPHO::C_SUM", "RemID", "REFITS::RemType", "TAPHO::fk_ID")
  
  taphodata_coltypes <- c("numeric","guess","numeric",rep("text",9),rep("numeric",3),"guess",rep("text",47),"guess","text", "numeric")
  
  # list of columns and coltypes necessary for CutData
  cutdata_colnames <- c("pk_ID", "fieldID", "SubID", "SPATIAL::USfield", 
                        "SPATIAL::Group1", "SPATIAL::Group2", "SPATIAL::Group3", "SPECIES::Taxon", 
                        "SKEL::Anat_Class", "SKEL::Anat", "SKEL::Anat_Detail", "CUT::fk_ID", 
                        "CUT::At_a", "CUT::At_b", "CUT::At_bs", "CUT::At_c", "CUT::At_cp", "CUT::At_d", "CUT::At_dp", "CUT::At_ds", 
                        "CUT::At_e", "CUT::At_ep", "CUT::At_es", "CUT::At_f", "CUT::At_fs", "CUT::At_g", "CUT::At_gp", "CUT::At_h", 
                        "CUT::At_i", "CUT::Ax_a", "CUT::Ax_ap", "CUT::Ax_b", "CUT::Ax_bp", "CUT::Ax_bs", "CUT::Ax_c", "CUT::Ax_cp", 
                        "CUT::Ax_d", "CUT::Ax_dp", "CUT::Ax_e", "CUT::Ax_ep", "CUT::Ax_f", "CUT::Ax_fp", "CUT::Ax_g", "CUT::Ax_gp", 
                        "CUT::Ax_h", "CUT::Ax_i", "CUT::Ax_ip", "CUT::Ax_j", "CUT::Ax_jp", "CUT::Ax_js", "CUT::Ax_k", "CUT::Ax_kp", 
                        "CUT::Ax_ks", "CUT::Cal_a", "CUT::Cal_b", "CUT::Cal_c", "CUT::Cal_dp", "CUT::Cal_e", "CUT::Cal_ep", 
                        "CUT::Cal_f", "CUT::Cal_g", "CUT::Cal_gp", "CUT::Cal_h", "CUT::Cal_i", "CUT::Cbn_a", "CUT::Cbn_ap", 
                        "CUT::Ctt_a", "CUT::Ctt_b", "CUT::Cv_a", "CUT::Cv_ap", "CUT::Cv_b", "CUT::Cv_bp", "CUT::Cv_c", "CUT::Cv_cp", 
                        "CUT::Cv_d", "CUT::Cv_dp", "CUT::Cv_ds", "CUT::Cv_e", "CUT::Cv_f", "CUT::Cv_g", "CUT::Cv_gs", "CUT::Cv_h", 
                        "CUT::Cv_i", "CUT::Cv_ip", "CUT::Cv_j", "CUT::Cv_jp", "CUT::Cv_k", "CUT::Cv_kp", "CUT::Cv_l", "CUT::Cv_lp", 
                        "CUT::Cv_ls", "CUT::Cv_m", "CUT::Cv_n", "CUT::Cv_o", "CUT::Fd_a", "CUT::Fd_b", "CUT::Fd_c", "CUT::Fd_cp", 
                        "CUT::Fd_d", "CUT::Fd_e", "CUT::Fd_f", "CUT::Fd_g", "CUT::Fd_h", "CUT::Fd_i", "CUT::Fib_a", "CUT::Fib_b", 
                        "CUT::Fib_c", "CUT::Fp_a", "CUT::Fp_b", "CUT::Fp_c", "CUT::Fp_cp", "CUT::Fp_d", "CUT::Fp_dp", "CUT::Fp_e", 
                        "CUT::Fp_ep", "CUT::Fp_f", "CUT::Fp_fp", "CUT::Fp_g", "CUT::Fp_h", "CUT::Fp_i", "CUT::Fp_ip", "CUT::Fs_a", 
                        "CUT::Fs_ap", "CUT::Gcf_a", "CUT::Hd_a", "CUT::Hd_ap", "CUT::Hd_b", "CUT::Hd_c", "CUT::Hd_cp", "CUT::Hd_d", 
                        "CUT::Hd_dp", "CUT::Hd_e", "CUT::Hd_f", "CUT::Hd_g", "CUT::Hd_gp", "CUT::Hd_h", "CUT::Hd_hp", "CUT::Hp_a", 
                        "CUT::Hp_ap", "CUT::Hp_b", "CUT::Hp_c", "CUT::Hp_cp", "CUT::Hp_d", "CUT::Hp_e", "CUT::Hp_ep", "CUT::Hp_f", 
                        "CUT::Hp_g", "CUT::Hp_gp", "CUT::Hp_h", "CUT::Hp_hp", "CUT::Hp_ip", "CUT::Hp_j", "CUT::Hp_k", "CUT::Hp_kp", 
                        "CUT::Hp_l", "CUT::Hp_lp", "CUT::Hp_m", "CUT::Hp_mp", "CUT::Hs_a", "CUT::Hs_ap", "CUT::Lun_a", "CUT::Lun_ap", 
                        "CUT::Lun_b", "CUT::Lun_bp", "CUT::Lun_c", "CUT::Lv_a", "CUT::Lv_ap", "CUT::Lv_b", "CUT::Lv_bp", 
                        "CUT::Lv_bs", "CUT::Lv_c", "CUT::Lv_cp", "CUT::Lv_cs", "CUT::Lv_d", "CUT::Lv_dp", "CUT::Lv_ds", 
                        "CUT::Lv_e", "CUT::Lv_ep", "CUT::Lv_es", "CUT::Lv_f", "CUT::Lv_g", "CUT::Lv_h", "CUT::Lv_hp", "CUT::Lv_i", 
                        "CUT::Lv_ip", "CUT::Lv_j", "CUT::Lv_k", "CUT::Lv_kp", "CUT::Lv_l", "CUT::Lv_m", "CUT::Lv_mp", "CUT::Lv_n", 
                        "CUT::Lv_ns", "CUT::Lv_o", "CUT::Man_a", "CUT::Man_ap", "CUT::Man_b", "CUT::Man_bp", "CUT::Man_c", 
                        "CUT::Man_cp", "CUT::Man_d", "CUT::Man_dp", "CUT::Man_e", "CUT::Man_ep", "CUT::Man_f", "CUT::Man_fp", 
                        "CUT::Man_g", "CUT::Man_gp", "CUT::Man_gs", "CUT::Man_h", "CUT::Man_i", "CUT::Mcd_a", "CUT::Mcd_b", 
                        "CUT::Mcd_bp", "CUT::Mcd_c", "CUT::Mcd_cp", "CUT::Mcd_d", "CUT::Mcp_a", "CUT::Mcp_b", "CUT::Mcp_d", 
                        "CUT::Mcp_e", "CUT::Mcs_a", "CUT::Mcs_ap", "CUT::Mcs_as", "CUT::Mcs_b", "CUT::Mcs_bp", "CUT::Mcs_bs", 
                        "CUT::Mcs_c", "CUT::Mcs_cp", "CUT::Mcs_d", "CUT::Mcs_e", "CUT::Mcs_f", "CUT::Mcs_fp", "CUT::Ml_a", 
                        "CUT::Mtd_a", "CUT::Mtd_b", "CUT::Mtd_bp", "CUT::Mtd_bs", "CUT::Mtd_c", "CUT::Mtd_cp", "CUT::Mtd_d", 
                        "CUT::Mtp_a", "CUT::Mtp_b", "CUT::Mtp_bs", "CUT::Mtp_c", "CUT::Mtp_d", "CUT::Mtp_e", "CUT::Mts_a", 
                        "CUT::Mts_ap", "CUT::Mts_as", "CUT::Mts_b", "CUT::Mts_bp", "CUT::Mts_bs", "CUT::Mts_c", "CUT::Mts_cp", 
                        "CUT::Mts_d", "CUT::Mts_e", "CUT::Mts_f", "CUT::Mts_fp", "CUT::Pat_a", "CUT::Ph1_a", "CUT::Ph1_b", 
                        "CUT::Ph1_c", "CUT::Ph1_d", "CUT::Ph1_e", "CUT::Ph1_f", "CUT::Ph1_fs", "CUT::Ph1_gp", "CUT::Ph1_h", 
                        "CUT::Ph2_b", "CUT::Ph2_c", "CUT::Ph2_d", "CUT::Ph2_hp", "CUT::Pis_a", "CUT::Pis_b", "CUT::Pis_c", 
                        "CUT::Pis_cp", "CUT::Pis_d", "CUT::Pyr_a", "CUT::Pyr_ap", "CUT::Pyr_b", "CUT::Pyr_c", "CUT::Pyr_cp", 
                        "CUT::Rd_a", "CUT::Rd_ap", "CUT::Rd_b", "CUT::Rd_c", "CUT::Rd_cp", "CUT::Rd_d", "CUT::Rd_dp", "CUT::Rd_e", 
                        "CUT::Rd_ep", "CUT::Rp_a", "CUT::Rp_ap", "CUT::Rp_b", "CUT::Rp_c", "CUT::Rp_cp", "CUT::Rp_d", "CUT::Rp_e", 
                        "CUT::Rp_ep", "CUT::Rp_f", "CUT::Rp_fs", "CUT::Rp_g", "CUT::Rs_a", "CUT::Rs_ap", "CUT::Rs_b", "CUT::Rs_bp", 
                        "CUT::Rs_bs", "CUT::Rs_c", "CUT::Rs_cp", "CUT::Rs_cs", "CUT::Sac_a", "CUT::Sac_ap", "CUT::Sac_b", 
                        "CUT::Sac_c", "CUT::Sac_cp", "CUT::Sac_d", "CUT::Sac_dp", "CUT::Sac_e", "CUT::Sac_ep", "CUT::Sc_a", 
                        "CUT::Sc_ap", "CUT::Sc_as", "CUT::Sc_b", "CUT::Sc_bp", "CUT::Sc_bs", "CUT::Sc_c", "CUT::Sc_cp", 
                        "CUT::Sc_d", "CUT::Sc_e", "CUT::Sc_ep", "CUT::Sca_a", "CUT::Sca_b", "CUT::Sca_c", "CUT::Ses_a", 
                        "CUT::Ses_b", "CUT::Ses_c", "CUT::Ses_d", "CUT::Ses_e", "CUT::Ses_f", "CUT::Ses_g", "CUT::Ses_h", 
                        "CUT::Tal_a", "CUT::Tal_b", "CUT::Tal_c", "CUT::Tal_cs", "CUT::Tal_d", "CUT::Tal_e", "CUT::Td_ap", 
                        "CUT::Td_as", "CUT::Td_b", "CUT::Td_bs", "CUT::Td_c", "CUT::Td_d", "CUT::Td_e", "CUT::Td_f", "CUT::Td_fp", 
                        "CUT::Td_fs", "CUT::Td_g", "CUT::Tp_a", "CUT::Tp_b", "CUT::Tp_bp", "CUT::Tp_c", "CUT::Tp_d", "CUT::Tp_dp", 
                        "CUT::Tp_e", "CUT::Tp_f", "CUT::Tp_fp", "CUT::Ts_a", "CUT::Ts_ap", "CUT::Ts_b", "CUT::Ts_bp", "CUT::Ts_c", 
                        "CUT::Ts_cp", "CUT::Ts_d", "CUT::Ts_dp", "CUT::Ts_ds", "CUT::Ts_e", "CUT::Ts_ep", "CUT::Ts_es", 
                        "CUT::Tv_a", "CUT::Tv_ap", "CUT::Tv_as", "CUT::Tv_b", "CUT::Tv_c", "CUT::Tv_cp", "CUT::Tv_d", "CUT::Tv_dp", 
                        "CUT::Tv_ds", "CUT::Tv_e", "CUT::Tv_es", "CUT::Tv_f", "CUT::Tv_fp", "CUT::Tv_g", "CUT::Tv_gp", "CUT::Tv_h", 
                        "CUT::Tv_hp", "CUT::Tv_i", "CUT::Tv_j", "CUT::Tv_k", "CUT::Tv_l", "CUT::Tv_lp", "CUT::Ud_a", "CUT::Unc_a", 
                        "CUT::Unc_b", "CUT::Up_a", "CUT::Up_ap", "CUT::Up_b", "CUT::Up_bp", "CUT::Up_c", "CUT::Up_d", "CUT::Up_e", 
                        "CUT::Up_f", "CUT::Up_g", "CUT::Up_h", "CUT::Up_i", "CUT::Us_a", "CUT::Us_ap", "CUT::Us_b", "CUT::Us_bs", 
                        "CUT::Vest_a", "CUT::Vest_as")
  
  cutdata_newcolnames <- c("Group","pk_ID", "fieldID", "SubID", "SPATIAL::USfield", 
                        "SPATIAL::Group1", "SPATIAL::Group2", "SPATIAL::Group3", "SPECIES::Taxon", 
                        "SKEL::Anat_Class", "SKEL::Anat", "SKEL::Anat_Detail", "fk_ID", 
                        "At_a", "At_b", "At_bs", "At_c", "At_cp", "At_d", "At_dp", "At_ds", 
                        "At_e", "At_ep", "At_es", "At_f", "At_fs", "At_g", "At_gp", "At_h", 
                        "At_i", "Ax_a", "Ax_ap", "Ax_b", "Ax_bp", "Ax_bs", "Ax_c", "Ax_cp", 
                        "Ax_d", "Ax_dp", "Ax_e", "Ax_ep", "Ax_f", "Ax_fp", "Ax_g", "Ax_gp", 
                        "Ax_h", "Ax_i", "Ax_ip", "Ax_j", "Ax_jp", "Ax_js", "Ax_k", "Ax_kp", 
                        "Ax_ks", "Cal_a", "Cal_b", "Cal_c", "Cal_dp", "Cal_e", "Cal_ep", 
                        "Cal_f", "Cal_g", "Cal_gp", "Cal_h", "Cal_i", "Cbn_a", "Cbn_ap", 
                        "Ctt_a", "Ctt_b", "Cv_a", "Cv_ap", "Cv_b", "Cv_bp", "Cv_c", "Cv_cp", 
                        "Cv_d", "Cv_dp", "Cv_ds", "Cv_e", "Cv_f", "Cv_g", "Cv_gs", "Cv_h", 
                        "Cv_i", "Cv_ip", "Cv_j", "Cv_jp", "Cv_k", "Cv_kp", "Cv_l", "Cv_lp", 
                        "Cv_ls", "Cv_m", "Cv_n", "Cv_o", "Fd_a", "Fd_b", "Fd_c", "Fd_cp", 
                        "Fd_d", "Fd_e", "Fd_f", "Fd_g", "Fd_h", "Fd_i", "Fib_a", "Fib_b", 
                        "Fib_c", "Fp_a", "Fp_b", "Fp_c", "Fp_cp", "Fp_d", "Fp_dp", "Fp_e", 
                        "Fp_ep", "Fp_f", "Fp_fp", "Fp_g", "Fp_h", "Fp_i", "Fp_ip", "Fs_a", 
                        "Fs_ap", "Gcf_a", "Hd_a", "Hd_ap", "Hd_b", "Hd_c", "Hd_cp", "Hd_d", 
                        "Hd_dp", "Hd_e", "Hd_f", "Hd_g", "Hd_gp", "Hd_h", "Hd_hp", "Hp_a", 
                        "Hp_ap", "Hp_b", "Hp_c", "Hp_cp", "Hp_d", "Hp_e", "Hp_ep", "Hp_f", 
                        "Hp_g", "Hp_gp", "Hp_h", "Hp_hp", "Hp_ip", "Hp_j", "Hp_k", "Hp_kp", 
                        "Hp_l", "Hp_lp", "Hp_m", "Hp_mp", "Hs_a", "Hs_ap", "Lun_a", "Lun_ap", 
                        "Lun_b", "Lun_bp", "Lun_c", "Lv_a", "Lv_ap", "Lv_b", "Lv_bp", 
                        "Lv_bs", "Lv_c", "Lv_cp", "Lv_cs", "Lv_d", "Lv_dp", "Lv_ds", 
                        "Lv_e", "Lv_ep", "Lv_es", "Lv_f", "Lv_g", "Lv_h", "Lv_hp", "Lv_i", 
                        "Lv_ip", "Lv_j", "Lv_k", "Lv_kp", "Lv_l", "Lv_m", "Lv_mp", "Lv_n", 
                        "Lv_ns", "Lv_o", "Man_a", "Man_ap", "Man_b", "Man_bp", "Man_c", 
                        "Man_cp", "Man_d", "Man_dp", "Man_e", "Man_ep", "Man_f", "Man_fp", 
                        "Man_g", "Man_gp", "Man_gs", "Man_h", "Man_i", "Mcd_a", "Mcd_b", 
                        "Mcd_bp", "Mcd_c", "Mcd_cp", "Mcd_d", "Mcp_a", "Mcp_b", "Mcp_d", 
                        "Mcp_e", "Mcs_a", "Mcs_ap", "Mcs_as", "Mcs_b", "Mcs_bp", "Mcs_bs", 
                        "Mcs_c", "Mcs_cp", "Mcs_d", "Mcs_e", "Mcs_f", "Mcs_fp", "Ml_a", 
                        "Mtd_a", "Mtd_b", "Mtd_bp", "Mtd_bs", "Mtd_c", "Mtd_cp", "Mtd_d", 
                        "Mtp_a", "Mtp_b", "Mtp_bs", "Mtp_c", "Mtp_d", "Mtp_e", "Mts_a", 
                        "Mts_ap", "Mts_as", "Mts_b", "Mts_bp", "Mts_bs", "Mts_c", "Mts_cp", 
                        "Mts_d", "Mts_e", "Mts_f", "Mts_fp", "Pat_a", "Ph1_a", "Ph1_b", 
                        "Ph1_c", "Ph1_d", "Ph1_e", "Ph1_f", "Ph1_fs", "Ph1_gp", "Ph1_h", 
                        "Ph2_b", "Ph2_c", "Ph2_d", "Ph2_hp", "Pis_a", "Pis_b", "Pis_c", 
                        "Pis_cp", "Pis_d", "Pyr_a", "Pyr_ap", "Pyr_b", "Pyr_c", "Pyr_cp", 
                        "Rd_a", "Rd_ap", "Rd_b", "Rd_c", "Rd_cp", "Rd_d", "Rd_dp", "Rd_e", 
                        "Rd_ep", "Rp_a", "Rp_ap", "Rp_b", "Rp_c", "Rp_cp", "Rp_d", "Rp_e", 
                        "Rp_ep", "Rp_f", "Rp_fs", "Rp_g", "Rs_a", "Rs_ap", "Rs_b", "Rs_bp", 
                        "Rs_bs", "Rs_c", "Rs_cp", "Rs_cs", "Sac_a", "Sac_ap", "Sac_b", 
                        "Sac_c", "Sac_cp", "Sac_d", "Sac_dp", "Sac_e", "Sac_ep", "Sc_a", 
                        "Sc_ap", "Sc_as", "Sc_b", "Sc_bp", "Sc_bs", "Sc_c", "Sc_cp", 
                        "Sc_d", "Sc_e", "Sc_ep", "Sca_a", "Sca_b", "Sca_c", "Ses_a", 
                        "Ses_b", "Ses_c", "Ses_d", "Ses_e", "Ses_f", "Ses_g", "Ses_h", 
                        "Tal_a", "Tal_b", "Tal_c", "Tal_cs", "Tal_d", "Tal_e", "Td_ap", 
                        "Td_as", "Td_b", "Td_bs", "Td_c", "Td_d", "Td_e", "Td_f", "Td_fp", 
                        "Td_fs", "Td_g", "Tp_a", "Tp_b", "Tp_bp", "Tp_c", "Tp_d", "Tp_dp", 
                        "Tp_e", "Tp_f", "Tp_fp", "Ts_a", "Ts_ap", "Ts_b", "Ts_bp", "Ts_c", 
                        "Ts_cp", "Ts_d", "Ts_dp", "Ts_ds", "Ts_e", "Ts_ep", "Ts_es", 
                        "Tv_a", "Tv_ap", "Tv_as", "Tv_b", "Tv_c", "Tv_cp", "Tv_d", "Tv_dp", 
                        "Tv_ds", "Tv_e", "Tv_es", "Tv_f", "Tv_fp", "Tv_g", "Tv_gp", "Tv_h", 
                        "Tv_hp", "Tv_i", "Tv_j", "Tv_k", "Tv_l", "Tv_lp", "Ud_a", "Unc_a", 
                        "Unc_b", "Up_a", "Up_ap", "Up_b", "Up_bp", "Up_c", "Up_d", "Up_e", 
                        "Up_f", "Up_g", "Up_h", "Up_i", "Us_a", "Us_ap", "Us_b", "Us_bs", 
                        "Vest_a", "Vest_as")
  
  cutdata_coltypes <- c("numeric","guess","numeric",rep("text",8),rep("numeric",411))
  
  # list of columns and coltypes necessary for NDEData
  ndedata_colnames <- c("pk_ID", "fieldID", "SubID", "SPATIAL::USfield", 
                        "SPATIAL::Group1", "SPATIAL::Group2", "SPATIAL::Group3", "SPECIES::Taxon", 
                        "SKEL::Anat_Class", "SKEL::Anat", "SKEL::Anat_Detail", "NDE::Ldmk_17", 
                        "NDE::Ldmk_30", "NDE::Ldmk_88mesRib", "NDE::Ldmk_89mesRad", "NDE::Ldmk_90mesMan", 
                        "NDE::Ldmk_91mesFem", "NDE::Ldmk_01", "NDE::Ldmk_02", "NDE::Ldmk_03", "NDE::Ldmk_04", 
                        "NDE::Ldmk_05", "NDE::Ldmk_06", "NDE::Ldmk_07", "NDE::Ldmk_08", "NDE::Ldmk_09", "NDE::Ldmk_10", 
                        "NDE::Ldmk_11", "NDE::Ldmk_12", "NDE::Ldmk_13", "NDE::Ldmk_14", "NDE::Ldmk_15", "NDE::Ldmk_16", 
                        "NDE::Ldmk_18", "NDE::Ldmk_19", "NDE::Ldmk_20", "NDE::Ldmk_21", "NDE::Ldmk_22", "NDE::Ldmk_23", 
                        "NDE::Ldmk_24", "NDE::Ldmk_25", "NDE::Ldmk_26", "NDE::Ldmk_27", "NDE::Ldmk_28", "NDE::Ldmk_29", 
                        "NDE::Ldmk_31", "NDE::Ldmk_31a", "NDE::Ldmk_31b", "NDE::Ldmk_32", "NDE::Ldmk_33", "NDE::Ldmk_34", 
                        "NDE::Ldmk_35", "NDE::Ldmk_36", "NDE::Ldmk_37", "NDE::Ldmk_38", "NDE::Ldmk_38tymp", "NDE::Ldmk_39", 
                        "NDE::Ldmk_40", "NDE::Ldmk_40basi", "NDE::Ldmk_40cera", "NDE::Ldmk_40thyr", "NDE::Ldmk_41", 
                        "NDE::Ldmk_42", "NDE::Ldmk_43", "NDE::Ldmk_44", "NDE::Ldmk_45", "NDE::Ldmk_46", "NDE::Ldmk_47", 
                        "NDE::Ldmk_48", "NDE::Ldmk_49", "NDE::Ldmk_50", "NDE::Ldmk_51", "NDE::Ldmk_52", "NDE::Ldmk_53", 
                        "NDE::Ldmk_54", "NDE::Ldmk_55", "NDE::Ldmk_56", "NDE::Ldmk_57", "NDE::Ldmk_58", "NDE::Ldmk_59", 
                        "NDE::Ldmk_60", "NDE::Ldmk_61", "NDE::Ldmk_62", "NDE::Ldmk_63", "NDE::Ldmk_63cen", "NDE::Ldmk_63pis", 
                        "NDE::Ldmk_63rad", "NDE::Ldmk_64", "NDE::Ldmk_64cap", "NDE::Ldmk_64tra", "NDE::Ldmk_64trapd", 
                        "NDE::Ldmk_65", "NDE::Ldmk_66", "NDE::Ldmk_67", "NDE::Ldmk_68", "NDE::Ldmk_69", "NDE::Ldmk_70", 
                        "NDE::Ldmk_71", "NDE::Ldmk_71de", "NDE::Ldmk_71ds", "NDE::Ldmk_71pe", "NDE::Ldmk_71ps", 
                        "NDE::Ldmk_72", "NDE::Ldmk_73", "NDE::Ldmk_74", "NDE::Ldmk_75", "NDE::Ldmk_76", "NDE::Ldmk_77", 
                        "NDE::Ldmk_77acc", "NDE::Ldmk_77cub", "NDE::Ldmk_77nav", "NDE::Ldmk_78", "NDE::Ldmk_78first", 
                        "NDE::Ldmk_78int", "NDE::Ldmk_78med", "NDE::Ldmk_78sec", "NDE::Ldmk_79", "NDE::Ldmk_80", 
                        "NDE::Ldmk_81", "NDE::Ldmk_82", "NDE::Ldmk_83", "NDE::Ldmk_84", "NDE::Ldmk_85", "NDE::Ldmk_86", 
                        "NDE::Ldmk_87", "NDE::Ldmk_92ph1", "NDE::Ldmk_93ph2", "NDE::Ldmk_94ph3", "SKEL::AgeCort", 
                        "SKEL::AgeFus", "NDE::fk_ID")
  
  ndedata_coltypes <- c("numeric","guess","numeric",rep("guess",4),rep("text",4),rep("numeric",117),rep("text",2), "numeric")
  
  ndedata_newcolnames <- c("Group","BASE::pk_ID", "BASE::fieldID", "BASE::SubID", "SPATIAL::USfield", 
                           "SPATIAL::Group1", "SPATIAL::Group2", "SPATIAL::Group3", "SPECIES::Taxon", 
                           "SKEL::Anat_Class", "SKEL::Anat", "SKEL::Anat_Detail", "Ldmk_17", 
                           "Ldmk_30", "Ldmk_88mesRib", "Ldmk_89mesRad", "Ldmk_90mesMan", 
                           "Ldmk_91mesFem", "Ldmk_01", "Ldmk_02", "Ldmk_03", "Ldmk_04", 
                           "Ldmk_05", "Ldmk_06", "Ldmk_07", "Ldmk_08", "Ldmk_09", "Ldmk_10", 
                           "Ldmk_11", "Ldmk_12", "Ldmk_13", "Ldmk_14", "Ldmk_15", "Ldmk_16", 
                           "Ldmk_18", "Ldmk_19", "Ldmk_20", "Ldmk_21", "Ldmk_22", "Ldmk_23", 
                           "Ldmk_24", "Ldmk_25", "Ldmk_26", "Ldmk_27", "Ldmk_28", "Ldmk_29", 
                           "Ldmk_31", "Ldmk_31a", "Ldmk_31b", "Ldmk_32", "Ldmk_33", "Ldmk_34", 
                           "Ldmk_35", "Ldmk_36", "Ldmk_37", "Ldmk_38", "Ldmk_38tymp", "Ldmk_39", 
                           "Ldmk_40", "Ldmk_40basi", "Ldmk_40cera", "Ldmk_40thyr", "Ldmk_41", 
                           "Ldmk_42", "Ldmk_43", "Ldmk_44", "Ldmk_45", "Ldmk_46", "Ldmk_47", 
                           "Ldmk_48", "Ldmk_49", "Ldmk_50", "Ldmk_51", "Ldmk_52", "Ldmk_53", 
                           "Ldmk_54", "Ldmk_55", "Ldmk_56", "Ldmk_57", "Ldmk_58", "Ldmk_59", 
                           "Ldmk_60", "Ldmk_61", "Ldmk_62", "Ldmk_63", "Ldmk_63cen", "Ldmk_63pis", 
                           "Ldmk_63rad", "Ldmk_64", "Ldmk_64cap", "Ldmk_64tra", "Ldmk_64trapd", 
                           "Ldmk_65", "Ldmk_66", "Ldmk_67", "Ldmk_68", "Ldmk_69", "Ldmk_70", 
                           "Ldmk_71", "Ldmk_71de", "Ldmk_71ds", "Ldmk_71pe", "Ldmk_71ps", 
                           "Ldmk_72", "Ldmk_73", "Ldmk_74", "Ldmk_75", "Ldmk_76", "Ldmk_77", 
                           "Ldmk_77acc", "Ldmk_77cub", "Ldmk_77nav", "Ldmk_78", "Ldmk_78first", 
                           "Ldmk_78int", "Ldmk_78med", "Ldmk_78sec", "Ldmk_79", "Ldmk_80", 
                           "Ldmk_81", "Ldmk_82", "Ldmk_83", "Ldmk_84", "Ldmk_85", "Ldmk_86", 
                           "Ldmk_87", "Ldmk_92ph1", "Ldmk_93ph2", "Ldmk_94ph3", "SKEL::AgeCort", 
                           "SKEL::AgeFus")
  
  # list of ALL columns and coltypes necessary
    fragmentdata_imported_colnames <- c(speciesdata_colnames,taphodata_colnames,cutdata_colnames,ndedata_colnames)
    fragmentdata_imported_coltypes <- c(speciesdata_coltypes,taphodata_coltypes,cutdata_coltypes,ndedata_coltypes)
  
  
    excel_filename <- NULL
  
  ## extracting fragmentdata_imported = all the necessary columns for further analysis #GIGA OBSERVE !
  observe({
    selected_group <- input$selected_group
    
    if (!is.null(input$fragmentdata_file)){
    excel_filename <- input$fragmentdata_file$datapath
    }

    if (input$import_testdata == "TRUE"){
      excel_filename <- "TestData.xlsx"
    }
   
    
    if (!is.null(excel_filename)){
      dataFile <- read_excel(excel_filename, sheet=1, col_types = "text")
      dat <- data.frame(dataFile)
      df_fragmentdata(dat)
      
   # importing colnames from fragmentdata
  fragmentdata_colnames <- colnames(read_excel(excel_filename, sheet=1, col_types = "text", n_max = 0))
  
  # selecting columns for fragmentdata_imported
  fragmentdata_importedcol <- match(fragmentdata_colnames, fragmentdata_imported_colnames)
  fragmentdata_importedcol <- fragmentdata_imported_coltypes[fragmentdata_importedcol]
  fragmentdata_importedcol[is.na(fragmentdata_importedcol)] <- "skip"

  # importing the appropriate columns with the good column types
  fragmentdata_imported <- read_excel(excel_filename, sheet=1, col_types = fragmentdata_importedcol)
  
    ## group definition
  fragmentdata_imported$Group <- fragmentdata_imported[,selected_group]
  fragmentdata_imported$Group[is.na(fragmentdata_imported$Group)] <- "Unknown"  
  
  df_fragmentdata_imported(fragmentdata_imported)
  
  ### creating group data summary
  groupdata <- as.matrix(df_fragmentdata_imported())
  groupdata <- table(groupdata[,"Group"], exclude = NULL)
  groupdata <- addmargins(groupdata)
  groupdata <- as.data.frame(groupdata)
  colnames(groupdata) <- c("Group","Number of records")
  # rownames(groupdata) <- groupdata$Group
  df_groupdata(groupdata)

  
  ### creating speciesdata, cutdata etc
  
  ##SPECIES
  # selecting columns
  speciesdata <- fragmentdata_imported[,c("Group",speciesdata_colnames)]
  # removing empty rows, and dropping the useless fk_ID column
  speciesdata <- subset(speciesdata, subset = `SPECIES::fk_ID`!="NA", select = -`SPECIES::fk_ID`)
  
  df_speciesdata(speciesdata)
  
  
  ##TAPHO
  # selecting columns
  taphodata <- fragmentdata_imported[,c("Group",taphodata_colnames)]
  
  # removing empty rows, and dropping the useless fk_ID column
  taphodata <- subset(taphodata, subset = `TAPHO::fk_ID`!="NA", select = -`TAPHO::fk_ID`)
  
  # correcting fields that should not have been recorded (cf. manual)
  tmpNotLBN <- taphodata$`SKEL::Anat_Class`!="LBN"
  tmpTeeth <- taphodata$`SKEL::Anat`=="TTH"
  tmpCarbo <- taphodata$Burnt>=2
  taphodata$`TAPHO::Fract1`[tmpNotLBN] <- NA
  taphodata$`TAPHO::Fract2`[tmpNotLBN] <- NA
  taphodata$`TAPHO::Fract1`[tmpCarbo] <- NA
  taphodata$`TAPHO::Fract2`[tmpCarbo] <- NA
  taphodata$`TAPHO::Crack`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::Crack`[tmpCarbo] <- "NA"
  taphodata$`TAPHO::Sheet`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::Sheet`[tmpCarbo] <- "NA"
  taphodata$`TAPHO::Black`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::Black`[tmpCarbo] <- "NA"
  taphodata$`TAPHO::DenD`[tmpCarbo] <- "NA"
  taphodata$`TAPHO::P_scar`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::P_oppo`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::P_cort`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::P_flake`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::P_peel`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::P_spong`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::C_cren`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::C_scoop`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::Rodent`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::Read`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::Exfo`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::Exfo`[tmpCarbo] <- "NA"
  taphodata$`TAPHO::C_pit`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::C_punct`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::C_scor`[tmpTeeth] <- "NA"
  taphodata$`TAPHO::C_furr`[tmpTeeth] <- "NA"
  rm(tmpNotLBN)
  rm(tmpTeeth)
  rm(tmpCarbo)
  
  df_taphodata(taphodata)
  
  
  ##CUT
  # selecting columns
  cutdata <- fragmentdata_imported[,c("Group",cutdata_colnames)]
  # removing empty rows (fkID is preserved in this case)
  cutdata <- subset(cutdata, subset = `CUT::fk_ID`!="NA")
  # renaming columns removing the CUT::
  colnames(cutdata) <- cutdata_newcolnames
  
  df_cutdata(cutdata)
  
  ##NDE
  # selecting columns
  ndedata <- fragmentdata_imported[,c("Group",ndedata_colnames)]
  # removing empty rows, and dropping the useless fk_ID column
  ndedata <- subset(ndedata, subset = `NDE::fk_ID`!="NA", select = -`NDE::fk_ID`)
  # renaming columns removing the NDE::
  colnames(ndedata) <- ndedata_newcolnames
  
  df_ndedata(ndedata)
  
  
    }
  })
  
  
  
  # rendering imported data table
  output$groupdata_table = DT::renderDT({
    df_groupdata()
  }, options = list(pageLength = 50, scrollX = TRUE))
  
  output$fragmentdata_table = DT::renderDT({
   df_fragmentdata()
  }, options = list(pageLength = 50, scrollX = TRUE))
  
  output$fragmentdata_imported_table = DT::renderDT({
    df_fragmentdata_imported()
  }, options = list(pageLength = 50, scrollX = TRUE))
  
  output$speciesdata_table = DT::renderDT({
    df_speciesdata()
  }, options = list(pageLength = 50, scrollX = TRUE))
  
  output$taphodata_table = DT::renderDT({
    df_taphodata()
  }, options = list(pageLength = 50, scrollX = TRUE))
  
  output$cutdata_table = DT::renderDT({
    df_cutdata()
  }, options = list(pageLength = 50, scrollX = TRUE))
  
  output$ndedata_table = DT::renderDT({
    df_ndedata()
  }, options = list(pageLength = 50, scrollX = TRUE))
  
  
  
  ################################ SERVER: FILTERING THE DATASET WITH THE SIDEBAR #################################################################
  
  choices<-reactiveValues(group=NULL,
                          taxa=NULL)
  
  
  #creating the list of groups
  observe({
  if (!is.null(df_fragmentdata_imported())){
    data <- df_fragmentdata_imported()
    
    choices$group <- (rownames(table(data$Group)))
    updatePickerInput(session, "list_sidebar_group",
                      choices = choices$group,
                      selected = choices$group)
  }
    })
  
  #creating the list of taxa
  observe({
    if (!is.null(df_fragmentdata_imported())){
      data <- df_fragmentdata_imported()

      choices$taxa <- (rownames(table(data[,"SPECIES::Taxon"])))
      updatePickerInput(session, "list_sidebar_taxa",
                        choices = choices$taxa,
                        selected = choices$taxa)
      
      updateSelectInput(session, "species_plot_selected",
                        choices = choices$taxa)
      
    
    }
  })
  
  
  df_speciesdata_filtered<-reactiveVal(NULL)
  df_taphodata_filtered<-reactiveVal(NULL)
  df_cutdata_filtered<-reactiveVal(NULL)
  df_ndedata_filtered<-reactiveVal(NULL)
  
  
  observe({
    if(!is.null(df_speciesdata())){
      data <- df_speciesdata()
      group_filter <- input$list_sidebar_group
      taxa_filter <- input$list_sidebar_taxa
      data <- as_tibble(data) %>% mutate(across(everything(), unlist)) #necessary to remove the named list for Group!!! otherwise the filter does not work on Group...
      df_filt <- data %>% filter(Group %in% group_filter, `SPECIES::Taxon` %in% taxa_filter)
      df_speciesdata_filtered(df_filt)
    }
  })
  
  observe({
    if(!is.null(df_taphodata())){
      data <- df_taphodata()
      group_filter <- input$list_sidebar_group
      taxa_filter <- input$list_sidebar_taxa
      data <- as_tibble(data) %>% mutate(across(everything(), unlist)) #necessary to remove the named list for Group!!! otherwise the filter does not work on Group...
      df_filt <- data %>% filter(Group %in% group_filter, `SPECIES::Taxon` %in% taxa_filter)
      df_taphodata_filtered(df_filt)
    }
  })

  observe({
    if(!is.null(df_cutdata())){
      data <- df_cutdata()
      group_filter <- input$list_sidebar_group
      taxa_filter <- input$list_sidebar_taxa
      data <- as_tibble(data) %>% mutate(across(everything(), unlist)) #necessary to remove the named list for Group!!! otherwise the filter does not work on Group...
      df_filt <- data %>% filter(Group %in% group_filter, `SPECIES::Taxon` %in% taxa_filter)
      df_cutdata_filtered(df_filt)
    }
  })

  observe({
    if(!is.null(df_ndedata())){
      data <- df_ndedata()
      group_filter <- input$list_sidebar_group
      taxa_filter <- input$list_sidebar_taxa
      data <- as_tibble(data) %>% mutate(across(everything(), unlist)) #necessary to remove the named list for Group!!! otherwise the filter does not work on Group...
      df_filt <- data %>% filter(Group %in% group_filter, `SPECIES::Taxon` %in% taxa_filter)
      df_ndedata_filtered(df_filt)
    }
  })

  

 
  ###################################################### SERVER: SPECIES ########################################################################
  
  df_speciestable<-reactiveVal(NULL)
  
  
  #equivalent to dataprep species
  
  df_speciestable <- reactive({
    req(!is.null(df_speciesdata_filtered()))
    req(!is.null(input$list_sidebar_group))
    req(!is.null(input$list_sidebar_taxa))
    
    dataset <- as.matrix(df_speciesdata_filtered())
    SumGroup <- table(dataset[,"Group"], exclude = NULL)
    SumGroup.margins <- addmargins(SumGroup)

    SpeciesNISP<-addmargins(table(dataset[,"Group"], dataset[,"SPECIES::Taxon"], exclude = NULL))
    x <- rownames(SpeciesNISP) #used for weird names

    SpeciesNISP<-data.frame(cbind(t(SpeciesNISP)))
    colnames(SpeciesNISP)<-x #used for weird names

    # importing species latin and common names
    SpeciesCodes <- read.csv(paste0("ref_data/", input$species_codes), header=TRUE, sep=";", row.names=NULL, encoding="UTF-8")

    SpeciesNISP$SpeciesCode<-row.names(SpeciesNISP)

    tmp <- merge.data.frame(SpeciesCodes, SpeciesNISP, by.x = "SpeciesCode", all.y = TRUE, sort = FALSE)
    rownames(tmp) <- tmp$SpeciesCode
    Species <- tmp[,-1]

    # df_speciestable(Species)
    return(Species)
  })

# output of the table for the UI
  output$species_table = DT::renderDT({
      datatable(df_speciestable(), options = list(pageLength = 50, scrollX = TRUE))
  },options = list(scrollX = TRUE))

  
  
  
  # # Exporting
  output$export_species_table <- downloadHandler(
    filename = function() {
      paste(site_name(),"_Species_PivotTable.xlsx")
    },
    content = function(file) {
      req(!is.null(df_speciestable()))
      write.xlsx(df_speciestable(), file, sheetName = "Species", rowNames = TRUE, colNames = TRUE)
    }
  )
  
  
  
  ####species figure
  plot_Species <- function(taxon)
  {
    req(!is.null(df_speciesdata_filtered()))
    req(!is.null(input$list_sidebar_group))
    req(!is.null(input$list_sidebar_taxa))
    
    dataset <- as.matrix(df_speciesdata_filtered())
    SumGroup <- table(dataset[,"Group"], exclude = NULL)
    SumGroup.margins <- addmargins(SumGroup)
    
    SpeciesNISP<-addmargins(table(dataset[,"Group"], dataset[,"SPECIES::Taxon"], exclude = NULL))
    
    dataforplot<- cbind(SpeciesNISP[,"Sum"],
                        BinomCI(SpeciesNISP[,taxon],
                                SpeciesNISP[,"Sum"],
                                conf.level = 0.95,
                                method = "wilson"))
    colnames(dataforplot) <- c("N", "p","low","up")

    data = data.frame(Group = row.names(dataforplot), dataforplot)
    
    # print(data)
    plot <- ggplot(data, aes(x=Group, y=p, ymin=0, ymax=1)) + 
      geom_errorbar(aes(ymin=low, ymax=up),
                    width=.15,                    # Width of the error helpbars
                    position=position_dodge(.9)) +
      geom_point(shape=21, fill="white", size =1.5, stroke=0.8, stat="identity") +
      xlab(NULL) +
      ylab(NULL) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      ggtitle(paste(taxon, "%NISP")) +
      theme(plot.title = element_text(size=10))
    return(plot)
  }
  
  species_plot_reactive <-  reactive({
    plot_Species(input$species_plot_selected)
  })
  
  
  # output of the table for the UI
  output$species_plot = renderPlot({
    species_plot_reactive()
    })
  
  ### SAVING species plot
  output$saveplot_species <- downloadHandler(
    filename = paste0("plot_species_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file){
      ggsave(species_plot_reactive(),filename=file, device = "pdf")
    },
  )
  
  ################################ SERVER: TAPHO TABLES#################################################################
  #creating all the tapho tables
  dftapho<-reactiveValues(TaphoAccu_n=NULL,
                          TaphoAccu_Ntot=NULL,
                          TaphoAccu_p=NULL,
                          TaphoNat_n=NULL,
                          TaphoNat_Ntot=NULL,
                          TaphoNat_p=NULL,
                          dfTaphoAccu_n=NULL,
                          dfTaphoAccu_Ntot=NULL,
                          dfTaphoAccu_p=NULL,
                          dfTaphoNat_n=NULL,
                          dfTaphoNat_Ntot=NULL,
                          dfTaphoNat_p=NULL,
                          TableFract=NULL,
                          TableConcType=NULL,
                          TableBoneColor=NULL,
                          raw_dataset=NULL,
                          selected_group=NULL,
                          selected_taxa=NULL,
                          selected_records=NULL)
  
  
  
  dataprep_tapho <- function(dataset)
  {
    # creating SumGroup
    SumGroup <- table(dataset[,"Group"], exclude = NULL)
    SumGroup.margins <- addmargins(SumGroup)

    # creating TaphoAccu_n
    TaphoAccu_n <- matrix(nrow = nrow(SumGroup),ncol = 27)
    colnames(TaphoAccu_n) <- c("Cut","Scrape","Chop","Ret","BoneTool","Burnt","Ptot","P_scar","P_oppo","P_mark","P_flake","P_cort","P_peel","P_spong","P_tooth","Ctot","C_pit","C_punct","C_scor","C_furr","C_cren","C_scoop","Dig","Rodent","CutUnsure","RetUnsure","BoneToolUnsure")
    rownames(TaphoAccu_n) <- rownames(SumGroup)
    
    # creating TaphoAccu_Ntot
    TaphoAccu_Ntot <- matrix(nrow = nrow(SumGroup),ncol = 27)
    colnames(TaphoAccu_Ntot) <- c("Cut","Scrape","Chop","Ret","BoneTool","Burnt","Ptot","P_scar","P_oppo","P_mark","P_flake","P_cort","P_peel","P_spong","P_tooth","Ctot","C_pit","C_punct","C_scor","C_furr","C_cren","C_scoop","Dig","Rodent","CutUnsure","RetUnsure","BoneToolUnsure")
    rownames(TaphoAccu_Ntot) <- rownames(SumGroup)
    
    # creating TaphoAccu_p
    TaphoAccu_p <- matrix(nrow = nrow(SumGroup),ncol = 27)
    colnames(TaphoAccu_p) <- c("Cut","Scrape","Chop","Ret","BoneTool","Burnt","Ptot","P_scar","P_oppo","P_mark","P_flake","P_cort","P_peel","P_spong","P_tooth","Ctot","C_pit","C_punct","C_scor","C_furr","C_cren","C_scoop","Dig","Rodent","CutUnsure","RetUnsure","BoneToolUnsure")
    rownames(TaphoAccu_p) <- rownames(SumGroup)
    
    # creating TaphoNat_n
    TaphoNat_n <- matrix(nrow = nrow(SumGroup),ncol = 25)
    colnames(TaphoNat_n) <- c("Exfo","Sheet1","Sheet2","Crack1","Crack2","Abra1","Abra2","Black1","Black2","Black3","DenD","DenE","CirE","Cor1","Cor2","Conc1","Conc2","Conc3","Tramp","TrampUnsure","ReadNA","Read1","Read2","Read3","Read4")
    rownames(TaphoNat_n) <- rownames(SumGroup)
    
    # creating TaphoNat_Ntot
    TaphoNat_Ntot <- matrix(nrow = nrow(SumGroup),ncol = 25)
    colnames(TaphoNat_Ntot) <- c("Exfo","Sheet1","Sheet2","Crack1","Crack2","Abra1","Abra2","Black1","Black2","Black3","DenD","DenE","CirE","Cor1","Cor2","Conc1","Conc2","Conc3","Tramp","TrampUnsure","ReadNA","Read1","Read2","Read3","Read4")
    rownames(TaphoNat_Ntot) <- rownames(SumGroup)
    
    # creating TaphoNat_p
    TaphoNat_p <- matrix(nrow = nrow(SumGroup),ncol = 25)
    colnames(TaphoNat_p) <- c("Exfo","Sheet1","Sheet2","Crack1","Crack2","Abra1","Abra2","Black1","Black2","Black3","DenD","DenE","CirE","Cor1","Cor2","Conc1","Conc2","Conc3","Tramp","TrampUnsure","ReadNA","Read1","Read2","Read3","Read4")
    rownames(TaphoNat_p) <- rownames(SumGroup)
    
    # functions for _n
    persosum_n1to4 <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("1" %in% colnames(persotable), tmp<-(tmp + persotable[,c("1")]), tmp<-tmp)
      ifelse("2" %in% colnames(persotable), tmp<-(tmp + persotable[,c("2")]), tmp<-tmp)
      ifelse("3" %in% colnames(persotable), tmp<-(tmp + persotable[,c("3")]), tmp<-tmp)
      ifelse("4" %in% colnames(persotable), tmp<-(tmp + persotable[,c("4")]), tmp<-tmp)
      return(tmp)
    }
    
    persosum_n0only <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("0" %in% colnames(persotable), tmp<-(tmp + persotable[,c("0")]), tmp<-tmp)
      return(tmp)
    }
    
    persosum_n1only <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("1" %in% colnames(persotable), tmp<-(tmp + persotable[,c("1")]), tmp<-tmp)
      return(tmp)
    }
    
    persosum_n2only <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("2" %in% colnames(persotable), tmp<-(tmp + persotable[,c("2")]), tmp<-tmp)
      return(tmp)
    }
    
    persosum_n3only <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("3" %in% colnames(persotable), tmp<-(tmp + persotable[,c("3")]), tmp<-tmp)
      return(tmp)
    }
    
    persosum_n4only <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("4" %in% colnames(persotable), tmp<-(tmp + persotable[,c("4")]), tmp<-tmp)
      return(tmp)
    }
    
    persosum_nUnsure <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("?" %in% colnames(persotable), tmp<-(tmp + persotable[,c("?")]), tmp<-tmp)
      return(tmp)
    }
    
    # functions for _Ntot
    persosum_Ntot0to4sure <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("0" %in% colnames(persotable), tmp<-(tmp + persotable[,c("0")]), tmp<-tmp)
      ifelse("1" %in% colnames(persotable), tmp<-(tmp + persotable[,c("1")]), tmp<-tmp)
      ifelse("2" %in% colnames(persotable), tmp<-(tmp + persotable[,c("2")]), tmp<-tmp)
      ifelse("3" %in% colnames(persotable), tmp<-(tmp + persotable[,c("3")]), tmp<-tmp)
      ifelse("4" %in% colnames(persotable), tmp<-(tmp + persotable[,c("4")]), tmp<-tmp)
      return(tmp)
    }
    
    persosum_Ntot0to4unsure <- function(group, var)
    {
      persotable <- table(group, var, exclude = NULL)
      tmp <- rep(0,nrow(SumGroup))
      ifelse("0" %in% colnames(persotable), tmp<-(tmp + persotable[,c("0")]), tmp<-tmp)
      ifelse("1" %in% colnames(persotable), tmp<-(tmp + persotable[,c("1")]), tmp<-tmp)
      ifelse("2" %in% colnames(persotable), tmp<-(tmp + persotable[,c("2")]), tmp<-tmp)
      ifelse("3" %in% colnames(persotable), tmp<-(tmp + persotable[,c("3")]), tmp<-tmp)
      ifelse("4" %in% colnames(persotable), tmp<-(tmp + persotable[,c("4")]), tmp<-tmp)
      ifelse("?" %in% colnames(persotable), tmp<-(tmp + persotable[,c("?")]), tmp<-tmp)
      return(tmp)
    }
    
    dataset <- as.data.frame(dataset)
    
    # populating TaphoAccu_n
    TaphoAccu_n[,"BoneTool"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::BoneTool`)  
    TaphoAccu_n[,"C_cren"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_cren`) 
    TaphoAccu_n[,"C_furr"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_furr`) 
    TaphoAccu_n[,"C_pit"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_pit`) 
    TaphoAccu_n[,"C_punct"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_punct`) 
    TaphoAccu_n[,"C_scoop"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_scoop`) 
    TaphoAccu_n[,"C_scor"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_scor`) 
    TaphoAccu_n[,"Chop"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Chop`) 
    TaphoAccu_n[,"P_cort"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_cort`) 
    TaphoAccu_n[,"P_flake"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_flake`) 
    TaphoAccu_n[,"P_tooth"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_tooth`) 
    TaphoAccu_n[,"P_scar"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_scar`) 
    TaphoAccu_n[,"P_mark"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_mark`) 
    TaphoAccu_n[,"P_oppo"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_oppo`) 
    TaphoAccu_n[,"P_peel"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_peel`) 
    TaphoAccu_n[,"P_spong"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_spong`) 
    TaphoAccu_n[,"Rodent"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Rodent`) 
    TaphoAccu_n[,"Scrape"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Scrape`) 
    TaphoAccu_n[,"Dig"]<-persosum_n3only(dataset$`Group`, dataset$`TAPHO::CorDig`)
    TaphoAccu_n[,"Ctot"] <-persosum_n1only(dataset$`Group`, dataset$`TAPHO::C_SUM`) 
    TaphoAccu_n[,"Ptot"] <-persosum_n1only(dataset$`Group`, dataset$`TAPHO::P_SUM`) 
    TaphoAccu_n[,"Cut"] <-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Cut`) 
    TaphoAccu_n[,"Ret"] <-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Ret`) 
    TaphoAccu_n[,"Burnt"]<-persosum_n1to4(dataset$`Group`, dataset$`Burnt`)
    TaphoAccu_n[,"CutUnsure"] <-persosum_nUnsure(dataset$`Group`, dataset$`TAPHO::Cut`)
    TaphoAccu_n[,"RetUnsure"] <-persosum_nUnsure(dataset$`Group`, dataset$`TAPHO::Ret`) 
    TaphoAccu_n[,"BoneToolUnsure"] <-persosum_nUnsure(dataset$`Group`, dataset$`TAPHO::BoneTool`) 
    TaphoAccu_n <- replace(TaphoAccu_n, is.na(TaphoAccu_n),0)
    
    # populating TaphoNat_n
    TaphoNat_n[,"Exfo"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Exfo`)
    TaphoNat_n[,"DenD"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::DenD`)
    TaphoNat_n[,"DenE"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::DenE`)
    TaphoNat_n[,"Sheet1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Sheet`)
    TaphoNat_n[,"Sheet2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Sheet`)
    TaphoNat_n[,"Abra1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Abra`)
    TaphoNat_n[,"Abra2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Abra`)
    TaphoNat_n[,"CirE"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::CirE`)
    TaphoNat_n[,"Crack1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Crack`)
    TaphoNat_n[,"Crack2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Crack`)
    TaphoNat_n[,"Conc1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Conc`)
    TaphoNat_n[,"Conc2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Conc`)
    TaphoNat_n[,"Conc3"] <- persosum_n3only(dataset$`Group`, dataset$`TAPHO::Conc`)
    TaphoNat_n[,"Black1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Black`)
    TaphoNat_n[,"Black2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::Black`)
    TaphoNat_n[,"Black3"] <- persosum_n3only(dataset$`Group`, dataset$`TAPHO::Black`)
    TaphoNat_n[,"Tramp"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::Tramp`)
    TaphoNat_n[,"TrampUnsure"] <- persosum_nUnsure(dataset$`Group`, dataset$`TAPHO::Tramp`)
    TaphoNat_n[,"Cor1"] <- persosum_n1only(dataset$`Group`, dataset$`TAPHO::CorDig`)
    TaphoNat_n[,"Cor2"] <- persosum_n2only(dataset$`Group`, dataset$`TAPHO::CorDig`)
    TaphoNat_n[,"ReadNA"]<-persosum_n0only(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_n[,"Read1"]<-persosum_n1only(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_n[,"Read2"]<-persosum_n2only(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_n[,"Read3"]<-persosum_n3only(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_n[,"Read4"]<-persosum_n4only(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_n <- replace(TaphoNat_n, is.na(TaphoNat_n),0)
    
    # populating TaphoAccu_Ntot
    TaphoAccu_Ntot[,"BoneTool"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::BoneTool`)  
    TaphoAccu_Ntot[,"C_cren"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_cren`) 
    TaphoAccu_Ntot[,"C_furr"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_furr`) 
    TaphoAccu_Ntot[,"C_pit"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_pit`) 
    TaphoAccu_Ntot[,"C_punct"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_punct`) 
    TaphoAccu_Ntot[,"C_scoop"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_scoop`) 
    TaphoAccu_Ntot[,"C_scor"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_scor`) 
    TaphoAccu_Ntot[,"Chop"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Chop`) 
    TaphoAccu_Ntot[,"P_cort"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_cort`) 
    TaphoAccu_Ntot[,"P_flake"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_flake`) 
    TaphoAccu_Ntot[,"P_tooth"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_tooth`) 
    TaphoAccu_Ntot[,"P_scar"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_scar`) 
    TaphoAccu_Ntot[,"P_mark"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_mark`) 
    TaphoAccu_Ntot[,"P_oppo"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_oppo`) 
    TaphoAccu_Ntot[,"P_peel"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_peel`) 
    TaphoAccu_Ntot[,"P_spong"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_spong`) 
    TaphoAccu_Ntot[,"Rodent"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Rodent`) 
    TaphoAccu_Ntot[,"Scrape"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Scrape`) 
    TaphoAccu_Ntot[,"Dig"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::CorDig`)
    TaphoAccu_Ntot[,"Ctot"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::C_SUM`) 
    TaphoAccu_Ntot[,"Ptot"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::P_SUM`) 
    TaphoAccu_Ntot[,"Cut"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Cut`) 
    TaphoAccu_Ntot[,"Ret"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Ret`) 
    TaphoAccu_Ntot[,"Burnt"] <-persosum_Ntot0to4sure(dataset$`Group`, dataset$`Burnt`) 
    TaphoAccu_Ntot[,"CutUnsure"] <-persosum_Ntot0to4unsure(dataset$`Group`, dataset$`TAPHO::Cut`) 
    TaphoAccu_Ntot[,"RetUnsure"] <-persosum_Ntot0to4unsure(dataset$`Group`, dataset$`TAPHO::Ret`)
    TaphoAccu_Ntot[,"BoneToolUnsure"] <-persosum_Ntot0to4unsure(dataset$`Group`, dataset$`TAPHO::BoneTool`) 
    TaphoAccu_Ntot <- replace(TaphoAccu_Ntot, is.na(TaphoAccu_Ntot),0)
    
    # populating TaphoNat_Ntot
    TaphoNat_Ntot[,"Exfo"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Exfo`)
    TaphoNat_Ntot[,"DenD"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::DenD`)
    TaphoNat_Ntot[,"DenE"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::DenE`)
    TaphoNat_Ntot[,"Sheet1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Sheet`)
    TaphoNat_Ntot[,"Sheet2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Sheet`)
    TaphoNat_Ntot[,"Abra1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Abra`)
    TaphoNat_Ntot[,"Abra2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Abra`)
    TaphoNat_Ntot[,"CirE"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::CirE`)
    TaphoNat_Ntot[,"Crack1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Crack`)
    TaphoNat_Ntot[,"Crack2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Crack`)
    TaphoNat_Ntot[,"Conc1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Conc`)
    TaphoNat_Ntot[,"Conc2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Conc`)
    TaphoNat_Ntot[,"Conc3"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Conc`)
    TaphoNat_Ntot[,"Black1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Black`)
    TaphoNat_Ntot[,"Black2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Black`)
    TaphoNat_Ntot[,"Black3"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Black`)
    TaphoNat_Ntot[,"Tramp"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Tramp`)
    TaphoNat_Ntot[,"TrampUnsure"] <- persosum_Ntot0to4unsure(dataset$`Group`, dataset$`TAPHO::Tramp`)
    TaphoNat_Ntot[,"Cor1"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::CorDig`)
    TaphoNat_Ntot[,"Cor2"] <- persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::CorDig`)
    TaphoNat_Ntot[,"ReadNA"]<-persosum_Ntot0to4sure(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_Ntot[,"Read1"]<-persosum_n1to4(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_Ntot[,"Read2"]<-persosum_n1to4(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_Ntot[,"Read3"]<-persosum_n1to4(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_Ntot[,"Read4"]<-persosum_n1to4(dataset$`Group`, dataset$`TAPHO::Read`)
    TaphoNat_Ntot <- replace(TaphoNat_Ntot, is.na(TaphoNat_Ntot),0)
    
    # adding margins and populating _p
    TaphoAccu_n <- addmargins(TaphoAccu_n, 1)
    TaphoAccu_Ntot <- addmargins(TaphoAccu_Ntot, 1)
    TaphoAccu_p <- TaphoAccu_n / TaphoAccu_Ntot
    TaphoNat_n <- addmargins(TaphoNat_n, 1)
    TaphoNat_Ntot <- addmargins(TaphoNat_Ntot, 1)
    TaphoNat_p <- TaphoNat_n / TaphoNat_Ntot
    
    # translating and creating data frames
    dfTaphoAccu_n <- data.frame(t(TaphoAccu_n), check.rows = FALSE, check.names = FALSE)
    dfTaphoAccu_Ntot <- data.frame(t(TaphoAccu_Ntot), check.rows = FALSE, check.names = FALSE)
    dfTaphoAccu_p <- data.frame(t(TaphoAccu_p), check.rows = FALSE, check.names = FALSE)
    dfTaphoNat_n <- data.frame(t(TaphoNat_n), check.rows = FALSE, check.names = FALSE)
    dfTaphoNat_Ntot <- data.frame(t(TaphoNat_Ntot), check.rows = FALSE, check.names = FALSE)
    dfTaphoNat_p <- data.frame(t(TaphoNat_p), check.rows = FALSE, check.names = FALSE)
    
    # saving colnames for weird group charac
    colnames(dfTaphoAccu_n) <- rownames(TaphoAccu_n)
    colnames(dfTaphoAccu_Ntot) <- rownames(TaphoAccu_Ntot)
    colnames(dfTaphoAccu_p) <- rownames(TaphoAccu_p)
    colnames(dfTaphoNat_n) <- rownames(TaphoNat_n)
    colnames(dfTaphoNat_Ntot) <- rownames(TaphoNat_Ntot)
    colnames(dfTaphoNat_p) <- rownames(TaphoNat_p)
    
    
    # function for calculation of fractures
    persotableFract <- function(group)
    {
      library(dplyr)
      tableFract1<-table(group, dataset$`TAPHO::Fract1`, exclude = NULL)
      tableFract2<-table(group, dataset$`TAPHO::Fract2`, exclude = NULL)
      Green<-addmargins(cbind(tableFract1[,grep("Green",colnames(tableFract1))],tableFract2[,grep("Green",colnames(tableFract2))]))
      Dry<-addmargins(cbind(tableFract1[,grep("Dry",colnames(tableFract1))],tableFract2[,grep("Dry",colnames(tableFract2))]))
      Recent<-addmargins(cbind(tableFract1[,grep("Recent",colnames(tableFract1))],tableFract2[,grep("Recent",colnames(tableFract2))]))
      TOT<-cbind(Green[,"Sum"],Dry[,"Sum"],Recent[,"Sum"])
      TOT<-addmargins(TOT,2)
      TOT<-cbind(TOT,TOT[,1]/(TOT[,1]+TOT[,2]))
      colnames(TOT) <- c("Green","Dry","Recent","Sum","G/(G+D)")
      return(data.frame(TOT, check.rows = FALSE, check.names = FALSE))
    }
    
    # creating the fracture table
    TableFract <- persotableFract(dataset$`Group`)
    
    # creating the conctype table
    TableConcType <- data.frame(cbind(addmargins(table(dataset$`Group`, dataset$`TAPHO::ConcType`, exclude = NA))), check.rows = FALSE, check.names = FALSE)
    
    # creating the BoneColor table
    TableBoneColor <- data.frame(cbind(addmargins(table(dataset$`Group`, dataset$`BoneColor`, exclude = NA))), check.rows = FALSE, check.names = FALSE)
    
    
    # creating the final list
    #Tapholist <- list(TaphoAccu_n, TaphoAccu_Ntot, TaphoAccu_p, TaphoNat_n,TaphoNat_Ntot, TaphoNat_p, data.frame(dfTaphoAccu_n), data.frame(dfTaphoAccu_Ntot), data.frame(dfTaphoAccu_p), data.frame(dfTaphoNat_n), data.frame(dfTaphoNat_Ntot), data.frame(dfTaphoNat_p), data.frame(TableFract), data.frame(TableConcType), data.frame(TableBoneColor), dataset, all.names = TRUE)
    
    Tapholist <- list(TaphoAccu_n, TaphoAccu_Ntot, TaphoAccu_p, TaphoNat_n,TaphoNat_Ntot, TaphoNat_p, dfTaphoAccu_n, dfTaphoAccu_Ntot, dfTaphoAccu_p, dfTaphoNat_n, dfTaphoNat_Ntot, dfTaphoNat_p, TableFract, TableConcType, TableBoneColor, dataset, all.names = TRUE)
    # tapholist edited to remove dataframe for weird names
    
    names(Tapholist) <- c("TaphoAccu_n","TaphoAccu_Ntot","TaphoAccu_p", "TaphoNat_n","TaphoNat_Ntot", "TaphoNat_p","dfTaphoAccu_n","dfTaphoAccu_Ntot", "dfTaphoAccu_p","dfTaphoNat_n","dfTaphoNat_Ntot", "dfTaphoNat_p", "TableFract", "TableConcType", "TableBoneColor", "raw_dataset")
    
    return(Tapholist)
    
  }
  
  observeEvent(input$button_tapho, {
    req(!is.null(df_taphodata_filtered()))
    req(!is.null(input$list_sidebar_group))
    req(!is.null(input$list_sidebar_taxa))
    req(nrow(df_taphodata_filtered())>0)
    
    # validate(need(nrow(df_taphodata_filtered())>0, "NOOOOO"))
    
    # if(nrow(df_taphodata_filtered())>0)){
    #   
   
    dataset <- dataprep_tapho(as.matrix(df_taphodata_filtered()))
    
  #recording the sidebar selections at the time of df creation
    dftapho$selected_group <- input$list_sidebar_group
    dftapho$selected_taxa <- input$list_sidebar_taxa
    dftapho$selected_records <- nrow(df_taphodata_filtered())
    
  #part included in order to change the list of selected groups and taxa to all if necessary
    if(length(dftapho$selected_group) == length(choices$group)){
    dftapho$selected_group <- "all groups"
    }
    if(length(dftapho$selected_taxa) == length(choices$taxa)){
      dftapho$selected_taxa <- "all taxa"
    }
    
    dftapho$TaphoAccu_n <- dataset[["TaphoAccu_n"]]
    dftapho$TaphoAccu_Ntot <- dataset[["TaphoAccu_Ntot"]]
    dftapho$TaphoAccu_p <- dataset[["TaphoAccu_p"]]
    dftapho$TaphoNat_n <- dataset[["TaphoNat_n"]]
    dftapho$TaphoNat_Ntot <- dataset[["TaphoNat_Ntot"]]
    dftapho$TaphoNat_p <- dataset[["TaphoNat_p"]]
    dftapho$dfTaphoAccu_n <- dataset[["dfTaphoAccu_n"]]
    dftapho$dfTaphoAccu_Ntot <- dataset[["dfTaphoAccu_Ntot"]]
    dftapho$dfTaphoAccu_p <- dataset[["dfTaphoAccu_p"]]
    dftapho$dfTaphoNat_n <- dataset[["dfTaphoNat_n"]]
    dftapho$dfTaphoNat_Ntot <- dataset[["dfTaphoNat_Ntot"]]
    dftapho$dfTaphoNat_p <- dataset[["dfTaphoNat_p"]]
    dftapho$TableFract <- dataset[["TableFract"]]
    dftapho$TableConcType <- dataset[["TableConcType"]]
    dftapho$TableBoneColor <- dataset[["TableBoneColor"]]
    dftapho$raw_dataset <- dataset[["raw_dataset"]]
    # }# end of if
  })

  # output info for selected records
  output$tapho_selected_group_text<- renderText({
    ifelse(!is.null(dftapho$selected_group),paste("You are currently analysing the following selection of groups:",paste(dftapho$selected_group, collapse = ", ")),"Zero group under tapho analysis")
  })
  
  output$tapho_selected_taxa_text<- renderText({
    ifelse(!is.null(dftapho$selected_taxa),paste("You are currently analysing the following selection of taxa:",paste(dftapho$selected_taxa, collapse = ", ")),"Zero taxon under tapho analysis")
  })
  
  output$tapho_selected_records_text<- renderText({
    ifelse(!is.null(dftapho$selected_records),paste("You are currently analysing:",paste(dftapho$selected_records), "records"),"Zero record under tapho analysis")
  })

  
  
  
  # output table taphos
  output$table_dfTaphoAccu_n <- DT::renderDataTable({
    req(!is.null(dftapho$dfTaphoAccu_n))
    
    datatable(dftapho$dfTaphoAccu_n, options = list(pageLength = 50, scrollX = TRUE))
  })
  
  output$table_dfTaphoAccu_Ntot <- DT::renderDataTable({
    req(!is.null(dftapho$dfTaphoAccu_Ntot))
    
    datatable(dftapho$dfTaphoAccu_Ntot, options = list(pageLength = 50, scrollX = TRUE))
  })
  
  output$table_dfTaphoAccu_p <- DT::renderDataTable({
    req(!is.null(dftapho$dfTaphoAccu_p))
    
    datatable(dftapho$dfTaphoAccu_p, options = list(pageLength = 50, scrollX = TRUE))    %>% formatPercentage(names(dftapho$dfTaphoAccu_p))
    
  })
  
  output$table_dfTaphoNat_n <- DT::renderDataTable({
    req(!is.null(dftapho$dfTaphoNat_n))
    
    datatable(dftapho$dfTaphoNat_n, options = list(pageLength = 50, scrollX = TRUE))
  })
  
  output$table_dfTaphoNat_Ntot <- DT::renderDataTable({
    req(!is.null(dftapho$dfTaphoNat_Ntot))
    dftapho$dfTaphoNat_Ntot
  })
  
  output$table_dfTaphoNat_p <- DT::renderDataTable({
    req(!is.null(dftapho$dfTaphoNat_p))
    
    datatable(dftapho$dfTaphoNat_p, options = list(pageLength = 50, scrollX = TRUE))  %>% formatPercentage(names(dftapho$dfTaphoNat_p))
  })
  
  output$table_TableFract <- DT::renderDataTable({
    req(!is.null(dftapho$TableFract))
    
    datatable(dftapho$TableFract, options = list(pageLength = 50, scrollX = TRUE)) %>% formatPercentage("G/(G+D)")
  })
  
  output$table_TableConcType <- DT::renderDataTable({
    req(!is.null(dftapho$TableConcType))
    
    datatable(dftapho$TableConcType, options = list(pageLength = 50, scrollX = TRUE))
  })
  
  output$table_TableBoneColor <- DT::renderDataTable({
    req(!is.null(dftapho$TableBoneColor))
   
    datatable( dftapho$TableBoneColor, options = list(pageLength = 50, scrollX = TRUE))
  })
  
  output$table_raw_dataset <- DT::renderDataTable({
    req(!is.null(dftapho$raw_dataset))
    dftapho$raw_dataset
  })
  
  #exporting df$tapho_table
  output$export_tapho_tables <- downloadHandler(
    filename = function() {
      paste0(site_name(),"_Tapho_PivotTable.xlsx")
    },
    content = function(file) {
      dfTaphoAccu_n <- dftapho$dfTaphoAccu_n
      dfTaphoAccu_Ntot <- dftapho$dfTaphoAccu_Ntot
      dfTaphoAccu_p <- dftapho$dfTaphoAccu_p
      dfTaphoNat_n <- dftapho$dfTaphoNat_n
      dfTaphoNat_Ntot <- dftapho$dfTaphoNat_Ntot
      dfTaphoNat_p <- dftapho$dfTaphoNat_p
      TableFract <- dftapho$TableFract
      TableConcType <- dftapho$TableConcType
      TableBoneColor <- dftapho$TableBoneColor
      raw_dataset <- dftapho$raw_dataset
      
      write.xlsx(list(dfTaphoAccu_n,dfTaphoAccu_Ntot, dfTaphoAccu_p,dfTaphoNat_n,dfTaphoNat_Ntot, dfTaphoNat_p, TableFract, TableConcType, TableBoneColor, raw_dataset), file, sheetName = c("dfTaphoAccu_n","dfTaphoAccu_Ntot", "dfTaphoAccu_p","dfTaphoNat_n","dfTaphoNat_Ntot", "dfTaphoNat_p", "TableFract", "TableConcType", "TableBoneColor", "raw_dataset"), rowNames = TRUE, colNames = TRUE)
    }
  )
  
  ################################ SERVER: TAPHO FIGURES#################################################################
  
  # Functions for Tapho plots
  
  plot_Tapho <- function(dfn, dfNtot, var, varname, groupselect)
  {
    require(DescTools)
    require(ggplot2)
    dataforplot<- cbind(dfNtot[,var], BinomCI(dfn[,var],
                                              dfNtot[,var],
                                              conf.level = 0.95,
                                              method = "wilson"))
    colnames(dataforplot) <- c("N", "p","low","up")
    data = data.frame(Group = row.names(dataforplot), dataforplot)
    data = data[groupselect,]
    # print(data)
    plot <- ggplot(data, aes(x=Group, y=p, ymin=0, ymax=1)) + 
      geom_errorbar(aes(ymin=low, ymax=up),
                    width=.15,                    # Width of the error helpbars
                    position=position_dodge(.9)) +
      geom_point(shape=21, fill="white", size =1.5, stroke=0.8, stat="identity") +
      xlab(NULL) +
      ylab(NULL) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      ggtitle(varname) +
      theme(plot.title = element_text(size=10))
    return(plot)
  }
  

  bar_TaphoNat <- function(data, varname, var1, var2, var3, var4, groupselect)
  {
    require(reshape2)
    require(ggplot2)
    dataforplot <- data[,c(var1, var2, var3, var4)]
    dataforplot = dataforplot[groupselect,]
    # print(dataforplot)
    dataforplot = data.frame(Group = row.names(dataforplot), dataforplot)
    dataforplot <- melt(dataforplot,id.vars = "Group")
    plot <- ggplot(dataforplot, aes(x=Group, y=value,ymin=0, ymax=1, fill=variable)) +
      geom_bar(stat="identity") + 
      geom_col(position = position_stack(reverse = TRUE)) +
      xlab(NULL) +
      ylab(NULL) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      ggtitle(varname) +
      theme(plot.title = element_text(size=10)) 
    return(plot)
  }
  
  
  bar_createPercentage <- function(varname, data, groupselect)
  {
    require(reshape2)
    require(ggplot2)
    dataforplot <- data[,-which(names(data) == "Sum")]
    dataforplot = dataforplot[groupselect,]
    # print(dataforplot)
    dataforplot <- dataforplot/rowSums(dataforplot)
    # print(dataforplot)
    dataforplot = data.frame(Group = row.names(dataforplot), dataforplot)
    dataforplot <- melt(dataforplot,id.vars = "Group")
    plot <- ggplot(dataforplot, aes(x=Group, y=value,ymin=0, ymax=1, fill=variable)) +
      geom_bar(stat="identity") + 
      geom_col(position = position_stack(reverse = TRUE)) +
      xlab(NULL) +
      ylab(NULL) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      ggtitle(varname) +
      theme(plot.title = element_text(size=10)) 
    return(plot)
  }
  
  
  # plot for barread
  barRead <- reactive({
    req(!is.null(dftapho$dfTaphoNat_p))
    
    legend1 <- case_when(
      (input$label_language_figtapho == "EN") ~ "Readibility of cortical surfaces",
      (input$label_language_figtapho == "FR") ~ "Lisibilité des surfaces corticales",
    )
    
    legend2 <- case_when(
      (input$label_language_figtapho == "FR") ~ "% surface preservée",
      (input$label_language_figtapho == "EN") ~ "% well-preserved surface",
    )
    
    p <-  bar_TaphoNat(dftapho$TaphoNat_p, legend1,"Read1","Read2","Read3","Read4", groupselect = ) + theme(panel.background = element_blank(), legend.title = element_text(size=10)) + scale_fill_manual(name=legend2, values=c("#D1D0D2", "#A7A9AC", "#808285", "#58595B"), labels=c("0-25%", "25-50%", "50-75%","75-100%"), guide=guide_legend(reverse=T))
    return(p)
  })
  
  output$barRead_output <- renderPlot({
    req(!is.null(barRead()))
    print(barRead())
  })
  
  ### SAVING barRead plot
  output$saveplot_barRead <- downloadHandler(
    filename = paste0("plot_barread_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file){
      ggsave(barRead(),filename=file, device = "pdf")
    },
  )
  
  #barchart plot for BoneColor
  barColor <- reactive({
    req(!is.null(dftapho$TableBoneColor))
    
    legend <- case_when(
      (input$label_language_figtapho == "EN") ~ "Bone color",
      (input$label_language_figtapho == "FR") ~ "Couleur des ossements",
    )
    
    p <- bar_createPercentage(legend, dftapho$TableBoneColor, groupselect = ) + theme(panel.background = element_blank(), legend.title = element_text(size=10)) + scale_fill_discrete(name = legend)
    return(p)
  })
  
  output$barColor <- renderPlot({
    print(barColor())
  })
  
  ### SAVING barcolor plot
  output$saveplot_barColor <- downloadHandler(
    filename = paste0("plot_barcolor_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file){
      ggsave(barColor(),filename=file, device = "pdf")
    },
  )
  
  
  ##session store for figure export
  session_store <- reactiveValues() 
  
  # plot for TaphoAccu 
  plot_taphoaccu <- function(var)({
    req(!is.null(dftapho$TaphoAccu_n))
    req(!is.null(dftapho$TaphoAccu_Ntot))
    
    legendEN <- case_when(
      (var == "Cut") ~ "Cut marks",
      (var == "Ctot") ~ "Carnivore marks (without digested)",
      (var == "Dig") ~ "Digested remains",
      (var == "Ptot") ~ "Percussion/pressure marks",
      (var == "Burnt") ~ "Burnt remains"
    )
    
    legendFR <- case_when(
      (var == "Cut") ~ "Stries de découpe",
      (var == "Ctot") ~ "Traces de carnivores (digérés exclus)",
      (var == "Dig") ~ "Traces de digestion",
      (var == "Ptot") ~ "Traces de percussion/pression",
      (var == "Burnt") ~ "Restes brûlés"
    )
    
    legend <- case_when(
      (input$label_language_figtapho == "EN") ~ legendEN,
      (input$label_language_figtapho == "FR") ~ legendFR,
    )
    
    p <-  plot_Tapho(dfn = dftapho$TaphoAccu_n, dfNtot = dftapho$TaphoAccu_Ntot, var = var, varname = legend, groupselect = )
    session_store$plot <- p
    return(p)
  })
  
  output$plot_taphoaccu <- renderPlot({
    req(input$var_taphoaccu)
    plot_taphoaccu(input$var_taphoaccu)
  })
  
  ### SAVING PLOTS
  output$saveplot_taphoaccu <- downloadHandler(
    filename = paste0("plot_taphoaccu_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file){
      ggsave(session_store$plot,filename=file, device = "pdf")
    },
  )
  
  
  # TaphoNat
  plot_taphonat <- function(var){
    req(!is.null(dftapho$TaphoNat_n))
    req(!is.null(dftapho$TaphoNat_Ntot))
    
    legendFR <- case_when(
      var == "Exfo" ~ "Exfoliation",
      var == "DenD" ~ "Dépots dendritiques (racines ?)",
      var == "DenE" ~ "Creusements dendritiques (racines ?)",
      var == "Sheet1" ~ "Délitement (léger)",
      var == "Sheet2" ~ "Délitement (important)",
      var == "Abra1" ~ "Abrasion (légère)",
      var == "Abra2" ~ "Abrasion (forte)",
      var == "CirE" ~ "Creusements en cupules",
      var == "Crack1" ~ "Fissures longitudinales (légères)",
      var == "Crack2" ~ "Fissures longitudinales (profondes)",
      var == "Conc1" ~ "Concrétions (<1/3)",
      var == "Conc2" ~ "Concrétions (1/3 à 2/3)",
      var == "Conc3" ~ "Concrétions (>2/3)",
      var == "Black1" ~ "Dépôts noirs (<1/3)",
      var == "Black2" ~ "Dépôts noirs (1/3 à 2/3)",
      var == "Black3" ~ "Dépôts noirs (>2/3)",
      var == "Tramp" ~ "Stries de piétinement",
      var == "TrampUnsure" ~ "Stries de piétinement incertaines",
      var == "Cor1" ~ "Attaque chimique (légère)",
      var == "Cor2" ~ "Attaque chimique (forte)"
    )
    legendEN <- case_when(
      var == "Exfo" ~ "Exfoliation",
      var == "DenD" ~ "Dendritic deposits (roots?)",
      var == "DenE" ~ "Dendritic etching (roots?)",
      var == "Sheet1" ~ "Sheeting (slight)",
      var == "Sheet2" ~ "Sheeting (important)",
      var == "Abra1" ~ "Abrasion (slight)",
      var == "Abra2" ~ "Abrasion (strong)",
      var == "CirE" ~ "Circular etching",
      var == "Crack1" ~ "Longitudinal cracks (slight)",
      var == "Crack2" ~ "Longitudinal cracks (deep)",
      var == "Conc1" ~ "Concretions (<1/3)",
      var == "Conc2" ~ "Concretions (1/3 to 2/3)",
      var == "Conc3" ~ "Concretions (>2/3)",
      var == "Black1" ~ "Black deposits (<1/3)",
      var == "Black2" ~ "Black deposits (1/3 to 2/3)",
      var == "Black3" ~ "Black deposits (>2/3)",
      var == "Tramp" ~ "Trampling marks",
      var == "TrampUnsure" ~ "Uncertain trampling marks",
      var == "Cor1" ~ "Chemical attack (slight)",
      var == "Cor2" ~ "Chemical attack (strong)"
    )
    
    legend <- case_when(
      (input$label_language_figtapho == "EN") ~ legendEN,
      (input$label_language_figtapho == "FR") ~ legendFR,
    )
    
    p <- plot_Tapho(dfn = dftapho$TaphoNat_n, dfNtot = dftapho$TaphoNat_Ntot, var = var, varname = legend, groupselect = )  
    session_store$plot <- p
    return(p)
  }
  
  output$plot_taphonat <- renderPlot({
    req(input$var_taphonat)
    plot_taphonat(input$var_taphonat)
  })
  
  #SAVING PLOTS
  output$saveplot_taphonat <- downloadHandler(
    filename = paste0("plot_taphonat_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file){
      ggsave(session_store$plot,filename=file, device = "pdf")
    }
  ) 

  
  #  Multi Plot Tapho accu 
  output$multiplot_taphoaccu <- renderPlot({
    req(input$multi_var_taphoaccu)  
    plots <- lapply(input$multi_var_taphoaccu, function(var) {
      plot_taphoaccu(var) 
    })
    
    num_plots <- length(plots)
    if (num_plots > 0) {
      ncol <- 3  # 3 graphe par ligne 
      nrow <- ceiling(num_plots / ncol)  
      grid.arrange(grobs = plots, ncol = ncol, nrow = nrow)  
    } else {
      NULL  
    }
  }, height = 780, width = 1400)  
  
  
  # Saving Plot 
  output$download_multiplot_accu <- downloadHandler(
    filename = paste0("multiplot_taphoaccu_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file) {
      plots <- lapply(input$multi_var_taphoaccu, function(var) {
        plot_taphoaccu(var) 
      })
      g <- arrangeGrob(grobs = plots)
      ggsave(file, plot = g, device = "pdf", width = 11, height = 8.5)
    }
  )
  
  
  # Partie MultiPlot Nat
  output$multiplot_taphonat <- renderPlot({
    req(input$multi_var_taphonat)  
    plots <- lapply(input$multi_var_taphonat, function(var) {
      plot_taphonat(var)
    })
    
    num_plots <- length(plots)
    if (num_plots > 0) {
      ncol <- 3  # 3 graphe par ligne 
      nrow <- ceiling(num_plots / ncol)  
      grid.arrange(grobs = plots, ncol = ncol, nrow = nrow)  
    } else {
      NULL  
    }
  }, height = 780, width = 1400)  

  output$select_multiplot_text1 <- renderText({ getTranslation(input$lang, "select_multiplot_text1") })
  output$min1 <- renderText({ getTranslation(input$lang, "min1") })
  
  # Saving Plot 
  output$download_multiplot_nat<- downloadHandler(
    filename = paste0("mutiplot_taphonat_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file) {
      plots <- lapply(input$multi_var_taphonat, function(var) {
        plot_taphonat(var)  
      })
      g <- arrangeGrob(grobs = plots)
      ggsave(file, plot = g, device = "pdf", width = 11, height = 8.5)
    }
  )
  
  
  
  ###################################################### SERVER: CUT #############################################################################################

 
  #creating reactive for cut analyses
  df_cutdata_analysis <- reactiveValues( cutdata=NULL,
                                         cutdatalong=NULL,
                                         selected_group=NULL,
                                         selected_taxa=NULL,
                                         selected_records=NULL)
  
  observeEvent(input$button_cut, {
    req(!is.null(df_cutdata_filtered()))  
   if(nrow(df_cutdata_filtered()) == 0) #so that data is removed if no cutmarks record
     {
     df_cutdata_analysis$cutdata <- NULL
     df_cutdata_analysis$cutdatalong <- NULL
     
   }
    else{
    dataset <- as.matrix(df_cutdata_filtered())

    #recording the sidebar selections at the time of df creation
    df_cutdata_analysis$selected_group <- input$list_sidebar_group
    df_cutdata_analysis$selected_taxa <- input$list_sidebar_taxa
    df_cutdata_analysis$selected_records <- nrow(df_cutdata_filtered())
    
    #part included in order to change the list of selected groups and taxa to all if necessary
    if(length(df_cutdata_analysis$selected_group) == length(choices$group)){
      df_cutdata_analysis$selected_group <- "all groups"
    }
    if(length(df_cutdata_analysis$selected_taxa) == length(choices$taxa)){
      df_cutdata_analysis$selected_taxa <- "all taxa"
    }
    
    
    
    
    # selecting the codes activities in French (CutCodesFR.csv) or English (CutCodesEN.csv)
    CutCodes<-read.csv(input$cutcodes_selected, header=TRUE, sep=";", row.names=1, encoding="UTF-8") #cut.codes = "ref_data/CutCodesEN.csv"
    
    # reordering dataset
    datatmp <- replace(dataset[,14:423], is.na(dataset[,14:423]),0) #replacing NA by 0 in the recorded cut-marks
    datatmpGroup <- replace(dataset[,"Group"], is.na(dataset[,"Group"]),"Unknown") #replacing NA by Unknown for the Group field
    SumGroup <- table(datatmpGroup, exclude = NULL)
    datatmp2<-cbind(datatmpGroup,datatmp)
    dataOK<-data.frame(datatmp2)

    rm(datatmp)
    rm(datatmp2)
  
    colnames(dataOK)[1] <- "Group"
    
    # creating a pivot table by group and cut code, only if coded at least once
    tmp <- rep(0,3+nrow(SumGroup))

    for(i in 2:411) {
      tmpPivot<-table(dataOK[,"Group"], dataOK[,i], exclude = NULL)

      if(" 1" %in% colnames(tmpPivot)){
        tmpCol<-data.frame(tmpPivot[," 1"])
        colnames(tmpCol)<-colnames(dataOK)[i]
        tmpCodes<-t(CutCodes[colnames(tmpCol),1:3])
        tmpCodesCol<-rbind(tmpCodes, tmpCol,stringsAsFactors = FALSE)
        tmp<-cbind(tmp,tmpCodesCol)
      }

    }
    col_goodnames_tmp <- rownames(tmp[,-1])
    tmp <- data.frame(t(tmp[,-1]),stringsAsFactors = FALSE)
    colnames(tmp) <- col_goodnames_tmp #necessary to get the good names if weird names in Group

    if(nrow(SumGroup) > 1){ ##necessary to avoid bugs if there is only one group
      tmp$AllGroup <- rowSums(sapply(tmp[,-c(1:3)], as.numeric)) #summing rows to get the AllGroup column
    }
    CutData<-tmp
    rm(tmp)
    df_cutdata_analysis$cutdata <- CutData
    
    
    # creating a pivot table for the proportion of longitudinal cut-marks on shafts
    tmpDataSH <- dataOK[,c("Group","Fs_a","Fs_ap", "Hs_a","Hs_ap","Rs_a","Rs_ap","Rs_b","Rs_bp","Rs_bs","Rs_c","Rs_cp","Rs_cs","Ts_a","Ts_ap","Ts_b","Ts_bp","Ts_c","Ts_cp","Ts_d","Ts_dp","Ts_ds","Ts_e","Ts_ep","Ts_es")]
    
    tmp <- rep(0,nrow(SumGroup))
    for(i in 2:25) {
      tmpPivot<-table(tmpDataSH[,"Group"], tmpDataSH[,i], exclude = NULL)
      tmpCol <- data.frame(rep(0,nrow(SumGroup)))
      colnames(tmpCol)<-colnames(tmpDataSH)[i]
      if(" 1" %in% colnames(tmpPivot)){
        tmpCol<-data.frame(tmpPivot[," 1"])
        colnames(tmpCol)<-colnames(tmpDataSH)[i]
      }
      tmp<-cbind(tmp,tmpCol)
    }

    
    tmp$nHumSH_long <- tmp[,5]
    tmp$nHumSH_all <- rowSums(tmp[,4:5])
    tmp$nFemSH_long <- tmp[,3]
    tmp$nFemSH_all <- rowSums(tmp[,2:3])
    tmp$nRadSH_long <- rowSums(tmp[,c(7,9,12)])
    tmp$nRadSH_all <- rowSums(tmp[,6:13])
    tmp$nTibSH_long <- rowSums(tmp[,c(15,17,19,21,24)])
    tmp$nTibSH_all <- rowSums(tmp[,14:25])
    tmp$nAllSH_long <- rowSums(tmp[,c(26,28,30,32)])
    tmp$nAllSH_all <- rowSums(tmp[,c(27,29,31,33)])
    tmp <- rbind(tmp, "AllGroup" = colSums(tmp))
    tmp$pHumSH_long <- tmp$nHumSH_long / tmp$nHumSH_all
    tmp$pFemSH_long <- tmp$nFemSH_long / tmp$nFemSH_all
    tmp$pRadSH_long <- tmp$nRadSH_long / tmp$nRadSH_all
    tmp$pTibSH_long <- tmp$nTibSH_long / tmp$nTibSH_all
    tmp$pAllSH_long <- tmp$nAllSH_long / tmp$nAllSH_all
    
    
    CutDataLong <- data.frame(t(tmp[,26:40]),check.names = FALSE,check.rows = FALSE)
    
    df_cutdata_analysis$cutdatalong <- CutDataLong
    
    }#end of else
  })
  
  
  
  # output info for selected records
  output$cut_selected_group_text<- renderText({
    ifelse(!is.null(df_cutdata_analysis$selected_group),paste("You are currently analysing the following selection of groups:",paste(df_cutdata_analysis$selected_group, collapse = ", ")),"Zero group under cut-mark analysis")
  })
  
  output$cut_selected_taxa_text<- renderText({
    ifelse(!is.null(df_cutdata_analysis$selected_taxa),paste("You are currently analysing the following selection of taxa:",paste(df_cutdata_analysis$selected_taxa, collapse = ", ")),"Zero taxon under cut-mark analysis")
  })
  
  output$cut_selected_records_text<- renderText({
    ifelse(!is.null(df_cutdata_analysis$selected_records),paste("You are currently analysing:",paste(df_cutdata_analysis$selected_records), "records"),"Zero record under cut-mark analysis")
  })
  
  
  # output cut tables
  
  output$cutdata_analysis_table <- DT::renderDataTable({
    req(df_cutdata_analysis$cutdata)
    datatable(df_cutdata_analysis$cutdata, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  output$cutdatalong_analysis_table <- DT::renderDataTable({
    req(df_cutdata_analysis$cutdatalong)
    datatable(df_cutdata_analysis$cutdatalong, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  
  #exporting for cut tables
  output$export_cut_tables <- downloadHandler(
    filename = function() {
      paste0(site_name(),"_Cut_Tables.xlsx")
    },
    content = function(file) {
      
      
      write.xlsx(list(df_cutdata_analysis$cutdata,
                      df_cutdata_analysis$cutdatalong),
                 file, sheetName = c("CutData","CutDataLong"), rowNames = TRUE, colNames = TRUE)
    }
  )
  
  ###################################################### SERVER: NDE #############################################################################################
  
  # Creating NDE tables
  df_ndedata_analysis <- reactiveValues(
    NDEData = NULL,
    MAUfromNDE = NULL,
    MNIfromNDE = NULL,
    MAUbyElement = NULL,
    MAUbyElementPortion = NULL,
    pMAUbyElement = NULL,
    pMAUbyElementPortion = NULL,
    boneDensityData = NULL,
    indexData = NULL, 
    selected_group=NULL,
    selected_taxa=NULL,
    selected_records=NULL
  )
  
  ###################dataprep_NDE
  dataprep_NDE <- function(data, species.codes, exclude.foetus)
  {
    dataset <- data
    # by default, this script removes foetus bones from the skeletal-part analysis
    if(exclude.foetus == "TRUE"){
      dataset$`SKEL::AgeCort` <-   replace(dataset$`SKEL::AgeCort`, is.na(dataset$`SKEL::AgeCort`),"?") # replacing NAs in AgeCort, to avoid errors
      tmpFoetus <- dataset$`SKEL::AgeCort`=="F"
      dataset <- dataset[!tmpFoetus,]
      rm(tmpFoetus)
    }
    
    # select the correct species for NDEcodes
    NDEcodes<-read.csv(species.codes, header=TRUE, sep=";", encoding="UTF-8") #number of bones per individual for RANG
    
    
    # reordering dataset
    datatmp <- replace(dataset[,13:129], is.na(dataset[,13:129]),0) #replacing empty NDE by a value of 0
    datatmp2 <- replace(dataset$Group, is.na(dataset$Group),"NA Group") # replacing NAs in Group, to avoid errors
    datatmp3 <- cbind(datatmp2,dataset$`SKEL::Anat`,dataset$`SKEL::Anat_Detail`,datatmp)
    dataOK <- data.frame(datatmp3)
    colnames(dataOK)[1] <- 'Group'
    colnames(dataOK)[2] <- 'Anat'
    colnames(dataOK)[3] <- 'Anat_Detail'
    rm(datatmp)
    rm(datatmp2)
    rm(datatmp3)
    SumGroup <- table(dataOK$Group, exclude = NULL)
    
    # creating a pivot table by group and landmark code, for the 0/1/2 landmarks (landmarks 17,30,88,89,90 and 91 excluded)
    tmp <- rep(0,nrow(SumGroup))
    for(i in 10:120) {
      tmpPivot<-addmargins(table(dataOK$Group, dataOK[,i], exclude = NULL),2)
      tmpCol <- data.frame(tmpPivot[,"Sum"]-tmpPivot[,"0"])
      colnames(tmpCol)<-colnames(dataOK)[i]
      tmp<-cbind(tmp,tmpCol)
    }
    proper_names <- rownames(tmp) #necessary to keep weird group names
    NDEData_1<-data.frame(t(tmp[,-1]),check.names = FALSE,check.rows = FALSE)
    colnames(NDEData_1) <- proper_names #necessary to keep weird group names
    rm(tmpPivot)
    rm(tmpCol)
    rm(i)
    rm(tmp)
    rm(proper_names)
    
    # total landmarks measurements calculations
    tmpMes17 <- rowsum(dataOK$Ldmk_17, dataOK$Group)
    tmpMes30 <- rowsum(dataOK$Ldmk_30, dataOK$Group)
    tmpMes88 <- rowsum(dataOK$Ldmk_88mesRib, dataOK$Group)
    tmpMes89 <- rowsum(dataOK$Ldmk_89mesRad, dataOK$Group)
    tmpMes90 <- rowsum(dataOK$Ldmk_90mesMan, dataOK$Group)
    tmpMes91 <- rowsum(dataOK$Ldmk_91mesFem, dataOK$Group)
    tmpMes <- cbind(tmpMes17,tmpMes30,tmpMes88,tmpMes89,tmpMes90,tmpMes91)
    colnames(tmpMes) <- c("Ldmk_17","Ldmk_30","Ldmk_88","Ldmk_89","Ldmk_90", "Ldmk_91")
    proper_namesMes <- rownames(tmpMes) #necessary to keep weird group names
    NDEData_2<-data.frame(t(tmpMes),check.names = FALSE,check.rows = FALSE)
    colnames(NDEData_2) <- proper_namesMes #necessary to keep weird group names
    rm(tmpMes17)
    rm(tmpMes30)
    rm(tmpMes88)
    rm(tmpMes89)
    rm(tmpMes90)
    rm(tmpMes91)
    rm(tmpMes)
    
    # adapt the script for cases where there is only one group
    if(nrow(SumGroup) == "1"){
      colnames(NDEData_1) <-colnames(NDEData_2)
    }
    
    # final NDEtable (counts and measurements) - first step
    NDEData <- rbind(NDEData_1, NDEData_2) #merging 0,1,2 data and measurements
    rm(NDEData_1)
    rm(NDEData_2)
    NDEData$AllGroup <- rowSums(NDEData) #calculating total NDE for all groups combined
    
    # calculation of MAU by NDE
    tmpMAU <- NDEData / NDEcodes$Quantity
    MAUfromNDE <- cbind(NDEcodes[,2:5],tmpMAU)
    MNIfromNDE <- cbind(NDEcodes[,2:5],ceiling (tmpMAU))
    rm(tmpMAU)
    MAUfromNDE <- subset(MAUfromNDE, Quantity!="NA") #removing NDE landmarks that are not present in this species
    MNIfromNDE <- subset(MNIfromNDE, Quantity!="NA") #removing NDE landmarks that are not present in this species
    
    # final NDEtable - second step
    NDEData <- cbind(NDEcodes[,2:5],NDEData) #merging NDEcodes and NDEdata
    NDEData <- subset(NDEData, Quantity!="NA") #removing NDE landmarks that are not present in this species
    
    # maxMAU by Element
    MAUbyElement<-aggregate(MAUfromNDE[5:(nrow(SumGroup)+5)], by = list(MAUfromNDE$Element), max)
    rownames(MAUbyElement)<-MAUbyElement[,1]
    MAUbyElement<-MAUbyElement[,-1]
    
    # maxMAU by Element and Portion
    MAUbyElementPortion<-aggregate(MAUfromNDE[5:(nrow(SumGroup)+5)], by = list(MAUfromNDE$ElementPortion), max)
    rownames(MAUbyElementPortion)<-MAUbyElementPortion[,1]
    MAUbyElementPortion<-MAUbyElementPortion[,-1]
    
    # calculating %MAUs
    tmpMaxMAUbyElement <- apply(MAUbyElement, 2, max, na.rm = TRUE)
    pMAUbyElement <- data.frame(t(t(MAUbyElement)/tmpMaxMAUbyElement))
    colnames(pMAUbyElement) <- colnames(MAUbyElement)
    tmpMaxMAUbyElementPortion <- apply(MAUbyElementPortion, 2, max, na.rm = TRUE)
    pMAUbyElementPortion <- data.frame(t(t(MAUbyElementPortion)/tmpMaxMAUbyElementPortion))
    colnames(pMAUbyElementPortion) <- colnames(MAUbyElementPortion)
    rm(tmpMaxMAUbyElement)
    rm(tmpMaxMAUbyElementPortion)
    
    
    NDElist <- list(NDEData, MAUfromNDE, MNIfromNDE, MAUbyElement, MAUbyElementPortion, pMAUbyElement, pMAUbyElementPortion, all.names = TRUE)
    names(NDElist) <- c("NDEData","MAUfromNDE", "MNIfromNDE", "MAUbyElement", "MAUbyElementPortion", "pMAUbyElement","pMAUbyElementPortion")
    
    return(NDElist)
    
  }
  
  

  observeEvent(input$button_nde, {
    req(!is.null(df_ndedata_filtered()))
    
    #recording the sidebar selections at the time of df creation
    df_ndedata_analysis$selected_group <- input$list_sidebar_group
    df_ndedata_analysis$selected_taxa <- input$list_sidebar_taxa
    df_ndedata_analysis$selected_records <- nrow(df_ndedata_filtered())
    
    #part included in order to change the list of selected groups and taxa to all if necessary
    if(length(df_ndedata_analysis$selected_group) == length(choices$group)){
      df_ndedata_analysis$selected_group <- "all groups"
    }
    if(length(df_ndedata_analysis$selected_taxa) == length(choices$taxa)){
      df_ndedata_analysis$selected_taxa <- "all taxa"
    }

   
    dataset <- dataprep_NDE(data = df_ndedata_filtered(), species.codes =  input$ndecodes_selected, exclude.foetus = "TRUE")
  
    df_ndedata_analysis$NDEData <- dataset[["NDEData"]]
    df_ndedata_analysis$MAUfromNDE <- dataset[["MAUfromNDE"]]
    df_ndedata_analysis$MNIfromNDE <- dataset[["MNIfromNDE"]]
    df_ndedata_analysis$MAUbyElement <- dataset[["MAUbyElement"]]
    df_ndedata_analysis$MAUbyElementPortion <- dataset[["MAUbyElementPortion"]]
    df_ndedata_analysis$pMAUbyElement <- dataset[["pMAUbyElement"]]
    df_ndedata_analysis$pMAUbyElementPortion <- dataset[["pMAUbyElementPortion"]]
    df_ndedata_analysis$list_group <- colnames(df_ndedata_analysis$pMAUbyElement)
    
  })
  
  
  # output info for selected records
  output$nde_selected_group_text<- renderText({
    ifelse(!is.null(df_ndedata_analysis$selected_group),paste("You are currently analysing the following selection of groups:",paste(df_ndedata_analysis$selected_group, collapse = ", ")),"Zero group under skeletal-part analysis")
  })
  
  output$nde_selected_taxa_text<- renderText({
    ifelse(!is.null(df_ndedata_analysis$selected_taxa),paste("You are currently analysing the following selection of taxa:",paste(df_ndedata_analysis$selected_taxa, collapse = ", ")),"Zero taxon under skeletal-part analysis")
  })
  
  output$nde_selected_records_text<- renderText({
    ifelse(!is.null(df_ndedata_analysis$selected_records),paste("You are currently analysing:",paste(df_ndedata_analysis$selected_records), "records"),"Zero record under skeletal-part analysis")
  })
  
  
  
  
  
  
  
  observe({
        updateSelectInput(session, "list_ndedata_group",
                      choices = df_ndedata_analysis$list_group
    )})

  # Output for NDE tables
  output$table_ndedata_analysis_NDEData <- DT::renderDataTable({
    req(df_ndedata_analysis$NDEData)
    datatable(df_ndedata_analysis$NDEData, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  output$table_ndedata_analysis_MAUfromNDE <- DT::renderDataTable({
    req(df_ndedata_analysis$MAUfromNDE)
    datatable(df_ndedata_analysis$MAUfromNDE, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  output$table_ndedata_analysis_MNIfromNDE <- DT::renderDataTable({
    req(df_ndedata_analysis$MNIfromNDE)
    datatable(df_ndedata_analysis$MNIfromNDE, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  output$table_ndedata_analysis_MAUbyElement <- DT::renderDataTable({
    req(df_ndedata_analysis$MAUbyElement)
    datatable(df_ndedata_analysis$MAUbyElement, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  output$table_ndedata_analysis_MAUbyElementPortion <- DT::renderDataTable({
    req(df_ndedata_analysis$MAUbyElementPortion)
    datatable(df_ndedata_analysis$MAUbyElementPortion, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  output$table_ndedata_analysis_pMAUbyElement <- DT::renderDataTable({
    req(df_ndedata_analysis$pMAUbyElement)
    datatable(df_ndedata_analysis$pMAUbyElement, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  output$table_ndedata_analysis_pMAUbyElementPortion <- DT::renderDataTable({
    req(df_ndedata_analysis$pMAUbyElementPortion)
    datatable(df_ndedata_analysis$pMAUbyElementPortion, options = list(pageLength = 100, scrollX = TRUE))
  })
  

  
  #exporting for nde tables
  output$export_nde_tables <- downloadHandler(
    filename = function() {
      paste0(site_name(),"_NDE_Tables.xlsx")
    },
    content = function(file) {
      
      
      write.xlsx(list(df_ndedata_analysis$NDEData,
                            df_ndedata_analysis$MAUfromNDE,
                            df_ndedata_analysis$MNIfromNDE,
                            df_ndedata_analysis$MAUbyElement,
                            df_ndedata_analysis$MAUbyElementPortion,
                            df_ndedata_analysis$pMAUbyElement,
                            df_ndedata_analysis$pMAUbyElementPortion),
         file, sheetName = c("NDEData","MAUfromNDE", "MNIfromNDE","MAUbyElement","MAUbyElementPortion", "pMAUbyElement", "pMAUbyElementPortion"), rowNames = TRUE, colNames = TRUE)
    }
  )
  
  

  
  ###################skelanalysis
  bonedensity <- function(file,ref)
  {
    BoneDensityRef <- read.csv(ref, header=TRUE, sep=";", dec=",",encoding="UTF-8")
    # merging ref data and %MAU for bone density
    tmp_pMAU <- file
    tmp_pMAU$ElementPortion <- rownames(file)
    BoneDensityData <- merge.data.frame(BoneDensityRef, tmp_pMAU, by.x = "ElementPortion")
    return(BoneDensityData)
  }
  
  mergeindex <- function(file,ref)
  {
    tableref <- read.csv(ref, header=TRUE, sep=";", dec=",",encoding="UTF-8")   
    # merging ref data and %MAU for index
    tmp_pMAU <- file
    tmp_pMAU$Element <- rownames(file)
    data <- merge.data.frame(tableref, tmp_pMAU, by.x = "Element")
    return(data)
  }
  
  observe({
    req(input$bonedensity_ref)
    req(df_ndedata_analysis$pMAUbyElementPortion)
    df_ndedata_analysis$boneDensityData <- bonedensity(df_ndedata_analysis$pMAUbyElementPortion, ref = input$bonedensity_ref)
  })
  
  # observeEvent(input$bonedensity_ref, {
  #   req(df_ndedata_analysis$pMAUbyElementPortion)
  #   df_ndedata_analysis$boneDensityData <- bonedensity(df_ndedata_analysis$pMAUbyElementPortion, ref = input$bonedensity_ref)
  # })
  
  observe({
    req(input$index_ref)
    req(df_ndedata_analysis$pMAUbyElement)
    df_ndedata_analysis$indexData <- mergeindex(df_ndedata_analysis$pMAUbyElement, ref = input$index_ref)
  })
  
  # observeEvent(input$index_ref, {
  #   req(df_ndedata_analysis$pMAUbyElement)
  #   df_ndedata_analysis$indexData <- mergeindex(df_ndedata_analysis$pMAUbyElement, ref = input$index_ref)
  # })
  
  output$table_ndedata_analysis_boneDensityData <- DT::renderDataTable({
    req(df_ndedata_analysis$boneDensityData)
    datatable(df_ndedata_analysis$boneDensityData, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  output$table_ndedata_analysis_indexData <- DT::renderDataTable({
    req(df_ndedata_analysis$indexData)
    datatable(df_ndedata_analysis$indexData, options = list(pageLength = 100, scrollX = TRUE))
  })
  
  
  ##### plots for skel analysis
  BoneDensityPlot_reactive <- reactive({
    req(!is.null(df_ndedata_analysis$boneDensityData))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$boneDensityData
    p <- ggplot(data, aes(x = Density, y = data[,plotted_group])) + 
      geom_point(size = 2) +
      geom_text_repel(aes(label = ElementPortion), size = 3.5) +
      xlab("Bone mineral density") + 
      ylab(paste0("%MAU calculated from NDE values (",plotted_group,")"))
    session_store$plot <- p
    return(p)
  })
  
  
  
  output$BoneDensityPlot <- renderPlot({
    req(!is.null(df_ndedata_analysis$boneDensityData))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$boneDensityData
    p <- ggplot(data, aes(x = Density, y = data[,plotted_group])) + 
      geom_point(size = 2) +
      geom_text_repel(aes(label = ElementPortion), size = 3.5) +
      xlab("Bone mineral density") + 
      ylab(paste0("%MAU calculated from NDE values (",plotted_group,")"))
    session_store$plot <- p
    return(p)
  })
  
  output$total_MAU_density <- renderText({
    req(!is.null(df_ndedata_analysis$MAUbyElement))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$MAUbyElement
    paste("The total of MAU for this group is (number of elements):", sum(data[,plotted_group]))
  })
  
  output$max_MAU_density <- renderText({
    req(!is.null(df_ndedata_analysis$MAUbyElement))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$MAUbyElement
    paste("The maximum MAU for this group is (number of elements):", max(data[,plotted_group]))
  })
  
  output$total_MAU_index <- renderText({
    req(!is.null(df_ndedata_analysis$MAUbyElement))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$MAUbyElement
    paste("The total of MAU for this group is (number of elements):", sum(data[,plotted_group]))
  })
  
  output$max_MAU_index <- renderText({
    req(!is.null(df_ndedata_analysis$MAUbyElement))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$MAUbyElement
    paste("The maximum MAU for this group is (number of elements):", max(data[,plotted_group]))
  })
  
  output$cor_test_BoneDensity <- renderPrint({
    req(!is.null(df_ndedata_analysis$boneDensityData))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$boneDensityData
    cor.test(data[,"Density"], data[,plotted_group], method = "spearman")
  })
  
  output$IndexPlot <- renderPlot({
    req(!is.null(df_ndedata_analysis$indexData))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$indexData
    plotted_index <- colnames(data)[2]
    
    p <- ggplot(data, aes(x = data[,plotted_index], y = data[,plotted_group])) + 
      geom_point(size = 2) +
      geom_text_repel(aes(label = Element), size = 3.5) +
      xlab(paste(plotted_index)) + 
      ylab(paste0("%MAU calculated from NDE values (",plotted_group,")"))
    
    session_store$plot <- p
    return(p)
  })
  
  output$cor_test_Index <- renderPrint({
    req(!is.null(df_ndedata_analysis$indexData))
    req(!is.null(input$list_ndedata_group))
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$indexData
    plotted_index <- colnames(data)[2]
    cor.test(data[,plotted_index], data[,plotted_group], method = "spearman")
      })
  
  
  #SAVING PLOTS
  output$saveplot_density <- downloadHandler(
    filename = paste0("plot_bonedensity_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file){
      ggsave(session_store$plot, filename=file, device = "pdf")
    }) 
  
  output$saveplot_index <- downloadHandler(
    filename = paste0("plot_utilityindex_",site_name(),"_", Sys.Date(),".pdf"),
    content = function(file){
      ggsave(session_store$plot,filename=file, device = "pdf")
    }
  ) 
  
  
##### NDE SKELETONS (adapting some code from ZooLogics by Marc Thomas & Enya Regis-Franzke)
  
  plot_skeleton <- reactive({
    req(!is.null(df_ndedata_analysis$pMAUbyElement))
    req(input$skeleton)
    req(!is.null(input$list_ndedata_group))
    
    skeleton_shapefile <- shapefile(input$skeleton)
    
    plotted_group <- input$list_ndedata_group
    data <- df_ndedata_analysis$pMAUbyElement
    dataforplot <- data.frame(Element = rownames(data),
                              p_mau = data[,plotted_group])
    
    shape_data <- merge(skeleton_shapefile, dataforplot, by="Element", all.x = T)
    
    # shape_data@plotOrder = order(-shape_data@data$order)
    # print(shape_data@order)
    
    legend_skel <- paste(paste(input$list_ndedata_group),"skeletal-part representation")
    
    p <- plot(shape_data, main = legend_skel, col = case_when(
      shape_data$p_mau < 0.01 ~ '#FFFFFF',
      shape_data$p_mau < 0.25 ~ '#DCDCDC',
      shape_data$p_mau < 0.50 ~ '#ADADADFF',
      shape_data$p_mau < 0.75 ~ '#4d4d4d',
      shape_data$p_mau < 1.05 ~ '#000000',
      shape_data$Element == "skin" ~ '#FFFFFF',
      TRUE ~ '#FFFFFF'
    ))
    
    recordPlot()
  })
  
  output$skeletal_representation <- renderPlot({
    req(!is.null(df_ndedata_analysis$pMAUbyElement))
    req(input$skeleton)
    replayPlot(req(plot_skeleton()))
  })
  
  output$skeletal_plot <- downloadHandler(
    filename = function() {
      paste0("skeletalplot_",site_name(),"_", Sys.Date(),".pdf")
    },
    content = function(file){
      pdf(file, width = 20, height = 20)
      replayPlot(plot_skeleton())
      dev.off()
      }
  ) 

  
  
  
  
  
#end server
}

  
#  Run the application 
shinyApp(ui = ui, server = server)