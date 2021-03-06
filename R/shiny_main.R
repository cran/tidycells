common_finish <- function(ui, server, title, viewer_pane, test_this) {
  if (test_this) {
    es <- new.env()
    assign("ui", ui, envir = es)
    assign("server", server, envir = es)
    return(es)
  }

  if (viewer_pane) {
    runGadget(shinyApp(ui, server),
      stopOnCancel = FALSE
    )
  } else {
    viewer <- dialogViewer(title,
      width = 900,
      height = 700
    )
    runGadget(shinyApp(ui, server),
      viewer = viewer,
      stopOnCancel = FALSE
    )
  }
}

shiny_app_va_classify <- function(d, viewer_pane = FALSE, test_this = FALSE) {


  ########## UI ##########
  ui <- miniPage(
    gadgetTitleBar("Value/Attribute Classifier"),
    miniTabstripPanel(
      ui_part_va_classify(),
      ui_part_crop(reset_msg = "Load initial data (With which you started this tab. To load original data reset from Classify Tab.)"),
      ui_part_plot_tune(selected_fill = "type"),
      ui_part_visualize(),
      id = "now_tab_main"
    )
  )

  ########## Server ##########
  server <- server_va_select(d)

  common_finish(ui, server,
    title = "TidyCells: Value/Attribute Classifier",
    viewer_pane = viewer_pane, test_this = test_this
  )
}

shiny_app_crop <- function(d, viewer_pane = FALSE, test_this = FALSE) {


  ########## UI ##########
  ui <- miniPage(
    gadgetTitleBar("Data Crop"),
    miniTabstripPanel(
      ui_part_crop(),
      ui_part_plot_tune(),
      ui_part_visualize(),
      id = "now_tab_main"
    )
  )

  ########## Server ##########
  server <- server_crop(d)

  common_finish(ui, server,
    title = "TidyCells: Data Crop",
    viewer_pane = viewer_pane, test_this = test_this
  )
}

shiny_app_data_block <- function(x, viewer_pane = FALSE, test_this = FALSE) {


  ########## UI ##########
  ui <- miniPage(
    gadgetTitleBar("Data Blocks Inspection"),
    miniTabstripPanel(
      ui_part_data_block(),
      ui_part_plot_tune(txt_alpha_max = 0.9, txt_alpha = 0.2, selected_fill = "type"),
      ui_part_visualize(),
      id = "now_tab_main"
    )
  )

  ########## Server ##########
  server <- server_data_block(x)

  common_finish(ui, server,
    title = "TidyCells: Data Blocks Inspection",
    viewer_pane = viewer_pane, test_this = test_this
  )
}

shiny_app_orientation_modification <- function(x, viewer_pane = FALSE, test_this = FALSE) {


  ########## UI ##########
  ui <- miniPage(
    gadgetTitleBar("Data Blocks Orientation Modification"),
    miniTabstripPanel(
      ui_part_data_block(zoom_this = TRUE, plot_issues_option = FALSE),
      ui_part_orientation_modification(),
      ui_part_plot_tune(txt_alpha_max = 0.9, txt_alpha = 0.2, selected_fill = "type"),
      ui_part_visualize(),
      id = "now_tab_main"
    )
  )

  ########## Server ##########
  server <- server_orientation_modification(x)

  common_finish(ui, server,
    title = "TidyCells: Data Blocks Orientation Modification",
    viewer_pane = viewer_pane, test_this = test_this
  )
}

shiny_app_traceback <- function(x, dcomp, viewer_pane = FALSE, test_this = FALSE) {
  if (!DT_present()) {
    abort(paste("The package 'DT' is required for this functionality",
      "If you want static plot, please use 'cell_traceback_plot'.",
      sep = "\n"
    ))
  }


  ########## UI ##########
  ui <- miniPage(
    gadgetTitleBar("Composition Traceback"),
    miniTabstripPanel(
      ui_part_traceback(),
      ui_part_data_block(zoom_this = TRUE, direction_text_this = FALSE, plot_issues_option = FALSE),
      ui_part_plot_tune(txt_alpha_max = 0.9, selected_fill = "type"),
      ui_part_visualize(),
      id = "now_tab_main"
    )
  )

  ########## Server ##########
  server <- server_traceback(x, dcomp)

  common_finish(ui, server,
    title = "TidyCells: Composition Traceback (Composition Viewer)",
    viewer_pane = viewer_pane, test_this = test_this
  )
}
