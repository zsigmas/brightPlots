card <- function(...){    
    shiny::div(class = "card", ...)
}

card_body <- function(...){
    shiny::div(class = "card-body", ...)
}

#' @export
mock_app <- function(dataset) {

    options("..__db_mode" = TRUE)

    if (missing(dataset)) {
        dataset <- format_vol_data(toptable)
    }

    menu_elements <- menu_UI("menu", unique(dataset[["facet"]]), as_tag_list = FALSE)

    ui <- shiny::tagList(
        shinyjs::useShinyjs(),
        shiny::fluidPage(
            ..__db_input_UI("dev"),
            shiny::fluidRow(
                class = "m-1",
                shiny::div(class = "col-auto",
                    shiny::div(class = "card",
                        shiny::div(class = "card-header p-1"),                    
                        shiny::div(class = "card-body p-1",                                
                                shiny::fluidRow(
                                    class = "g-1",
                                    shiny::div(
                                        class = "col fs-3 text-end col-auto",                                                                                
                                        shiny::tags[["p"]](htmltools::HTML("&alpha;"), class = "mb-1 pe-1")
                                    ),
                                    shiny::div(                                        
                                        class = "col col-auto",
                                        menu_elements[["p_val"]]
                                    ),
                                    shiny::div(
                                        class = "col col-auto",
                                        menu_elements[["adj"]]
                                    )
                                ),
                                shiny::div(menu_elements[["comparisons"]])
                        )
                    )
                )                
            ),
            shiny::fluidRow(
                class = "m-1",
                shiny::column(width = 12,
                shiny::div(class = "card",
                        shiny::div(class = "card-header p-1"),                    
                        shiny::div(class = "card-body p-1",                                
                                volcano_UI("volcano")
                        )
                    )
                
                )
                ),
            theme = bslib::bs_theme(version = 5)
        )
    )
        # shinydashboard::dashboardSidebar(
        #     menu_elements[["p_val"]],
        #     menu_elements[["adj"]],
        #     menu_elements[["comparisons"]]
        #     ),
        # shinydashboard::dashboardBody(
        #     shiny::fluidRow(
        #         shinydashboard::box(volcano_UI("volcano")),shinydashboard::box("aaaaa"), align = "center"
        #         ),
        #         shiny::fluidRow(
        #     shinydashboard::box(upset_UI("upset")),
        #     align = "center"
        # )
        # ),
        
        
    # )
    server <- function(input, output, session) {
        ..__db_input_server("dev")
        sel <- menu_server("menu")
        shiny::observe({
            purrr::iwalk(sel, ~rlang::inform(paste(.y, .x())))
        })
        volcano_server("volcano", dataset,p = sel[["p"]],adj_p = sel[["adj"]],comp = sel[["comp"]])
        upset_server("upset", dataset,p = sel[["p"]],adj_p = sel[["adj"]],comp = sel[["comp"]])
    }

    shiny::shinyApp(
        ui = ui,
        server = server
    )
}

# DS ----
#' @export
mock_ds_app <- function(dataset) {
    if (missing(dataset)) {
        dataset <- format_vol_data(toptable)
    }

    menu_elements <- menu_UI("menu", unique(dataset[["facet"]]), as_tag_list = FALSE)

    ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "Expression Dashboard"),
        shinydashboard::dashboardSidebar(
            menu_elements[["p_val"]],
            menu_elements[["adj"]],
            menu_elements[["comparisons"]]
            ),
        shinydashboard::dashboardBody(
            shiny::fluidRow(
                shinydashboard::box(volcano_UI("volcano")),shinydashboard::box("aaaaa"), align = "center"
                ),
                shiny::fluidRow(
            shinydashboard::box(upset_UI("upset")),
            align = "center"
        )
        ),
        
        
    )
    server <- function(input, output, session) {
        sel <- menu_server("menu")
        shiny::observe({
            purrr::iwalk(sel, ~rlang::inform(paste(.y, .x())))
        })
        volcano_server("volcano", dataset,p = sel[["p"]],adj_p = sel[["adj"]],comp = sel[["comp"]])
        upset_server("upset", dataset,p = sel[["p"]],adj_p = sel[["adj"]],comp = sel[["comp"]])
    }

    shiny::shinyApp(
        ui = ui,
        server = server
    )
}
