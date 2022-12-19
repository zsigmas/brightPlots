MENU_ID <- poc(
    P = "p_val",
    ADJ_P = "adj_p",
    COMP = "comp"
)
#' @export 
menu_UI <- function(id, comparisons, as_tag_list = TRUE) {
    ns <- shiny::NS(id)

    ui <- list(
        adj = shinyWidgets::pickerInput(
        inputId = ns(MENU_ID$ADJ_P), label = NULL, choices = c(Adjusted = "adjust", Unadjusted = "unadj"), width = "100%"),
        comparisons = shiny::selectInput(ns(MENU_ID$COMP), label = NULL, choices = c("Select Comparisons"= "", comparisons), multiple = TRUE, selected = comparisons, width = "100%"),
        p_val = shiny::numericInput(ns(MENU_ID$P), label = NULL, min = 0.01, max = 1, step = .01, value = .05, width = "100%")
    )

    if (as_tag_list) {
        t <- shiny::tags
        ui <- t$ol(
            class = "list-group",
            t$li(
                class = "list-group-item d-flex justify-content-between align-items-start",
                t$div(
                    class = "ms-2 me-auto",
                    t$div(
                        class = "fw-bold",
                        "p-value"
                    ),
                    t$div(
                        ui[["adj"]],
                        ui[["p_val"]]
                    )
                )
            ),
            t$li(
                class = "list-group-item",
                t$div(
                    class = "ms-2 me-auto",
                    t$div(
                        class = "fw-bold",
                        "Comparisons"
                    ),
                    t$div(
                        ui[["comparisons"]]
                    )
                )
            )
        )
    } else {
        ui
    }
}

#' @export

menu_server <- function(id, db_time = 500) {
    mod <- function(input, output, session) {
        ns <- session[["ns"]]        
        list(
            p = shiny::reactive({shiny::req(input[[MENU_ID$P]])}, label = ns(" p")),
            adj = shiny::reactive({
                if(shiny::req(input[[MENU_ID$ADJ_P]]) == "adjust") TRUE else FALSE
                }, label = ns(" adj")),
            comp = shiny::reactive({shiny::req(input[[MENU_ID$COMP]])}, label = ns(" comp"))
        ) %>%
        purrr::map(~.x %>% shiny::debounce(db_time))
    }
    shiny::moduleServer(id, mod)
}