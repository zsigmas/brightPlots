
DEBUG <- poc(
    INPUT_LIST = "input_list",
    BROWSER_BTN = "browser_button",
    UI = "ui_id",
    SHOW = "show",
    DEBUG_UI = "debug"
)

..__is_db <- function(option_name = "..__db_mode") {
    isTRUE(getOption(option_name))
}

..__db_input_UI <- function(id, option_name = "..__db_mode") {
    if (!..__is_db()) {
        return()
    }
    ns <- shiny::NS(id)
    shiny::wellPanel(
        shiny::h4("Debug Output"),
        shiny::checkboxInput(ns(DEBUG$SHOW), label = "Show Debug", value = FALSE),
        shiny::conditionalPanel(
            condition = paste0("input['", ns(DEBUG$SHOW), "']"),
            shiny::uiOutput(ns(DEBUG$UI))
        )
    )
}

..__db_input_server <- function(id, option_name = "..__db_mode") {
    if (!..__is_db()) {
        return()
    }
    module <- function(input, output, session) {
        shiny::setBookmarkExclude(
            names = c(DEBUG$INPUT_LIST, DEBUG$BROWSER_BTN)
        )

        ns <- session[["ns"]]

        output[[DEBUG$UI]] <- shiny::renderUI({
            shiny::tagList(
                shiny::fluidRow(shiny::verbatimTextOutput(ns(DEBUG$INPUT_LIST))),
                shiny::br(),
                shiny::fluidRow(shiny::actionButton(ns(DEBUG$BROWSER_BTN), "Browser", icon = shiny::icon("eye"))),
            )
        })

        shiny::observeEvent(
            input[[DEBUG$BROWSER_BTN]],
            {
                browser()
            },
            ignoreInit = TRUE
        )

        dev_paste0 <- function(x) {
            paste0(x, collapse = ",")
        }

        output[[DEBUG$INPUT_LIST]] <- shiny::renderText({
            # recurs_paste(input)
            anc <- shiny:::find_ancestor_session(session)
            vc <- shiny::reactiveValuesToList(anc$input)
            vc <- vc[sort(names(vc), index.return = TRUE)[["ix"]]]
            vc <- purrr::map(vc, ~ if (length(.x) > 1) {
                dev_paste0(.x)
            } else {
                .x
            })
            paste0(names(vc), " = ", vc, collapse = "\n")
        })
    }

    shiny::moduleServer(
        id,
        module = module
    )
}