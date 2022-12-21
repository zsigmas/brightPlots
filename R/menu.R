MENU_ID <- poc(
    P = "p_val",
    ADJ_P = "adj_p",
    COMP = "comp",
    GT = "gt",
    LT = "lt",
    GT_ENAB = "gt_enab",
    LT_ENAB = "lt_enab"
)

#' Menu module
#'
#' Provides inputs for the app
#'
#' @name menu_module
NULL

#' @describeIn menu_module UI
#' @param id Shiny id
#' @param comparisons a character vector with the comparisons in the dataset
#'
#' @export
menu_UI <- function(id, comparisons) {
    ns <- shiny::NS(id)

    alpha <- list(
        adj = shinyWidgets::pickerInput(
            inputId = ns(MENU_ID$ADJ_P), label = NULL, choices = c(Adjusted = "adjust", Unadjusted = "unadj"), width = "100%"
        ),
        p_val = shiny::numericInput(ns(MENU_ID$P), label = NULL, min = 0.01, max = 1, step = .01, value = .05, width = "5rem")
    )

    b <- list(
        gt_enab = shiny::checkboxInput(ns(MENU_ID$GT_ENAB), label = shiny::tags[["span"]]("FC >", class = "fs-5"), width = "4rem"),
        lt_enab = shiny::checkboxInput(ns(MENU_ID$LT_ENAB), label = shiny::tags[["span"]]("FC <", class = "fs-5"), width = "4rem"),
        gt = shiny::numericInput(ns(MENU_ID$GT), label = NULL, min = NA, max = NA, step = .01, value = 0, width = "6rem"),
        lt = shiny::numericInput(ns(MENU_ID$LT), label = NULL, min = NA, max = NA, step = .01, value = 0, width = "6rem")
    )

    comp <- list(
        comp = shiny::selectInput(ns(MENU_ID$COMP), label = NULL, choices = c("Select Comparisons" = "", comparisons), multiple = TRUE, selected = comparisons, width = "100%")
    )

    ui <- list(
        alpha = alpha,
        comp = comp,
        b = b
    )
}

#' @describeIn menu_module UI
#' @param id Shiny id
#' @param db_time debounce time for the inputs
#'
#' @value A list with the following entries:
#'  - p: reactive numerical with the limit p value
#'  - adj: reactive logical indicating unadjusted or adjusted p value
#'  - comp: reactive character vector indicating the selected comparisons
#'  - log_fc_range: a reactive numerical vector with two entries `gt` and `lt`. When their corresponding checks are disabled they return `-Inf` and `Inf` respectively.
#'
#' @export

menu_server <- function(id, db_time = 500) {
    mod <- function(input, output, session) {
        ns <- session[["ns"]]
        list(
            p = shiny::reactive(
                {
                    shiny::req(input[[MENU_ID$P]])
                },
                label = ns(" p")
            ),
            adj = shiny::reactive(
                {
                    if (shiny::req(input[[MENU_ID$ADJ_P]]) == "adjust") TRUE else FALSE
                },
                label = ns(" adj")
            ),
            comp = shiny::reactive(
                {
                    shiny::req(input[[MENU_ID$COMP]])
                },
                label = ns(" comp")
            ),
            log_fc_range = shiny::reactive(
                {
                    gt_act <- input[[MENU_ID$GT_ENAB]]
                    lt_act <- input[[MENU_ID$LT_ENAB]]

                    if (gt_act && !lt_act) {
                        return(list(gt = input[[MENU_ID$GT]], lt = -Inf))
                    }
                    if (lt_act && !gt_act) {
                        return(list(gt = Inf, lt = input[[MENU_ID$LT]]))
                    }
                    if (lt_act && gt_act) {
                        return(list(gt = input[[MENU_ID$GT]], lt = input[[MENU_ID$LT]]))
                    }
                    return(list(gt = -Inf, lt = Inf))
                },
                label = ns(" log_fc_range")
            )
        ) %>%
            purrr::map(~ .x %>% shiny::debounce(db_time))
    }
    shiny::moduleServer(id, mod)
}
