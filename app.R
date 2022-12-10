library(shiny)
library(tidyverse)
library(janitor)
library(purrr)
library(forcats)
library(vroom)
library(ggtext)
options(readr.show_progress = FALSE)

guilds <- data.frame(guild = c("Clones Unleashed", "The Bad Batch Unleashed",
                               "Mönkë Unleashed", "Mîghty Ducks Unleashed"),
                     guild_id = c(
                         "DsTZ7vt6Tuy_rjehJDtE-g",
                         "7KsGX9kTRqSzbXDcIqIGvg",
                         "xm_u_4XQRNiKAjZQeUgRmg",
                         "5Gf2zvFDRw2KFstt7CvWvw"
                     ))
fullrosters_long <- read_csv("temprosters.csv")
guild_list <- as.list(guilds$guild)

unit_base_to_unit_name <- read_csv("unit_base_to_name.csv") %>% 
    select(baseId, name = VALUE)

long_ops <- read.csv("operations_long.csv") %>% 
    mutate(align_phase = paste0(alignment,"-",phase))

phase_relic_reqs_df <- data.frame(align_phase = c(
    "DS-1","DS-2","DS-3","DS-4","DS-5","DS-6",
    "Mix-1","Mix-2","Mix-3","Mix-4","Mix-5","Mix-6",
    "LS-1","LS-2","LS-3","LS-4","LS-5","LS-6"),
    relic_req = c("R5","R6","R7","R8","R9","R9"))

hu_converter <- function(x){
    x %>% 
        select(baseId = BaseId,
               CombatType,
               rarity = Stars,
               relic = RelicLevel) %>% 
        mutate(relic = case_when(
            CombatType == "Ship" ~ NA_real_,
            TRUE ~ relic)) %>% 
        select(baseId, rarity, relic) %>% 
        left_join(unit_base_to_unit_name, by = "baseId") %>% 
        select(character_name = name, rarity, relic) %>% 
        return()
}

ui <- fluidPage(
    h2("TB Operations Checker"),
    selectizeInput(inputId = "guild",
                   label = "Pre-loaded Guild Rosters (12-10-22)",
                   choices = guild_list,
                   multiple = FALSE,
                   selected = "The Bad Batch Unleashed"),
    h4("Upload your roster and see what platoons can be filled individually or in combination"),
    # h4("Upload HotUtils Guild Roster"),
    
    fluidRow(column(width = 4,
                    "HotUtils full roster .csv file -- Requires {BaseId, CombatType, Stars, RelicLevel}",
                    fileInput("upload", NULL, buttonLabel = "Upload...")),
             column(width = 2, checkboxInput(inputId = "use_upload", label = "Use Uploaded Data"))),
    h6("(Mixed Phase-1 Operation-1 still has Scythe as a required unit. It will be removed, but we do not know what will replace it)"),
    h4("Current phases"),
    fluidRow(
        column(width = 2,
               radioButtons(inputId = "ds_phase",
                            label = "DS Phase",
                            choices = list(6,5,4,3,2,1),
                            selected = 1)),
        column(width = 2,
               
               radioButtons(inputId = "mix_phase",
                            label = "Mix Phase",
                            choices = list(6,5,4,3,2,1),
                            selected = 1)),
        column(width = 2,
               radioButtons(inputId = "ls_phase",
                            label = "LS Phase",
                            choices = list(6,5,4,3,2,1),
                            selected = 1))
    ),
    # verbatimTextOutput(outputId = "DSchecks"),
    h4("Select operations (groups of 15)"),
    fluidRow(column(width = 3,
                    actionButton("check_on_all_ops",
                                 "Select all Ops")),
             column(width = 3,
                    actionButton("check_off_all_ops",
                                 "Clear all Ops"))
    ),
    splitLayout(
        verticalLayout(
            checkboxGroupInput(inputId = "ds_ops",
                               label = "DS Ops",
                               choices = list(1,2,3,4,5,6),
                               selected = c(1:6),
                               inline = TRUE),
            
            checkboxGroupInput(inputId = "mix_ops",
                               label = "Mix Ops",
                               choices = list(1,2,3,4,5,6),
                               selected = c(1:6),
                               inline = TRUE),
            
            checkboxGroupInput(inputId = "ls_ops",
                               label = "LS Ops",
                               choices = list(1,2,3,4,5,6),
                               selected = c(1:6),
                               inline = TRUE),
        ),
        verticalLayout(
            h5("Which operations are even possible"),
            verbatimTextOutput(outputId = "allchecks")
        )
    ),
    
    # h3("Characters in guild"),
    # fluidRow(column(width = 6,
    plotOutput(outputId = "p1"),
    
    # )),
    h4("Number of total characters needed for selected operations"),
    fluidRow(column(width = 6,
                    verbatimTextOutput(outputId = "cops")
    )),
    downloadButton("download1", "Download full table"),
    
    h4("Net character needs for current rosters"),
    "How many of each character does the guild 'have', how many do they 'need' and what is the 'diff'erence between those two:",
    verbatimTextOutput(outputId = "gcounts_print"),
    
    fluidRow(column(width = 6,
                    verbatimTextOutput(outputId = "gnet_print")
    )),
    downloadButton("download2", "Download full table"),
    br(),
    "By LysiasTheFox"
)

server <- function(input, output, session) {
    
    observeEvent(input$check_on_all_ops, {
        updateCheckboxGroupInput(session, "ds_ops",
                                 selected = c(1:6))
        updateCheckboxGroupInput(session, "mix_ops",
                                 selected = c(1:6))
        updateCheckboxGroupInput(session, "ls_ops",
                                 selected = c(1:6))
    })
    observeEvent(input$check_off_all_ops, {
        updateCheckboxGroupInput(session, "ds_ops",
                                 selected = numeric())
        updateCheckboxGroupInput(session, "mix_ops",
                                 selected = numeric())
        updateCheckboxGroupInput(session, "ls_ops",
                                 selected = numeric())
    })
    
    
    data <- reactive({
        req(input$upload)
        input$upload$datapath %>% 
            read_csv() %>% 
            hu_converter() %>% 
            filter(rarity == 7 & (is.na(relic) | relic >= 5))
    })
    g_roster <- reactive({ 
        fullrosters_long %>% 
            filter(guild == input$guild) %>% 
            filter(rarity == 7 & (is.na(relic) | relic >= 5))
    })
    
    ap_filterset <- reactive({
        c(
            paste0("DS-",input$ds_phase),
            paste0("Mix-",input$mix_phase),
            paste0("LS-",input$ls_phase))
    })
    
    current_ops <- reactive({
        long_ops %>%
            filter(align_phase %in% ap_filterset()) %>% 
            filter(
                (alignment == "DS" & operation %in% input$ds_ops) |
                    (alignment == "Mix" & operation %in% input$mix_ops) |
                    (alignment == "LS" & operation %in% input$ls_ops)
            ) %>% 
            left_join(phase_relic_reqs_df, by = "align_phase") %>% 
            count(baseId, relic_req) %>% 
            # needs to go wide to fill zeroes, then back long
            pivot_wider(id_cols = baseId, names_from = relic_req, values_from = n,
                        values_fill = 0) %>% 
            pivot_longer(cols = -baseId, names_to = "relic_req", values_to = "n") %>% 
            #arrange(desc(n)) %>% 
            # need to make counts cumulative to lower relics
            mutate(relic_num = str_extract(relic_req, "[0-9]{1}$") %>% as.numeric()) %>% 
            group_by(baseId) %>% 
            arrange(desc(relic_num)) %>% 
            mutate(cum_n = reduce(map(0:5, ~ lag(n, ., 0)), `+`)) %>% 
            #mutate(cum_n = sum(lag(n, 1:5, default = 0),na.rm = TRUE)) %>% 
            ungroup() %>% 
            arrange(desc(cum_n), relic_num)
        
    })
    
    # need to make counts cumulative to lower relics
    
    
    copsdf <- reactive({
        current_ops() %>% left_join(unit_base_to_unit_name, by = "baseId") %>% 
            select(name, cum_n, relic_req) %>% 
            group_by(name) %>% 
            #mutate(tot = sum(n, na.rm = TRUE)) %>% 
            ungroup() %>% 
            #pivot_wider(id_cols = c("name", "tot"), names_from = relic_req, values_from = n) %>% 
            pivot_wider(id_cols = "name", names_from = relic_req, values_from = cum_n) %>% 
            replace_na(list(R5 = 0,
                            R6 = 0,
                            R7 = 0,
                            R8 = 0,
                            R9 = 0)) %>% 
            select(name, contains("5"), contains("6"), contains("7"), contains("8"), contains("9")#, tot
            )
    })
    
    output$cops <- renderPrint({
        copsdf() %>% head(10) %>% as.data.frame() %>% 
            column_to_rownames(var = "name")
    })
    
    
    output$download1 <- downloadHandler(
        filename = function() {
            paste0("FullOperationCharacters_currentsettings", ".csv")
        },
        content = function(file) {
            write.csv(copsdf(), file)
        }
    )
    
    ## Make need vs have dataframe
    
    g_counts <- reactive({
        if(input$use_upload == TRUE){
            req(input$upload)
            temp_roster <- data()
        } else {
            temp_roster <- g_roster()
        }
        
        temp_roster %>% 
            count(character_name, name = "R5") %>% 
            left_join(temp_roster %>% 
                          filter(is.na(relic) | relic >= 6) %>% 
                          count(character_name, name = "R6"),
                      by = "character_name",
            ) %>% 
            left_join(temp_roster %>% 
                          filter(is.na(relic) | relic >= 7) %>% 
                          count(character_name, name = "R7"),
                      by = "character_name",
            ) %>% 
            left_join(temp_roster %>% 
                          filter(is.na(relic) | relic >= 8) %>% 
                          count(character_name, name = "R8"),
                      by = "character_name",
            ) %>% 
            left_join(temp_roster %>% 
                          filter(is.na(relic) | relic >= 9) %>% 
                          count(character_name, name = "R9"),
                      by = "character_name",
            ) %>% 
            replace_na(list(R6 = 0,
                            R7 = 0,
                            R8 = 0,
                            R9 = 0))
    })
    
    g_longrostercounts <- reactive({
        # req(input$upload)
        g_counts() %>% 
            pivot_longer(cols = -character_name,
                         names_to = "relic",
                         values_to = "have")
    })
    
    # maybe bring back if we limit to failing/close characters
    # output$gcounts_print <- renderPrint({g_counts()})
    
    ## Check all ops
    all_ops <- reactive({
        # req(input$upload)
        long_allrequirements <- long_ops %>%
            filter(align_phase %in% ap_filterset()) %>% 
            left_join(phase_relic_reqs_df, by = "align_phase") %>% 
            count(baseId, relic_req, align_phase, operation) %>% 
            left_join(unit_base_to_unit_name, by = "baseId")
        # return(long_allrequirements)
        res <- data.frame(alignment = character(),
                          operation = integer(),
                          pass = logical())
        temp_longrostercounts <- g_longrostercounts()
        for(a in c("DS","Mix","LS")){
            for(o in c(1:6)){
                single_op_check <-long_allrequirements %>%
                    filter(str_detect(align_phase, a)) %>% 
                    filter(operation == o) %>% 
                    select(name, relic = relic_req, align_phase, operation, need=n) %>% 
                    left_join(temp_longrostercounts, by = c("name" = "character_name", "relic")) %>% 
                    replace_na(list(have=0)) %>% 
                    mutate(diff = have-need)
                chk_pass <- single_op_check %>% pull(diff) %>% min() >= 0
                
                tempdf <- data.frame(alignment = a,
                                     operation = o,
                                     pass = chk_pass)
                
                res <- bind_rows(res, tempdf)
                
            }
        }
        res2<- res %>% mutate(pass = case_when(pass ~ "Pass", TRUE ~ "---")) %>% 
            pivot_wider(id_cols = alignment, names_from = operation, values_from = pass) %>% 
            as.data.frame() %>% 
            column_to_rownames(var = "alignment")
        return(res2)
    })
    
    output$allchecks <- renderPrint(all_ops())
    # output$DSchecks <- renderPrint(all_ops()[1,])
    # output$Mixchecks <- renderPrint(all_ops()[2,])
    # output$LSchecks <- renderPrint(all_ops()[3,])
    
    
    gnet <- reactive({
        g_counts() %>%
            pivot_longer(cols = -character_name,
                         names_to = "relic", values_to = "have") %>% 
            rename(name = character_name) %>% 
            right_join(
                current_ops() %>% 
                    filter(n > 0) %>% 
                    left_join(unit_base_to_unit_name, by = "baseId") %>% 
                    select(name, relic = relic_req, need = cum_n),
                by = c("name", "relic")
            ) %>% 
            replace_na(replace = list(have = 0)) %>% 
            mutate(diff = have - need) %>% 
            arrange(diff)
    })
    
    ## deduct higher relics before lower in math
    output$gnet_print <- renderPrint({gnet() %>% head(10) %>% as.data.frame() %>% 
            column_to_rownames(var = "name")})
    output$download2 <- downloadHandler(
        filename = function() {
            paste0("NetCharacters_have_need_diff", ".csv")
        },
        content = function(file) {
            write.csv(gnet(), file)
        }
    )
    
    
    ## Plot diverging bars or something
    
    p1 <- reactive({
        p1_df <-
            gnet() %>%
            mutate(rank = row_number()) %>% 
            filter(diff <= 1 | rank <= 15) %>% 
            filter(diff < 21) %>% 
            mutate(name_relic = paste0(name, " (", relic, ")")) %>% 
            mutate(name_relic = fct_reorder(name_relic, -diff))
        #mutate(need = -need)
        # pivot_longer(cols = c("diff","have","need"),
        #              names_to = "class", values_to = "x")
        
        p1_min <- p1_df %>% pull(diff) %>% min()
        p1_max <- p1_df %>% pull(have) %>% max()
        
        
        p1_temp <- p1_df %>% 
            #filter(relic == "R5") %>% 
            ggplot(aes(x = diff, xend = have,
                       y = name_relic, yend = name_relic, 
                       #y = fct_reorder(name, -diff), 
                       
                       #color = class
            )) +
            geom_segment(size = 4.5, alpha = 0.5, color = "#aeb6bf") +
            geom_point(aes(x = diff, y = name_relic, color = diff<0), size = 4) +
            scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "salmon"))+
            geom_point(aes(x = have, y = name_relic), size = 4, color = "dodgerblue") +
            geom_text(aes(x = diff, y = name_relic, label = name_relic),
                      hjust = 1, nudge_x = -0.1) +
            theme_classic()+
            labs(x = NULL, y=NULL, 
                 title = "<b style = 'color: firebrick;'>Characters after filling operations</b> - <b style = 'color: dodgerblue;'>Characters in guild</b>") +
            theme(axis.text.x = element_text(size = 16),
                  axis.text.y = element_blank(),
                  axis.line.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "none", 
                  plot.title = element_markdown(size = 18))+
            coord_cartesian(clip = "off", xlim = c(p1_min - 1, p1_max)) +
            geom_vline(xintercept = -0.05, linetype = "3133") +
            geom_vline(xintercept = seq(-15,-1), alpha = 0.4, linetype = "1313")
        
        return(p1_temp)
    })
    
    output$p1 <- renderPlot(p1())
    
    
}

shinyApp(ui = ui, server = server)
