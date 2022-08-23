#
#

library(shiny)
library(tidyverse)
library(ggrepel)
library(viridis)
library(wesanderson)
#for converting gene IDs and goterm analysis
library(gprofiler2)
#for handy plotting of heatmaps
library(pheatmap)
#for other colors
library(RColorBrewer)
library(shinybusy)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dauer exit | gene expression."),


        # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(type = "tabs",
                            tabPanel("Overview.",
                                     add_busy_spinner(spin = "fading-circle"),
                                     plotOutput("pcaPlot")),
                            tabPanel("Gene enrichment (timepoints + conditions).",
                                     fluidRow(
                                       h4("Functional gene enrichment across timepoints and conditions."),
                                       radioButtons("timepoint_comp", "\n\nPlease select a comparison.",
                                                    c("0h vs 3h (decreased pheromone conc., differential genes in both conditions)" = "enriched_go_terms_diff_in_both_0h_vs_3h",
                                                      "0h vs 6h (decreased pheromone conc., differential genes in both conditions)" = "enriched_go_terms_diff_in_both_0h_vs_6h",
                                                      "6h vs 9h (food uptake, only differential genes specific for \"with bacteria\")" = "enriched_go_terms_specific_for_A_6h_vs_9h"),
                                                    selected="enriched_go_terms_diff_in_both_0h_vs_3h",
                                                    width="500px"
                                       ), align="left"
                                     ),
                                     fluidRow(
                                       h5("Corresponding p-value is indicated next to each dot."),
                                       column(6,
                                         plotOutput("dotPlot_first",
                                                    click="go_selected_first")
                                        ),align="center"
                                    ),
                                    fluidRow(
                                      h4("See heatmap below."),
                                      plotOutput("heatmap_first",
                                                 width="500px"),
                                      align="center"
                                    )
                            ),
                            tabPanel("Gene enrichment (only conditions).",
                                     fluidRow(
                                       h4("Functional gene enrichment\ncomparing \"with food\" and \"no bacteria\" conditions within one timepoint"),
                                       radioButtons("condition_comp_tp", "\n\nPlease select a time point.",
                                                    c("3h"="3h", "6h"="6h","9h"="9h"),
                                                    selected="6h"
                                       ), align="center"
                                      ),
                                     fluidRow(
                                       h5("Corresponding p-value is indicated next to each dot."),
                                       plotOutput("dotPlot_second",
                                                    click = "go_selected_second"),
                                       align="center"
                                     ),
                                     fluidRow(
                                        h4("Scroll down for heatmap."),
                                        plotOutput("heatmap_second"),
                                        align="center"
                                     )
                            ),
                            tabPanel("Plot single genes.",
                                     fluidRow(
                                              h4("Scroll down for heatmap."),
                                              selectizeInput(
                                                "gene_selected",
                                                label = "Please select a gene.",
                                                choices = readRDS("data_conv.RDS")$gene,
                                                selected = "daf-28",
                                                options = list(create = TRUE)
                                              ),align="center"
                                       ),
                                     fluidRow(
                                      add_busy_spinner(spin = "fading-circle"),
                                      plotOutput("plot_counts_first"),
                                      align="center"
                                     )
                            )
          )
                                     
                                    
    )
    
)

    


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ### functions ###
    
    pal <- wes_palette("Darjeeling1")[c(1,5)]
    
    dotplot <- function(data_dotPlot){
        ggplot(data = data_dotPlot, aes(x = tp, y = term_name, size = precision,label = signif(p_value, digits = 2))) + 
          geom_point()+
          geom_label_repel(fill = "white",
                           size = 5,
                           xlim = c(-Inf, Inf), 
                           ylim = c(-Inf, Inf),
                           nudge_x = .25,
                           nudge_y = .25)+
          # scale_radius(range=c(4, 10))+
          scale_color_viridis(option="turbo")+
          theme_bw() + 
          ylab("") + 
          xlab("") + 
          theme(axis.text.y = element_text(size=10,face="bold"),
            axis.text.x = element_text(size=20,face="bold"),
            plot.title = element_text(size = 15, face = "bold"),
            legend.position="top",
            legend.direction = "horizontal")
    }
    
    
    
    plot_counts <- function(gene_selected){

      nc_df <- readRDS("data_conv.RDS")

      counts <- filter(nc_df, gene == gene_selected) %>%
        pivot_longer(cols=starts_with("FP")) %>%
        dplyr::rename(counts_norm = value) %>%
        mutate(exp=gsub("(.+)\\_.+\\_.+","\\1",name)) %>%
        mutate(condition=gsub(".+\\_(.+)\\_.+","\\1",name)) %>%
        mutate(condition=ifelse(condition == "A", "with bacteria",condition)) %>%
        mutate(condition=ifelse(condition == "B", "no bacteria",condition)) %>%
        mutate(tp=gsub("FP.+\\_.+\\_(.+)h*","\\1",name)) %>%
        mutate(condition_tp=gsub("FP.+\\_(.+\\_)h*","\\1",name)) %>%
        group_by(condition_tp,condition,tp) %>%
        mutate(mean_counts_norm = mean(counts_norm)) %>%
        mutate(sd_mean_counts_norm = sd(counts_norm))
      
      ggplot(data=counts,aes(x=tp,y=counts_norm))+
        geom_pointrange(data=counts, aes(x=tp,y=mean_counts_norm,ymin=mean_counts_norm-sd_mean_counts_norm, ymax=mean_counts_norm+sd_mean_counts_norm,group=condition_tp), width=.1,size=1,position=position_dodge(0.3),shape=3)+
        geom_point(aes(fill=condition),position=position_dodge(0.3),size=5,color="black",shape=21,alpha=0.75)+
        scale_y_continuous(limits=c(0,max(counts$counts_norm)*1.2))+
        theme_bw()+
        scale_fill_manual(values = pal)+
        ggtitle(to_ENSEMBL_name(gene_selected))+
        labs(x="Time (h.a.t.)",y="Normalized counts")+
        theme(axis.text.y = element_text(size=20,face="bold"),
              axis.text.x = element_text(size=20,face="bold"),
              axis.title.x = element_text(size=20,face="bold"),
              axis.title.y = element_text(size=20,face="bold"))
      
      
    }
      
    heatmap <- function(gos,inp,term,list_of_datasets,which_h){
    
      geneset_1 <-  gos %>%
        filter(term_name == term)  %>%
        pull(intersection) %>%
        strsplit(., ",") %>%
        unlist()
    
      normalizedCounts <- readRDS("data.RDS")
      
      cols <- gsub("FP.+\\_(.+\\_.+)$","\\1" ,colnames(normalizedCounts))
      
      if(which_h=="first"){
        coldata <- cols %>%
          as.data.frame() %>%
          mutate(timepoint=gsub(".\\_(.h)","\\1",.)) %>%
          select(!.)
      } else {
      #extract condition and timepoint as new variable "condition_tp"
      coldata <- cols %>%
        as.data.frame() %>%
        mutate(condition=gsub("(.)\\_.+","\\1",.)) %>%
        mutate(condition=ifelse(condition == "A", "with bacteria",condition)) %>%
        mutate(condition=ifelse(condition == "B", "no bacteria",condition)) %>%
        select(!.)
      }
      
      rownames(coldata) <- colnames(normalizedCounts)
      
      # get the expression data for the gene set of interest
      M <- normalizedCounts[rownames(normalizedCounts) %in% geneset_1, ]
      
      M <- M %>%
        as.data.frame() %>%
        select(.,contains(list_of_datasets)) %>%
        tibble::rownames_to_column("gene") %>%
        mutate(gene=to_ENSEMBL_name(gene)) %>%
        tibble::column_to_rownames(var = "gene") %>%
        as.matrix()
      
      pheatmap(log2(M+1),
               color=brewer.pal(11,"PiYG"),
               show_rownames = TRUE,
               annotation_col  = coldata,
               # annotation_colors = c(wes_palette("GrandBudapest1")[4],wes_palette("GrandBudapest2")[4]),
               show_colnames = FALSE,
               fontsize_row = 8,
               scale = 'row', 
               cutree_cols = 2, 
               cutree_rows = 2,
               cellheight=11,
               cellwidth = 15,
               main=term
      )  
    }
    
    #this function can results in NAs
    to_ENSEMBL_name <- function(input){
      gconvert(input,
               organism = "celegans",
               target = "ENSG",
               numeric_ns = "",
               #show only one result
               mthreshold = 1,
               filter_na = FALSE
      )$name
    }
    #################
    
    output$pcaPlot <- renderPlot({
        data_pca <- readRDS("pca_data.RDS")
    
        data_pca %>%
            mutate(tp=gsub(".*\\_(.*)h","\\1",group)) %>%
            dplyr::rename(timepoint=tp) %>%
            mutate(condition=gsub("(.*)\\_.*","\\1",group)) %>%
            mutate(exp=gsub("(.*)\\_.*\\_.*","\\1",name)) %>%
            mutate(exp_condition=gsub("(.+)\\_.*","\\1",name)) %>%
            mutate(condition=ifelse(condition == "A", "with bacteria","no bacteria")) %>%
            ggplot(., aes(PC1,PC2))+
                geom_path(aes(group=exp_condition),alpha=0.25)+
                geom_point(aes(shape=timepoint, fill=condition),size=6,alpha=0.75)+
                scale_shape_manual(values=c(21,22,24,23)) +
                scale_fill_manual(values=pal[2:1])+
                ggtitle(paste0("\n500 most variable genes as input."))+
                # geom_label_repel(fill = "white", xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))+
                theme_bw()+
                guides(fill=guide_legend(override.aes=list(shape=21)))+
                theme(plot.title = element_text(size = 20, face = "bold"))
        
    })
    
    output$dotPlot_first <- renderPlot({
        inp <-input$timepoint_comp
        inp_clean <- gsub("\\_", " ",gsub("enriched_go_terms_(.+)","\\1", inp))
        file_to_load <- paste0(inp, ".txt")
        data_dotPlot_first <- read_csv2(file_to_load) %>%
            mutate(tp=inp_clean) %>%
            top_n(-5,p_value) %>%
            mutate(term_name=paste0(term_name, "\n(", term_id, ")")) %>%
            #quick workaround for parsing very long GOterm names
            mutate(term_name = paste(substring(term_name, 0,60), "\n", substring(term_name, 61,nchar(term_name))))
                   
        dotplot(data_dotPlot_first)
        }, width=600)
    
    output$plot_counts_first <- renderPlot({
      gene_selected <- input$gene_selected
      plot_counts(gene_selected)
    }, width=500)
    
    
    output$dotPlot_second<- renderPlot({
        inp <- input$condition_comp_tp
        data_dotPlot_second <- read_csv2("enriched_go_terms_A_vs_B.txt") %>%
            filter(tp == inp) %>%
            top_n(-5,p_value) %>%
            mutate(term_name=paste0(term_name, "\n(", term_id, ")"))
        
        dotplot(data_dotPlot_second)
        }, width= 500)
    
    output$heatmap_first<- renderPlot({
      
      validate(
        need(input$go_selected_first, "Please click on a GO-term circle to display genes in heatmap.")
      )
      inp <-input$timepoint_comp
      file_to_load <- paste0(inp, ".txt")
      inp_clean <- gsub("\\_", " ",gsub("enriched_go_terms_(.+)","\\1", inp))
      
      gos <- read_csv2(file_to_load) %>%
        mutate(tp=inp_clean) %>%
        top_n(-5,p_value) %>%
        mutate(term_name=paste0(term_name, "\n(", term_id, ")")) %>%
        #quick workaround for parsing very long GOterm names
        mutate(term_name = paste(substring(term_name, 0,60), "\n", substring(term_name, 61,nchar(term_name))))
      
      
      
      term <- nearPoints(gos,input$go_selected_first,threshold = 75,maxpoints = 1)$term_name
      
      if(inp == "enriched_go_terms_diff_in_both_0h_vs_3h"){
        list_of_datasets <- c("A_0h","B_0h","A_3h","B_3h")
      } else if(inp == "enriched_go_terms_diff_in_both_0h_vs_6h"){
        list_of_datasets <- c("A_0h","B_0h","A_6h","B_6h")
      } else if(inp == "enriched_go_terms_specific_for_A_6h_vs_9h"){
        list_of_datasets <- c("A_6h","A_9h")
      }
      
      heatmap(gos,inp,term,list_of_datasets,"first")
      
      
    },  height = 1250 )
    
    output$heatmap_second<- renderPlot({
      
        validate(
          need(input$go_selected_second, "Please click on a GO-term circle to display genes in heatmap.")
        )
        inp <- input$condition_comp_tp
        gos <- read_csv2("enriched_go_terms_A_vs_B.txt") %>%
          filter(tp == inp) %>%
          top_n(-5,p_value) %>%
          mutate(term_name=paste0(term_name, "\n(", term_id, ")"))
      
 

        term <- nearPoints(gos,input$go_selected_second,threshold = 75,maxpoints = 1)$term_name

        heatmap(gos,inp,term,c(paste0("A_",inp),paste0("B_",inp)),"second")

    },  height = 1500 )
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
