# Server

server <- function(input, output, session) {

##### Boris start

   ### Landing Page
  observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE,
               eventExpr = NULL, {
               showModal(modalDialog(
                   title = "Rshiny App Data Use Agreement", fade = FALSE,
                   wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",
                             HTML(paste0("<h3 style=\"text-align:center\">TERMS AND CONDITIONS OF DATA USE</h3>",
                                         "<p><br><b>By clicking the acceptance button or accessing, using or downloading any part of the Tool (as defined below) You expressly agree to and hereby consent to be bound by all of the Terms and Conditions below.</b></p>",
                                         "</br><p>UCSF agrees to provide You with visualized data comprised of vaginal microbiome data including associated metadata (e.g., age range, race, birth outcome) and derived features (e.g., diversity metrics, VALENCIA community state types) (\"Data\") displayed in VMAP (\"Tool\").</p>",
                                         "<br><b>1. Your Obligations.</b> </br>",
                                         "<ol type=\"a\"><li>You represent and warrant that you are over the age of eighteen (18) and have the power and authority to enter into and perform your obligations under these Terms and Conditions.</li>",
                                         "<li>You may not use and disclose the Data outside of this Tool or its Terms and Conditions;</li>",
                                         "<li>You agree to use the Data, and to protect the privacy and security ofthe Data, according to all applicable laws and regulations, by commercially-acceptable mean, and no less rigorously than you protect your own sensitiveinformation, but in no case less than reasonable care. You shall implement, maintain and use appropriate administrative, technical and physical security measures to preserve the security, integrity and availability of the Data. All Data stored on portable devices or media must be encrypted in accordance with the Federal Information Processing Standards (FIPS) Publication 140-2. You shall ensure that such security measures are regularly reviewed and revised to address evolving threats and vulnerabilities while You have responsibility for the Data under these Terms and Conditions.</li>",
                                         "<li>You shall not use the Data for commercial purposes, including any transfer, sale, or lease to a commercial entity.</li>",
                                         "<li>You shall not attempt to identify or contact any subjects/patients whose Data are portrayed in the Tool.  You may not use the Tool, either alone or in combination with any other data that is currently or in the future available to You, to identify an individual whose information may be included within the Tool, nor enable any third party to identify any individual whose information is included in the Tool. You shall notify UCSF immediately if you identify an individual, or if You become aware that a third party has done so. You may not link, combine, hash, concatenate, aggregate or add together data contained within the Tool with any other data sources or information without the express written approval of UCSF.</li>",
					"<li>You may, consistent with academic standards, cite the data in publications or presentations provided that you appropriately cite the contributions of UCSF using customary standards of scientific attribution</li></ol>",
                                         "</br><b>2. Breach and Enforcement.</b></br>",
                                         "<ol type=\"a\"><li><em>Breach.  </em>You shall report any confirmed or suspected breach of these Terms and Condition to UCSF (<a href=\"mailto:industrycontracts@ucsf.edu\">industrycontracts@ucsf.edu</a>) promptly upon discovery, both orally and in writing, but in no event more than two (2) business days after You reasonably believe that a breach has occurred. Your report shall identify: (i) the nature of the unauthorized access, use or disclosure, (ii) the Data accessed, used or disclosed, (iii) the person(s) who accessed, used and disclosed and/or received Data (if known), (iv) what You have done or will do to mitigate any deleterious effect of the unauthorized access, use or disclosure, and (v) what corrective action You have taken or will take to prevent future unauthorized access, use or disclosure. You shall provide such other information, including a written report, as reasonably requested by UCSF. In the event of a suspected breach, You shall keep UCSF informed regularly of the progress of its investigation until the uncertainty is resolved.</li>",
                                         "<li><em>Indemnification.  </em>You shall indemnify, hold harmless and defend UCSF from and against any and all claims, losses, liabilities, costs and other expenses, including attorney fees, resulting from, or relating to, Your acts or omissions in connection with Your representations, duties and obligations in these Terms and Conditions.</li></ol>",
                                         "</br><b>3. Use of Name.</b></br>",
                                         "<ol type=\"a\"><li>You shall not use the UCSF name or names of UCSF's employees in any advertisement, press release, or other publicity without prior written approval of UCSF.</li>",
                                         "<li>You understand that the California Education Code section 92000 provides that the name \"University of California\" is the property of the State and that no person shall use that name without permission of The Regents of the University of California. Such permission may be granted by the Chancellor or his designee.</li></ol>",
                                         "</br><b>4. Disclaimers.</b></br>",
                                         "<ol type=\"a\"><li>THE DATA IS BEING SUPPLIED TO YOU WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION AND WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  IN PARTICULAR, UCSF DOES NOT REPRESENT OR WARRANT THAT THE USE OF THE MATERIALS WILL NOT INFRINGE OR VIOLATE ANY PATENT OR PROPRIETARY RIGHTS OF THIRD PARTIES.</li>",
                                         "<li>To the extent permitted by law, You assume all liability for damages which may arise from Your use, storage or disposal of the Data.  UCSF (including, but not limited to, its directors, trustees, officers employees, students and agents, as applicable) will not be liable to You for any loss, claim or demand made by or against You, due to or arising from Your use of the Data, except to the extent caused by the gross negligence or willful misconduct of UCSF.</li></ol>"
                                         ))),
                   footer = modalButton("I Agree."), easyClose = FALSE
               ))
          }) 

##### Boris end
  
  metadataType <- reactive({
    
    metadata <- metadataSelection(metadata,input$sample)
    
  }) %>%
    bindCache(input$sample)
  
  
  
  metadataInput <- reactive({
    
    df <- metadataType() %>%
      filter((Age %in% input$Age) &
               project%in%input$projects &
               Type%in%input$type &
               Race %in% input$Race &
               Trimester %in% input$trimester)

  })


  cstInput <- reactive({
    
   cst_alluvia2 <- cst_alluvial %>%
     filter((Age %in% input$Age) &
              project%in%input$projects &
              Type%in%input$type &
              Race %in% input$Race &
              Trimester %in% input$trimester)

  })
  
  
  # Barplot Outcome
  
  output$bpType <- renderPlot({
    
    df <- metadataInput()
    plot <- data.frame(table(df[,c('Type',input$feature)]))
    
    ggplot(plot,
                   aes(x = Type, fill = plot[,input$feature], y = Freq)) +
              geom_bar(position = 'fill', stat = 'identity', color = 'black') +
              scale_fill_manual(labels=c("term" = "Term",
                                         "preterm" = "Preterm",
                                         "early" = "Early Preterm",
                                         "American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                         "Asian" = "Asian",
                                         "Black or African American" = "Black or African\nAmerican",
                                         "Others" = "Others",
                                         "Unknown" = "Unknown",
                                         "White" = "White"
              ),values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'Frequency', x = '')+
              theme_bw() + 
              theme(axis.text = element_text(size = 15, angle = 90),
                    axis.title = element_text(size = 25),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 20)) +
              scale_x_discrete(labels=c("term" = "Term",
                                        "preterm" = "Preterm",
                                        "early" = "Early Preterm",
                                        "American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                        "Asian" = "Asian",
                                        "Black or African American" = "Black or African\nAmerican",
                                        "Others" = "Others",
                                        "Unknown" = "Unknown",
                                        "White" = "White"
              ))
    
  })
  
  # Barplot Race
  
  output$PCrace <- renderPlot({
    
    df <- metadataInput()
    plot <- data.frame(table(df[,c('Race',input$feature)]))
    
    ggplot(plot,
                   aes(x = Race, fill = plot[,input$feature], y = Freq)) +
              geom_bar(position = 'fill', stat = 'identity', color = 'black') +
              scale_fill_manual(labels=c("term" = "Term",
                                         "preterm" = "Preterm",
                                         "early" = "Early Preterm",
                                         "American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                         "Asian" = "Asian",
                                         "Black or African American" = "Black or African\nAmerican",
                                         "Others" = "Others",
                                         "Unknown" = "Unknown",
                                         "White" = "White"),
                                         values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'Frequency', x = '')+
              theme_bw() + 
              theme(axis.text = element_text(size = 15, angle = 90),
                    axis.title = element_text(size = 25),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 20)) +
              scale_x_discrete(labels=c("American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                        "Asian" = "Asian",
                                        "Black or African American" = "Black or African\nAmerican",
                                        "Others" = "Others",
                                        "Unknown" = "Unknown",
                                        "White" = "White"
                                        ))
  })
  
  # Barplot Project
  
  output$bpProject <- renderPlot({
  
    df <- metadataInput()
    plot <- data.frame(table(df[,c('project',input$feature)]))
    
    ggplot(plot,
           aes(x = project, fill = plot[,input$feature], y = Freq)) +
      geom_bar(position = 'fill', stat = 'identity', color = 'black') +
      scale_fill_manual(labels=c("term" = "Term",
                                 "preterm" = "Preterm",
                                 "early" = "Early Preterm",
                                 "American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                 "Asian" = "Asian",
                                 "Black or African American" = "Black or African\nAmerican",
                                 "Others" = "Others",
                                 "Unknown" = "Unknown",
                                 "White" = "White"),
                                 values = my_colors[[input$feature]]) +
      labs(fill = input$feature, y = 'Frequency', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20))
    
  })
  
  
  diversityType <- reactive({
    
    df <- diversitySelection(diversity_all,input$sample)
    
  })
  
  output$my_race <- renderUI(HTML(markdown(
    
    
    ' #### Race categories
    
      Other category contains:
      - American Indian or Alaska Native
      - Native Hawaiian or Other Pacific Islander
      - Asian
      
    '
    
  )))
  
  output$studies <- renderTable(studies)
  
  
  # Pairplot diversity
  
 
  output$cpDiversity <- renderPlot({
    
   to_plot <- merge(diversityType(),
                             metadataInput(),
                             by=input$sample)

    ggpairs(data = to_plot, columns = input$metrics,
            aes(color = to_plot[,input$feature]),
            upper = list(continuous = wrap("cor", size = 6)))+
       scale_fill_manual(values = my_colors[[input$feature]]) +
      scale_color_manual(values = my_colors[[input$feature]]) +
      theme_bw()+
      theme(strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            legend.position = 'bottom')
    


  })
  
  # Violin Plot diversity
  
  output$vpDiversity <- renderPlot({
    
    md <- metadataInput()

    diversity_all_long <- melt(diversityType(), 
                              id.vars = input$sample,
                              variable.name = 'Metrics',
                              value.name = 'Score')
   
    
    diver_md <- merge(x = md, y = diversity_all_long,
                      by = input$sample)
    
    diver_md <- diver_md[diver_md$Metrics%in%input$metrics,]

    list_plot <- lapply(unique(diver_md[,'Metrics']), function(my_metric){
      to_plot <- diver_md[diver_md$Metrics == my_metric,]
      ggplot(data = to_plot,
             aes(x = Metrics, y = Score,
                 fill = to_plot[,input$feature]))+
        geom_violin()+
        scale_fill_manual(values = my_colors[[input$feature]]) +
        labs(fill = input$feature, y = 'Score', x = '')+
        theme_bw() +
        theme(axis.text = element_text(size = 25),
              axis.title = element_text(size = 25),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 20))

    })

    ggarrange(plotlist = list_plot, ncol=length(list_plot),
              common.legend = TRUE, legend="right")

  })
  
  
  # Legend text
  
  output$my_text <- renderUI(HTML(markdown(
    
    
    ' #### Diversity Metrics
    
      - Balance weighted phylogenetic diversity (bwpd)
      - Inverse Simpson (inv_simpson)
      - Phylogenetic entropy (phylo_entropy)
      - Quadratic (quadratic)
      - Rooted phylogenetic diversity (rooted_pd)
      - Shannon (shannon)
      - Unrooted phylogenetic diversity (unrooted_pd)'
    
  )))


  # CST alluvial plot

  output$apCST <- renderPlot({

   cst_alluvia2 <- cstInput()
   values <- unique(cst_alluvia2[,input$feature])

    list_df <- lapply(values, function(x){

      df <- cst_alluvia2[cst_alluvia2[,input$feature] == x,]

      to_plot <- df[,c("Trimester","participant_id","CST")]
      df_freq <- to_plot %>% group_by(participant_id,Trimester,CST) %>% summarise(Freq = n())
      df_freq <- df_freq %>%
        group_by(Trimester) %>%
        mutate(pct = Freq / sum(Freq)*100)


      p <- ggplot(df_freq,
                  aes(x = Trimester, stratum = CST, alluvium = participant_id,
                      fill = CST, label = CST, y = pct)) +
        scale_fill_manual(values = my_colors[['CST']], limits = names(my_colors[['CST']])) +
        geom_flow(stat = "alluvium", lode.guidance = "frontback") +
        geom_stratum() +
        scale_x_discrete(breaks=c("1","2","3")) +
        ggtitle(x)+
        theme_bw()+
        ylab("Freq")+
        theme(legend.position = "none", text = element_text(size = 20),
              plot.title = element_text(hjust = 0.5))
      return(p)

    })


    ggarrange(plotlist = list_df, ncol=length(list_df),
              common.legend = TRUE, legend="right")

  })

  
  umapInput <-  reactive({
    
    umap2plot(metadataInput(),umap_dfs[[input$sample]], input$sample)

  }) %>%
    bindCache(metadataInput(), input$sample)
  
  
  # UMAP phylotype plot

  output$upPhylo <- renderPlot({

    toplot <- umapInput()
    ggplot(toplot, aes(x = UMAP1,
                                y = UMAP2,
                       color = toplot[,input$feature]))+
              geom_point(size = 4) +
              scale_color_manual(values = my_colors[[input$feature]])+
              labs(x = "UMAP1",
                   y = "UMAP2",
                   color = "")+
              theme_bw() +
      theme(legend.position = "bottom",text = element_text(size = 20),
            legend.text = element_text(size=15)) +
      guides(fill=guide_legend(nrow=3,byrow=TRUE))


  })
  
  
  phyloType <- reactive({
    phyloSelection(heatmap_dfs,input$sample)
  }) %>% bindCache(input$sample)


  # Heatmap phylotype plot

  output$hmPhylo <- renderPlot({

    metadata <- metadataInput()

    list_plots <- lapply(sort(input$trimester), function(my_tri){

      metadata_trimester <- metadata[metadata$Trimester == my_tri,]
      phylo_trimester <- merge(phyloType(), metadata_trimester[,c(input$sample,input$feature)],
                               by = input$sample)

      test <- do.call('cbind',lapply(sort(unique(metadata_trimester[,input$feature])), function(my_feat){

        # print(my_feat)

        phylo_feat <- phylo_trimester[phylo_trimester[,input$feature] == my_feat,
                                      c(phylo_specie,input$sample)] %>%
          remove_rownames %>% column_to_rownames(var = input$sample)

        if(nrow(phylo_feat) > 0 ){

          counts <- as.data.frame(apply(phylo_feat>0,2,sum) / nrow(phylo_feat))
          colnames(counts) <- my_feat
        } else{

          warning(paste0('No information for', my_feat, collapse = ' '))

        }

        return(counts)

      }))

      if(ncol(test) == 1) {
        
        id <- colnames(test)
        test$ID <- rownames(test)
        test <- test[input$Specie,]
        test_sorted <- test[order(test[,id], decreasing = F),]
        
      } else {
        test_sorted <- test[names(sort(apply(test[input$Specie,],1,mean), decreasing = F)),]
        test_sorted$ID <- rownames(test_sorted)
      }

      to_plot <- melt(test_sorted, id.vars = 'ID') %>% rename(!!input$feature := variable, Counts = value) %>%
        arrange(Counts)
      to_plot$Counts <- round(to_plot$Counts*100,0)
      to_plot$ID <- factor(to_plot$ID,levels = rownames(test_sorted))

      plot <- ggplot(to_plot, aes(x = to_plot[,input$feature], y = ID, fill = Counts)) +
        xlab(input$feature) +
        geom_tile(color='black')+
        scale_fill_gradient(low = "white", high = "purple", limits = c(0,100))+
        geom_text(aes(label = Counts), color = 'black',size = 6)+
        theme_bw()+
        labs(fill = 'Percentage') +
        ggtitle(paste0('Trimester ',my_tri)) +
        theme(axis.text = element_text(size = 15),
              axis.title.y = element_blank(),
              axis.title.x  = element_text(size = 20),
              legend.text = element_text(size = 15),
              legend.title = element_text(size = 15),
              plot.title = element_text(size = 20))

      if (my_tri != 3)
      {plot = plot+theme(axis.title.x = element_blank())}

      return(plot)

    })

    ggarrange(plotlist = list_plots, nrow=length(list_plots),
              common.legend = TRUE, legend="right")


  })
  
}
