# Libraries ---------------------------------------------------------------

library(tidyverse)
library(GGally)
library(ggpubr)
library(reticulate)
library(umap)
library(RColorBrewer)
library(stringr)
library(reshape2)

# Load main tables --------------------------------------------------------

# Metadata

metadata_all <- read.csv('/srv/shiny-server/vmap/Results/metadata_all.csv')


# Diversity

diversity_all <- read.csv('/srv/shiny-server/vmap/Results/diversity_all.csv')
diversity_all_long <- melt(diversity_all, 
                           id.vars = c('specimen','participant_id'),
                           variable.name = 'Metrics',
                           value.name = 'Score')

# Phylotypes

phylotypes <- read.csv('/srv/shiny-server/vmap/Results/phylotypes_all.csv')
load('/srv/shiny-server/vmap/Results/umap_dfs.RData') # umap_dfs

# CST

cst_alluvial <- read.csv('/srv/shiny-server/vmap/Results/cst2alluvia.csv')

# Phylotypes Heatmap Taxonomy

load('/srv/shiny-server/vmap/Results/heatmap_dfs.RData')
phylo_specie <- grep(pattern = 'pt_.*',
                     colnames(heatmap_dfs$specimen),
                     value = T)

# Studies information

studies <- read.delim(file = '/srv/shiny-server/vmap/Results/studies.csv', sep = ';',check.names = F)

# Create final metadata ---------------------------------------------------

all(phylotypes$specimen == diversity_all$specimen) # TRUE Same order
rownames(metadata_all) <- metadata_all$specimen
metadata <- metadata_all[phylotypes$specimen,]
metadata$Trimester <- as.character(metadata$Trimester)
dim(metadata) # 3909

all(metadata$specimen == diversity_all$specimen) # TRUE
metadata %>% select(Type,Age,participant_id,project) %>% distinct() %>% count(project,Type)


# Functions

umap2plot <- function(md,umap, sampletype){
  
  to_plot <- umap %>%
    inner_join(md, by=sampletype)
  
  return(to_plot)
}


metadataSelection <- function(metadata,sampleType){
  
  if (sampleType == 'participant_id'){
    
    df <- do.call('rbind', lapply(unique(metadata$participant_id),function(my_dup){
      
      i <- which.max(metadata[metadata$participant_id == my_dup,'Trimester'])
      metadata[metadata$participant_id == my_dup,][i,]
      
    }))
    
  } else {
    df <- metadata
  }
  
  return(df)
  
}


diversitySelection <- function(diversity_all,sampleType){
  
  if (sampleType == 'participant_id'){
    
    df <- diversity_all[,colnames(diversity_all) != 'specimen'] %>% 
      group_by(participant_id) %>%
      summarise(across(everything(), mean), .groups = 'drop')
    
  } else {
    df <- diversity_all[,colnames(diversity_all) != 'participant_id']
  }
  
  return(df)
  
}

umapSelection <- function(phylotypes_umap,sampleType){
  
  if (sampleType == 'participant_id'){
    
    phylotypes_umap$participant_id <- gsub('-.*','', rownames(phylotypes_umap))
    
    df <- phylotypes_umap %>% 
      group_by(participant_id) %>%
      summarise(across(everything(), mean), .groups = 'drop')
    df <- df %>% column_to_rownames(., var = 'participant_id')
    
  } else {
    df <- phylotypes_umap
  }
  
  return(df)
  
}


phyloSelection <- function(dfs,sampleType){
  
  phylo <- heatmap_dfs[[sampleType]]
  
  return(phylo)
  
}


# Colors
my_colors_race <- brewer.pal(6,'Dark2')
names(my_colors_race) <- unique(metadata$Race)

my_colors_project <- brewer.pal(12,'Paired')
names(my_colors_project) <- unique(metadata$project)

my_colors_age <- brewer.pal(5,'Dark2')
names(my_colors_age) <- unique(metadata$Age)

my_colors_trimester <- brewer.pal(5,'Dark2')
names(my_colors_trimester) <- unique(metadata$Trimester)

my_colors_type <- c('term' = 'grey',
                    'preterm' = '#72467c',
                    'early' = 'mediumpurple')

my_colors_cst <- brewer.pal(7,'Set1')
names(my_colors_cst) <- unique(cst_alluvial$CST)

my_colors = list(
  'project' = my_colors_project,
  'Race' = my_colors_race,
  'Type' = my_colors_type,
  'Age' = my_colors_age,
  'Trimester' = my_colors_trimester,
  'CST' = my_colors_cst
)
