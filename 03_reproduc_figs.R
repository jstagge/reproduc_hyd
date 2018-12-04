# *------------------------------------------------------------------
# | PROGRAM NAME: 03_reproduc_figs
# | FILE NAME: 03_reproduc_figs.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code generates all figures based on data from 
# | 02_reproduc_data_handling
# | 
# |
# *------------------------------------------------------------------

###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "./data"
output_path <- "./output"

global_path <- "./global_func"
function_path <- "./functions"

### Set output location
write_output_base_path <- output_path
dir.create(write_output_base_path, showWarnings = FALSE)


###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
#require(colorout)
require(assertthat)
require(staggefuncs)
require(tidyverse)
#require(colorblindr)

### Load these functions for this unique project
require(stringr)
require(ggridges)
require(ggthemes)
require(riverplot)
require(tspmeta)
### Fix the select command
select <- dplyr::select

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

### Load global functions
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)

###########################################################################
## Set Initial Values
###########################################################################
### Order modified for clarity
journal_abbrev <- c("EM&S", "HESS", "WRR", "JoH", "JAWRA", "JWRP&M")
journal_colors <- cb_pal("custom", n=6, sort=FALSE)

###########################################################################
## Set Additional Output Folders
###########################################################################
### Set up output folders
write_figures_path <- file.path(write_output_base_path, "all_figures")
dir.create(write_figures_path, recursive=TRUE, showWarnings = FALSE)

### Set up output folders
write_pub_path <- file.path(write_output_base_path, "publication_figures")
dir.create(write_pub_path, recursive=TRUE, showWarnings = FALSE)

###########################################################################
## Load data
###########################################################################
load(file=file.path(write_output_base_path, "survey_analysis/reproduc_data.rda"))

###########################################################################
## Create column with availability / reproducibility testing
###########################################################################
reproduc_df <- reproduc_df %>%
	mutate(rep_avail_clean = case_when(
		rep_avail == "repro" ~ "repro",
		TRUE ~ "avail")
	)
                           
###########################################################################
## Reorder journals using better grouping
###########################################################################                       
reproduc_df$Q2_abbrev <- factor(reproduc_df$Q2_abbrev, levels=c("EM&S", "HESS", "WRR", "JoH", "JAWRA", "JWRP&M"))

###########################################################################
###  Plot keywords by Journal
###########################################################################
### Set up output folders
write_output_path <- file.path(write_figures_path, "articles")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)

### Summarize articles sampled
plot_articles_sample <- reproduc_df %>%
	filter(rep_avail_clean == "avail") %>%
 	select(keyword, Q2_abbrev) %>%
 	group_by(Q2_abbrev, keyword) %>% 
 	summarise(count = n())

plot_articles_sample$keyword <- factor(plot_articles_sample$keyword, levels=c(TRUE, FALSE), labels=c("Keyword", "None"))

### Create a table for labels
plot_articles_sample_spread <- plot_articles_sample %>% 
	spread(keyword, count) %>%
	mutate(Total = Keyword + None)
#pub_table$labels <- paste0(pub_table$total, " [", pub_table$keyword, " : ", pub_table$none,"]")
plot_articles_sample_spread$labels <- paste0(plot_articles_sample_spread$Keyword, " : ", plot_articles_sample_spread$None," [", plot_articles_sample_spread$Total, "]")
#pub_table$labels <- paste0(" [", pub_table$total, "] ", pub_table$keyword, " : ", pub_table$none)
	
### Plot black and white
  p <- ggplot(data = plot_articles_sample, aes(x = Q2_abbrev)) 
  p <- p + geom_bar(aes(y = count, fill = keyword), stat = 'identity')
  p <- p + geom_text(data=plot_articles_sample_spread, aes(y = Total + 3, label=labels), vjust=0, size=2.3)
  p <- p + scale_y_continuous(name="Sampled Articles", expand = c(0, 0), limits=c(0,110), breaks=seq(0,160,20))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Keyword", values=c("grey20", "grey70"))
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p 
  
### Save figure
ggsave(file.path(write_output_path, "article_sample_keyword_by_journal_bw.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(write_output_path, "article_sample_keyword_by_journal_bw.svg"), p, width=5, height=3)
ggsave(file.path(write_output_path, "article_sample_keyword_by_journal_bw.pdf"), p, width=5, height=3)

	
plot_articles_sample$fill <- paste0(plot_articles_sample$Q2_abbrev, "--", plot_articles_sample$keyword)

journal_colors_black <- rep(journal_colors, each=2)
journal_colors_black[seq(1, length(journal_colors_black), 2)] <- "grey30"
journal_colors_black

### Plot as separate bars 
  p <- ggplot(data = plot_articles_sample, aes(x = Q2_abbrev)) 
  p <- p + geom_bar(aes(y = count, fill = fill), stat = 'identity')
  p <- p + geom_text(data=plot_articles_sample_spread, aes(y = Total + 3, label=labels), vjust=0, size=2.3)
  p <- p + scale_y_continuous(name="Sampled Articles", expand = c(0, 0), limits=c(0,110), breaks=seq(0,160,20))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Keyword", values=journal_colors_black)
  p <- p + theme_classic_new(9.5) +   theme(legend.position="none")
  p 
   
### Check color-deficient
#cvd_grid(p)

### Save figure
ggsave(file.path(write_output_path, "article_sample_keyword_by_journal_color.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(write_output_path, "article_sample_keyword_by_journal_color.svg"), p, width=5, height=3)
ggsave(file.path(write_output_path, "article_sample_keyword_by_journal_color.pdf"), p, width=5, height=3)

###########################################################################
## Plot population papers by keyword
###########################################################################
pub_summary_table$keyword_prop <- pub_summary_table$keyword / pub_summary_table$total
#pub_table$labels <- paste0(pub_table$total, " [", pub_table$keyword, " : ", pub_table$none,"]")
pub_summary_table$labels <- paste0(pub_summary_table$keyword, " : ", pub_summary_table$none," [", pub_summary_table$total, "]")
#pub_table$labels <- paste0(" [", pub_table$total, "] ", pub_table$keyword, " : ", pub_table$none)

plot_articles_population <- pub_summary_table %>%
 	select(-journal, -X, -total, -labels, -keyword_prop) %>%
 	gather(keyword, count, -journal_abbrev)

plot_articles_population$keyword[plot_articles_population$keyword == "keyword"] <- "Keyword"
plot_articles_population$keyword[plot_articles_population$keyword == "none"] <- "None"


### Plot black and white
  p <- ggplot(data = plot_articles_population, aes(x = journal_abbrev)) 
  p <- p + geom_bar(aes(y = count, fill = keyword), stat = 'identity')
  p <- p + geom_text(data=pub_summary_table, aes(y = total + 10, label=labels), vjust=0, size=2.3)
  p <- p + scale_y_continuous(name="Articles in 2017", expand = c(0, 0), limits=c(0,720), breaks=seq(0,800,200))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Keyword", values=c("grey20", "grey70"))
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p 

### Save figure
ggsave(file.path(write_output_path, "article_keyword_by_journal_bw.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(write_output_path, "article_keyword_by_journal_bw.svg"), p, width=5, height=3)
ggsave(file.path(write_output_path, "article_keyword_by_journal_bw.pdf"), p, width=5, height=3)


plot_articles_population$fill <- paste0(plot_articles_population$journal_abbrev, "--", plot_articles_population$keyword)


journal_colors_black <- rep(journal_colors, each=2)
journal_colors_black[seq(1, length(journal_colors_black), 2)] <- "grey30"
journal_colors_black

### Plot using same colors
  p <- ggplot(data = plot_articles_population, aes(x = journal_abbrev)) 
  p <- p + geom_bar(aes(y = count, fill = fill), stat = 'identity')
  p <- p + geom_text(data=pub_summary_table, aes(y = total + 10, label=labels), vjust=0, size=2.3)
  p <- p + scale_y_continuous(name="Articles in 2017", expand = c(0, 0), limits=c(0,720), breaks=seq(0,800,200))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Keyword", values=journal_colors_black)
  p <- p + theme_classic_new(9.5) +   theme(legend.position="none")
  p 


#cvd_grid(p)

### Save figure
ggsave(file.path(write_output_path, "article_keyword_by_journal_color.png"), p, width=5, height=3, dpi=600)
ggsave(file.path(write_output_path, "article_keyword_by_journal_color.svg"), p, width=5, height=3)
ggsave(file.path(write_output_path, "article_keyword_by_journal_color.pdf"), p, width=5, height=3)


###########################################################################
###  Create combined figure
###########################################################################
### Merge tables
plot_articles_join <- plot_articles_sample %>% 
	full_join(plot_articles_population, by= c("Q2_abbrev" = "journal_abbrev", "keyword" = "keyword")) %>%
	select(-fill.x, -fill.y) %>%
	as.data.frame()
### Rename columns
names(plot_articles_join) <- c("journal_abbrev", "keyword", "sample", "population")	
plot_articles_join

### Create a total column for plotting that sums non-keyword and keyword for stacking
plot_articles_join_total <- plot_articles_join %>% 
 	group_by(journal_abbrev) %>% 
 	summarise(sample = sum(sample), population = sum(population)) %>%
 	add_column( keyword = "Total", .after="journal_abbrev") %>%
 	as.data.frame()

plot_articles_join <- rbind(plot_articles_join, plot_articles_join_total)

### Make vertical table
yup <- plot_articles_join %>% 	
	gather(source, count, -journal_abbrev, -keyword)

ggplot(yup, aes(x=journal_abbrev, y=count, group=source, fill=keyword)) + geom_bar(stat = "identity",  position = "dodge")

to_plot <- subset(yup, keyword!="None")
to_plot$fill <- paste0(to_plot$journal_abbrev, "_", to_plot$source, "_", to_plot$keyword)
to_plot
to_plot$journal_abbrev <- factor(to_plot$journal_abbrev, levels=journal_abbrev)

#journal_colors_light<- c("#88EE99","#9AFFFF","#AADDFF","#FFB3C4","#F3F385","#FF99DD")
#journal_colors_light<- c("red","blue","purple","green","brown","grey")
journal_colors_light<- c("#88EE99","#9AFFFF","#FFB3C4","#AADDFF","#F3F385","#FF99DD")
#F3F385 - yellow
#FF99DD - pinkish purple
#FFB3C4 - pink
#AADDFF - light blue
#9AFFFF - cyan
#88EE99 - green

journal_colors_black <- rep(journal_colors, each=4)
journal_colors_black[seq(1, length(journal_colors_black), 4)] <- "grey20"
journal_colors_black[seq(3, length(journal_colors_black), 4)] <- "grey20"
journal_colors_black[seq(4, length(journal_colors_black), 4)] <- journal_colors_light
journal_colors_black

  p <- ggplot(to_plot, aes(x=journal_abbrev, y=count, group=source, fill=fill)) 
  p <- p + geom_bar(data=subset(to_plot, keyword == "Total"), stat = "identity",  position = "dodge", colour="black", size=0.2)
    p <- p + geom_bar(data=subset(to_plot, keyword == "Keyword"), stat = "identity",  position = "dodge")
#  p <- p + geom_text(data=pub_summary_table, aes(y = total + 10, label=labels), vjust=0, size=2.3)
  p <- p + scale_y_continuous(name="Articles", expand = c(0, 0), limits=c(0,720), breaks=seq(0,800,100))
  p <- p + scale_x_discrete(name="Journal")
  #p <- p + scale_fill_manual(name="Keyword", values=journal_colors_black)
  p <- p + scale_fill_manual(name="Keyword", values=journal_colors_black)
  p <- p + theme_classic_new(9.5) +   theme(legend.position="none")
  p 

### Save figure
ggsave(file.path(write_output_path, "article_keyword_by_journal_combined_color.png"), p, width=5.5, height=3.5, dpi=600)
ggsave(file.path(write_output_path, "article_keyword_by_journal_combined_color.svg"), p, width=5.5, height=3.5)
ggsave(file.path(write_output_path, "article_keyword_by_journal_combined_color.pdf"), p, width=5.5, height=3.5)

#cvd_grid(p)

journal_colors_black <- rep("grey60", each=4*6)
journal_colors_black[seq(1, length(journal_colors_black), 4)] <- "grey20"
journal_colors_black[seq(3, length(journal_colors_black), 4)] <- "grey20"
journal_colors_black[seq(4, length(journal_colors_black), 4)] <- "grey85"
journal_colors_black

  p <- ggplot(to_plot, aes(x=journal_abbrev, y=count, group=source, fill=fill)) 
  p <- p + geom_bar(data=subset(to_plot, keyword == "Total"), stat = "identity",  position = "dodge", colour="black", size=0.2)
    p <- p + geom_bar(data=subset(to_plot, keyword == "Keyword"), stat = "identity",  position = "dodge", colour="black", size=0.1)
#  p <- p + geom_text(data=pub_summary_table, aes(y = total + 10, label=labels), vjust=0, size=2.3)
  p <- p + scale_y_continuous(name="Articles", expand = c(0, 0), limits=c(0,720), breaks=seq(0,800,100))
  p <- p + scale_x_discrete(name="Journal")
  #p <- p + scale_fill_manual(name="Keyword", values=journal_colors_black)
  p <- p + scale_fill_manual(name="Keyword", values=journal_colors_black)
  p <- p + theme_classic_new(9.5) +   theme(legend.position="none")
  p 

### Save figure
ggsave(file.path(write_output_path, "article_keyword_by_journal_combined_bw.png"), p, width=5.5, height=3.5, dpi=600)
ggsave(file.path(write_output_path, "article_keyword_by_journal_combined_bw.svg"), p, width=5.5, height=3.5)
ggsave(file.path(write_output_path, "article_keyword_by_journal_combined_bw.pdf"), p, width=5.5, height=3.5)


###########################################################################
###  Plot Stated Availability  (Q5)
###########################################################################
### Set up output folders
write_output_path <- file.path(write_figures_path, "availability")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)


avail_journal <- reproduc_df %>% 
	filter(rep_avail_clean == "avail") %>%
	dplyr::select(Q2_abbrev, Q5) %>%
	count(Q2_abbrev, Q5) %>%  
	mutate(prop_all = prop.table(n)) %>%
	group_by(Q2_abbrev) %>% 
	mutate(prop_by_journal = n/sum(n)) %>% 
	as.data.frame()

### Reorder the levels for question Q5
avail_journal$Q5 <- factor(avail_journal$Q5, levels=levels(avail_journal$Q5)[c(2,1,3)])
avail_journal$Q5_leading <- factor(avail_journal$Q5, levels=levels(avail_journal$Q5)[c(3,2,1)])

### Plot as separate bars  
  p <- ggplot(data = avail_journal, aes(x = Q2_abbrev, y = prop_by_journal, fill = Q5_leading)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
  p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Stated Availability (Q5)", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p 
  
#cvd_grid(p)

### Save figure
ggsave(file.path(write_output_path, "q5_colors.png"), p, width=5, height=3.25, dpi=600)
ggsave(file.path(write_output_path, "q5_colors.svg"), p, width=5, height=3.25)
ggsave(file.path(write_output_path, "q5_colors.pdf"), p, width=5, height=3.25)

### Alternative colors
#p <- p + scale_fill_manual(name="Stated Availability (Q5)", values=c("#66c2a5", "#8da0cb", "#fc8d62"), limits=levels(avail_journal$Q5_leading)) 
#p <- p + scale_fill_manual(name="Stated Availability (Q5)", values=cb_pal("wong", n=3, sort=FALSE)[c(3, 2, 1)], limits=levels(avail_journal$Q5_leading)) 

p <- p + scale_fill_manual(name="Stated Availability (Q5)", values=c("grey10", "grey40", "grey80"), limits=levels(avail_journal$Q5_leading)) 
### Save figure
ggsave(file.path(write_output_path, "q5_bw.png"), p, width=5, height=3.25, dpi=600)
ggsave(file.path(write_output_path, "q5_bw.svg"), p, width=5, height=3.25)
ggsave(file.path(write_output_path, "q5_bw.pdf"), p, width=5, height=3.25)


### Plot as stacked bars    	
  p <- ggplot(data = avail_journal, aes(x = Q2_abbrev, y = prop_by_journal, fill = Q5)) 
  p <- p + geom_bar(stat = 'identity', position = 'stack', width=0.7)
  p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Stated Availability (Q5)", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p 
  

#cvd_grid(p)

### Save figure
ggsave(file.path(write_output_path, "q5_stack_colors.png"), p, width=5, height=3.25, dpi=600)
ggsave(file.path(write_output_path, "q5_stack_colors.svg"), p, width=5, height=3.25)
ggsave(file.path(write_output_path, "q5_stack_colors.pdf"), p, width=5, height=3.25)

### Alternative colors
#p <- p + scale_fill_manual(name="Stated Availability (Q5)", values=c("#66c2a5", "#8da0cb", "#fc8d62"), limits=levels(avail_journal$Q5_leading))
#p <- p + scale_fill_manual(name="Stated Availability (Q5)", values=cb_pal("wong", n=3, sort=FALSE)[c(3, 2, 1)], limits=levels(avail_journal$Q5_leading)) 


p <- p + scale_fill_manual(name="Stated Availability (Q5)", values=c("grey10", "grey40", "grey80"), limits=levels(avail_journal$Q5_leading)) 
 
### Save figure
ggsave(file.path(write_output_path, "q5_stack_bw.png"), p, width=5, height=3.25, dpi=600)
ggsave(file.path(write_output_path, "q5_stack_bw.svg"), p, width=5, height=3.25)
ggsave(file.path(write_output_path, "q5_stack_bw.pdf"), p, width=5, height=3.25)

###########################################################################
###  Plot Availability Source (Q6)
###########################################################################
  	
plot_q6 <- q6_journal_perc%>%
  	 gather(key=source, value=value, -Q2_abbrev) %>%
  	 filter(Q2_abbrev !="Total")

plot_q6$source <- factor(plot_q6$source, levels=c("some", "article", "author", "third", "none"), labels=q6_labels)  	
  	 	
p <- ggplot(subset(plot_q6, source != "None"), aes(x=source, y=value, fill=Q2_abbrev)) 
p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, .7))
p <- p + scale_x_discrete(name="Where Available (Q6)")
p <- p + scale_fill_manual(name="Journal", values=journal_colors) 
p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
p 	


#cvd_grid(p)

### Save figure
ggsave(file.path(write_output_path, "q6_by_source.png"), p, width=5, height=3.25, dpi=600)
ggsave(file.path(write_output_path, "q6_by_source.svg"), p, width=5, height=3.25)
ggsave(file.path(write_output_path, "q6_by_source.pdf"), p, width=5, height=3.25)


p <- ggplot(plot_q6, aes(x=Q2_abbrev, y=value, fill=source))
p <- p + geom_bar(stat = 'identity', position = position_stack())
p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1.25))
p <- p + scale_x_discrete(name="Journal")
p <- p + scale_fill_manual(name="Where Available (Q6)", values=cb_pal("wong", n=5, sort=TRUE)) 
p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
p 	

p <- ggplot(subset(plot_q6, source != "None"), aes(x=Q2_abbrev, y=value, fill=source))
p <- p + geom_bar(stat = 'identity', width=0.75, position = position_dodge(width=0.75))
p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 0.7))
p <- p + scale_x_discrete(name="Journal")
p <- p + scale_fill_manual(name="Where Available (Q6)", values=cb_pal("ptol", n=4, sort=TRUE)) 
p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
p 	
### Save figure
ggsave(file.path(write_output_path, "q6_by_journal.png"), p, width=5, height=3.25, dpi=600)
ggsave(file.path(write_output_path, "q6_by_journal.svg"), p, width=5, height=3.25)
ggsave(file.path(write_output_path, "q6_by_journal.pdf"), p, width=5, height=3.25)

p <- p + scale_fill_manual(name="Where Available (Q6)", values=c("grey15", "grey35", "grey60", "grey80"))
### Save figure
ggsave(file.path(write_output_path, "q6_by_journal_bw.png"), p, width=5, height=3.25, dpi=600)
ggsave(file.path(write_output_path, "q6_by_journal_bw.svg"), p, width=5, height=3.25)
ggsave(file.path(write_output_path, "q6_by_journal_bw.pdf"), p, width=5, height=3.25)


###########################################################################
###  Plot Combined Q5 and Q6
###########################################################################
### Use Q6 for Some available, Q5 for the rest
reproduc_df <- reproduc_df %>%
	mutate(Q5_6 = case_when(
		Q5 == "Dataless or review" ~ "Dataless or review",
		Q5 == "No availability" ~ "No availability",	
		Q5 == "Some or all available" & Q6_grouping == "Some or All\nAvailable Online" ~ "Some or All\nAvailable Online",	
		Q5 == "Some or all available" & Q6_grouping == "Only In\nArticle" ~ "Only In\nArticle",	
		Q5 == "Some or all available" & Q6_grouping == "Author\nRequest" ~ "Author\nRequest",	
		Q5 == "Some or all available" & Q6_grouping == "Third\nParty" ~ "Third\nParty",							
		TRUE ~ NA_character_)
	) 

### Check that numbers add correctly and no NAs
table(reproduc_df$Q5_6)

### Refactor labels
reproduc_df$Q5_6 <- factor(reproduc_df$Q5_6, levels= c("No availability", "Dataless or review", "Third\nParty", "Author\nRequest", "Only In\nArticle", "Some or All\nAvailable Online"))

### Create count table
plot_Q5_6 <- reproduc_df %>% 
	filter(rep_avail_clean == "avail") %>%
	dplyr::select(Q2_abbrev, Q5_6) %>%
	count(Q2_abbrev, Q5_6) %>%  
	complete(Q2_abbrev, Q5_6, fill = list(n = 0)) %>% 
	mutate(prop_all = prop.table(n)) %>%
	group_by(Q2_abbrev) %>% 
	mutate(prop_by_journal = n/sum(n)) %>% 
	as.data.frame()

	
plot_Q5_6$Q2_abbrev <- factor(plot_Q5_6$Q2_abbrev, levels=c("EM&S", "HESS", "WRR", "JoH", "JAWRA", "JWRP&M"))
plot_Q5_6$Q5_6 <- factor(plot_Q5_6$Q5_6, levels=rev(levels(plot_Q5_6$Q5_6)))


### Plot as separate bars  
  p <- ggplot(data = plot_Q5_6, aes(x = Q2_abbrev, y = prop_by_journal, fill = Q5_6)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
  p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 0.7))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Stated Availability", values=cb_pal("nature", n=6, sort=FALSE)[c(3, 2, 4, 5, 1, 6)], breaks= c("Some or All\nAvailable Online", "Only In\nArticle", "Author\nRequest", "Third\nParty", "No availability","Dataless or review"))
  #p <- p + scale_fill_manual(name="Availability claim", values=cb_pal("wong", n=6, sort=TRUE))
   #  p <- p + scale_fill_manual(name="Availability claim", values=c("purple", "red", "darkblue", "blue", "yellow", "green"))
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p <- p + guides(fill = guide_legend(nrow = 2, byrow=TRUE, title.position="top", title.hjust=0))
  p 

### Save figure
ggsave(file.path(write_output_path, "q5_q6_combined_byavail_bar.png"), p,  width=5.5, height=4, dpi=600)
ggsave(file.path(write_output_path, "q5_q6_combined_byavail_bar.svg"), p,  width=5.5, height=4)
ggsave(file.path(write_output_path, "q5_q6_combined_byavail_bar.pdf"), p,  width=5.5, height=4)



  p <- ggplot(data = plot_Q5_6, aes(x = Q5_6, y = prop_by_journal, fill = Q2_abbrev)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
  p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 0.7))
  p <- p + scale_x_discrete(name="Stated Availability")
  p <- p + scale_fill_manual(name="Journal", values=journal_colors) 
 # p <- p + scale_fill_manual(name="Availability claim", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p <- p + guides(fill = guide_legend(nrow = 1, byrow=TRUE, title.position="top", title.hjust=0.5))
  p 

# Save figure
ggsave(file.path(write_output_path, "q5_q6_combined_byjournal_bar.png"), p,  width=5.5, height=4, dpi=600)
ggsave(file.path(write_output_path, "q5_q6_combined_byjournal_bar.svg"), p,  width=5.5, height=4)
ggsave(file.path(write_output_path, "q5_q6_combined_byjournal_bar.pdf"), p,  width=5.5, height=4)


### Refactor labels
plot_Q5_6$Q5_6 <- factor(plot_Q5_6$Q5_6, levels= c("Dataless or review","No availability", "Third\nParty", "Author\nRequest", "Only In\nArticle", "Some or All\nAvailable Online"))

### Plot as stacked bars    	
  p <- ggplot(data = plot_Q5_6, aes(x = Q2_abbrev, y = prop_by_journal, fill = Q5_6)) 
  p <- p + geom_bar(stat = 'identity', position = 'stack', width=0.7)
  p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  #p <- p + scale_fill_manual(name="Availability claim", values=rev(cb_pal("ptol", n=6, sort=FALSE)[c(3, 6, 1, 2, 5, 4)]))#, limits=levels(avail_journal$Q5_leading)) 
  p <- p + scale_fill_manual(name="Stated Availability", values=rev(cb_pal("nature", n=6, sort=FALSE)[c(3, 2, 4, 5, 6, 1)]))
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p <- p + guides(fill = guide_legend(nrow = 2, byrow=TRUE, title.position="top", title.hjust=0, reverse=TRUE))
  p 
  
### Save figure
ggsave(file.path(write_output_path, "q5_q6_combined_byavail_stacked.png"), p,  width=5.5, height=4, dpi=600)
ggsave(file.path(write_output_path, "q5_q6_combined_byavail_stacked.svg"), p,  width=5.5, height=4)
ggsave(file.path(write_output_path, "q5_q6_combined_byavail_stacked.pdf"), p,  width=5.5, height=4)


###########################################################################
###  Save Figure 3 from Publication
###########################################################################
ggsave(file.path(write_pub_path, "Fig_3.png"), p,  width=5.5, height=4, dpi=600)
ggsave(file.path(write_pub_path, "Fig_3.svg"), p,  width=5.5, height=4)
ggsave(file.path(write_pub_path, "Fig_3.pdf"), p,  width=5.5, height=4)



###########################################################################
###  Plot Artifacts (Q7)
###########################################################################
### Set up output folders
write_output_path <- file.path(write_figures_path, "artifacts")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)

### Create dataframe for Q7
plot_q7 <- q7_journal_perc%>%
  	 gather(key=source, value=value, -Q2_abbrev) %>%
  	 filter(Q2_abbrev !="Total")

plot_q7 <- plot_q7 %>%
	complete(Q2_abbrev, source, fill = list(value = 0))  

#Directions to run
#Code/Model/Software
#Input data

#Hardware/Software requirements
#Materials linked by unique and persistent identifiers
#Metadata to describe the code
#Common file format /instructions to open

### Set up labels for primary and secondary artifacts
plot_q7$facet <- "Secondary"
plot_q7$facet[plot_q7$source %in% c("dir", "code", "data")] <- "Primary"

q7_labels <- c("Input\ndata", "Code/Model/\nSoftware", "Directions\nto run",  "Hardware/\nSoftware\nRequirements", "Common\nFile\nFormats", "Unique &\nPersistent\nIdentifiers", "Metadata")

plot_q7$source <- factor(plot_q7$source, levels=c("data", "code", "dir", "hardw","common", "doi", "meta"), labels=q7_labels)  	
plot_q7$Q2_abbrev <- factor(plot_q7$Q2_abbrev, levels=journal_abbrev)  	

### Create plot
p <- ggplot(subset(plot_q7, source != "None"), aes(x=source, y=value, fill=Q2_abbrev)) 
p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, .5))
p <- p + scale_x_discrete(name="Available Artifacts (Q7)")
p <- p + scale_fill_manual(name="Journal", breaks=journal_abbrev, values=journal_colors[c(1, 2, 6, 5, 4, 3)]) 
p <- p + facet_grid(. ~ facet, drop=TRUE, scales="free_x")
p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
p 	

#cvd_grid(p)

### Save figure
ggsave(file.path(write_output_path, "q7_by_journal.png"), p,  width=5.5, height=3.85, dpi=600)
ggsave(file.path(write_output_path, "q7_by_journal.svg"), p,  width=5.5, height=3.85)
ggsave(file.path(write_output_path, "q7_by_journal.pdf"), p,  width=5.5, height=3.85)

###########################################################################
###  Save Figure 4 from Publication
###########################################################################
ggsave(file.path(write_pub_path, "Fig_4.png"), p,  width=5.5, height=3.85, dpi=600)
ggsave(file.path(write_pub_path, "Fig_4.svg"), p,  width=5.5, height=3.85)
ggsave(file.path(write_pub_path, "Fig_4.pdf"), p,  width=5.5, height=3.85)



p <- ggplot(subset(plot_q7, source != "None"), aes(x=Q2_abbrev, y=value, fill=source))
p <- p + geom_bar(stat = 'identity', width=0.75, position = position_dodge(width=0.75))
p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 0.5))
p <- p + scale_x_discrete(name="Journal")
p <- p + scale_fill_manual(name="Available Artifacts", values=c("#e31a1c", "#a65628", "#ff7f00",  "#1f78b4","#a6cee3","#33a02c", "#b2df8a"), guide=guide_legend(nrow=2,byrow=TRUE))#cb_pal("nature", n=7, sort=TRUE)) 
p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
p 	

#cvd_grid(p)

#c("#d53e4f","#f46d43","#fdae61", "#3288bd", "#66c2a5", "#abdda4", "#e6f598")
#c("#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3","#1f78b4","#b2df8a","#33a02c")

### Save figure
ggsave(file.path(write_output_path, "q7_by_artifacts.png"), p,  width=5.5, height=3.85, dpi=600)
ggsave(file.path(write_output_path, "q7_by_artifacts.svg"), p,  width=5.5, height=3.85)
ggsave(file.path(write_output_path, "q7_by_artifacts.pdf"), p,  width=5.5, height=3.85)


p <- ggplot(subset(plot_q7, source != "None"), aes(x=source, y=value)) 
p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8), fill="grey20")
p <- p + scale_y_continuous(name="All Surveyed Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, .5))
p <- p + scale_x_discrete(name="Available Artifacts")
#p <- p + scale_fill_manual(name="Journal", values=journal_colors) 
p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
p <- p + facet_grid(. ~ facet, drop=TRUE, scales="free_x")
p 	

### Save figure
ggsave(file.path(write_output_path, "q7_allsurveyed_bw.png"), p,  width=5.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q7_allsurveyed_bw.svg"), p,  width=5.5, height=3)
ggsave(file.path(write_output_path, "q7_allsurveyed_bw.pdf"), p,  width=5.5, height=3)


p <- ggplot(subset(plot_q7, source != "None"), aes(x=source, y=value, fill=source)) 
p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
p <- p + scale_y_continuous(name="All Surveyed Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, .5))
p <- p + scale_x_discrete(name="Available Artifacts")
p <- p + scale_fill_manual(name="Available Artifacts", values=c("#e31a1c", "#a65628", "#ff7f00",  "#1f78b4","#a6cee3","#33a02c", "#b2df8a"), guide=FALSE)#cb_pal("nature", n=7, sort=TRUE))
p <- p + facet_grid(. ~ facet, drop=TRUE, scales="free_x")
p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
p 	

### Save figure
ggsave(file.path(write_output_path, "q7_allsurveyed_color.png"), p,  width=5.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q7_allsurveyed_color.svg"), p,  width=5.5, height=3)
ggsave(file.path(write_output_path, "q7_allsurveyed_color.pdf"), p,  width=5.5, height=3)

###########################################################################
###  Plot Reproducibility (Q11)
###########################################################################
### Set up output folders
write_output_path <- file.path(write_figures_path, "reproducibility")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)

plot_q11 <- reproduc_df%>%
  	 filter(rep_avail_clean =="repro") %>%
	select(Q11) %>%
	count(Q11) %>%  
	mutate(prop_all = prop.table(n)) %>%
	mutate(prop_by_journal = n/sum(n)) %>% 
	as.data.frame()

### Reorder the levels for question Q5
plot_q11$Q11 <- factor(plot_q11$Q11, levels=rev(q11_labels))
#plot_q11$Q5_leading <- factor(plot_q11$Q5, levels=levels(plot_q11$Q5)[c(3,2,1)])

repro_colors <- cb_pal("ptol", n=4, sort=FALSE)
repro_colors <- repro_colors[c(1,4,3,2)]
#repro_colors
 
### Plot as separate bars  
  p <- ggplot(data = plot_q11, aes(x = Q11, y = n, fill = Q11)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
  p <- p + scale_y_continuous(name="Number of Articles", expand = c(0, 0), limits = c(0, 11), breaks=seq(0,20,2))
  p <- p + scale_x_discrete(name="Reproducibility Determination (Q11)")
  p <- p + scale_fill_manual(name="Reproducibility Determination (Q11)", values=repro_colors)#, limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="none")
  p 
  
### Save figure
ggsave(file.path(write_output_path, "q11_color.png"), p,  width=5.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q11_color.svg"), p,  width=5.5, height=3)
ggsave(file.path(write_output_path, "q11_color.pdf"), p,  width=5.5, height=3)


###########################################################################
###  Plot Reproducibility Failure Cause (Q13)
###########################################################################
#sSor for plotting
plot_q13 <- q13_journal_perc%>%
  	 gather(key=source, value=value, -Q2_abbrev) %>%
  	 filter(Q2_abbrev !="Total")

plot_q13 <- q13_journal_count%>%
  	 gather(key=failure, value=value, -Q2_abbrev) %>%
  	 filter(Q2_abbrev =="Total") %>%
  	 ### remove total n
  	 filter(failure != "n")

### Remove None - not informative, it is either 4 with Q11 = "some" or availability fail
plot_q13 <- plot_q13 %>%
	filter(failure != "none")

plot_q13$failure <- factor(plot_q13$failure, levels=c("unclear", "differ", "no_result", "hard_soft", "other", "none", "avail_fail"), labels=c(q13_labels, "Availability\nFail"))  #plot_q13$failure <- factor(plot_q13$failure, levels=c("unclear", "differ", "no_result", "hard_soft", "other", "none", "avail_fail"), labels=c(q13_labels, "Availability\nFail"))  	
  	 	
p <- ggplot(plot_q13, aes(x=failure, y=value)) 
p <- p + geom_bar(stat = 'identity')#, width=0.8, position = position_dodge(width=0.8))
p <- p + scale_y_continuous(name="Number of Articles", breaks=seq(0,20,2), expand = c(0, 0))
p <- p + coord_cartesian(ylim = c(0, 11))
p <- p + scale_x_discrete(name="Why Did Reproducibility Fail (Q13)", limits=c("Availability\nFail", "Unclear directions", "Did not generate results", "Hardware/software error", "Results differed", "Other"), labels=c("Availability\nFail", "Unclear\ndirections", "Did not\ngenerate results", "Hardware/software\nerror", "Results\ndiffered", "Other"))
#p <- p + scale_fill_manual(name="Journal", values=journal_colors) 
p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))
p 	
  
### Save figure
ggsave(file.path(write_output_path, "q13_bw.png"), p,  width=5.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q13_bw.svg"), p,  width=5.5, height=3)
ggsave(file.path(write_output_path, "q13_bw.pdf"), p,  width=5.5, height=3)


### Output reproducibility paper details
reproduc_df %>%
	filter(rep_avail == "repro") %>%
	filter(Q11 != "Availability\nFail") %>%
	select(Q4, Q11, keyword) %>%
	arrange(Q11)

### Chi squared test of number of partially reproducible papers from keyword vs non-keyword
prop.test(x = c(5,1), n=c(119, 241), p = NULL, alternative = "greater", correct = TRUE)

	
	
#################
###  Plot time spent on paper   (Q12)
################ 
### Set up output folders
write_output_path <- file.path(write_figures_path, "time_spent")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)


reproduc_df$stopping_point <- "Availability"
reproduc_df$stopping_point[reproduc_df$Q5 != "Some or all available"] <- "Paper Type"
#reproduc_df$stopping_point[reproduc_df$Q10 == "Yes"] <- "Reproducibility"
### Pretty sure the long one is also a reproducibility
reproduc_df$stopping_point[reproduc_df$rep_avail_clean == "repro" & reproduc_df$Q11 != "Availability\nFail"] <- "Reproducibility"
reproduc_df$stopping_point <- factor(reproduc_df$stopping_point, levels = c( "Reproducibility", "Availability", "Paper Type"))

### Produce summary table
reproduc_df %>%
	group_by(stopping_point) %>% 
	summarize(mean_time=mean(Q12, na.rm=TRUE), median_time=median(Q12, na.rm=TRUE), perc_25=quantile(Q12, 0.25, na.rm=TRUE), perc_75=quantile(Q12, 0.75, na.rm=TRUE), min_time=min(Q12, na.rm=TRUE), max_time=max(Q12, na.rm=TRUE), count_with_time=sum(!is.na(Q12)), count=n()) %>%
	as.data.frame()
	
#Q9 is yes, continu
p <- ggplot(reproduc_df, aes(x=stopping_point, y=Q12))
p <- p + geom_boxplot(fill="grey")
p <- p + stat_summary(fun.y=mean, colour="#e41a1c", geom="point", shape=18, size=3,show_guide = FALSE)
p <- p + coord_flip(ylim=c(0,121))
p <- p + theme_classic_new(9.5)
p <- p + scale_x_discrete(name="Survey Ending Question", breaks=c( "Reproducibility", "Availability", "Paper Type"), labels=c( "Reproducibility of\nResults\n(Q13)", "Availability of\nArtifacts\n(Q9)", "No Artifacts\nAvailable\n(Q5)"))
p <- p + scale_y_continuous(name="Time Spent (Minutes)", breaks=seq(0,500,15))
p

### Save figure
ggsave(file.path(write_output_path, "q12_box_whiskers.png"), p,  width=4.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q12_box_whiskers.svg"), p,  width=4.5, height=3)
ggsave(file.path(write_output_path, "q12_box_whiskers.pdf"), p,  width=4.5, height=3)



p <- ggplot(reproduc_df, aes(x=stopping_point, y=Q12))
p <- p + geom_violin(fill="grey", adjust = 1.5)
p <- p + geom_jitter(height = 0, width = 0.07, colour="black", alpha=0.3, size=0.3)
p <- p + stat_summary(fun.y=mean, colour="#e41a1c", geom="point", shape=18, size=3,show_guide = FALSE)
p <- p + coord_flip()
p <- p + theme_classic_new(9.5)
p <- p + scale_x_discrete(name="Survey Ending Question", breaks=c( "Reproducibility", "Availability", "Paper Type"), labels=c( "Reproducibility of\nResults\n(Q13)", "Availability of\nArtifacts\n(Q9)", "No Artifacts\nAvailable\n(Q5)"))
p <- p + scale_y_continuous(name="Time Spent (Minutes)", breaks=seq(0,500,15))
p
### Save figure
ggsave(file.path(write_output_path, "q12_violin.png"), p,  width=4.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q12_violin.svg"), p,  width=4.5, height=3)
ggsave(file.path(write_output_path, "q12_violin.pdf"), p,  width=4.5, height=3)

###########################################################################
###  Save Figure 3 from Publication
###########################################################################
ggsave(file.path(write_pub_path, "Fig_6.png"), p,   width=4.5, height=3, dpi=600)
ggsave(file.path(write_pub_path, "Fig_6.svg"), p,   width=4.5, height=3)
ggsave(file.path(write_pub_path, "Fig_6.pdf"), p,   width=4.5, height=3)



p <- ggplot(reproduc_df, aes(x=Q12))
p <- p + geom_histogram(binwidth=5)
p <- p + theme_classic_new(9.5)
p <- p + scale_x_continuous(name="Time Spent (Minutes)", breaks=seq(0,500,15), expand = c(0, 0))
p <- p + scale_y_continuous(name="Count", expand = c(0, 0))
p 
### Save figure
ggsave(file.path(write_output_path, "q12_histogram.png"), p,  width=4.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q12_histogram.svg"), p,  width=4.5, height=3)
ggsave(file.path(write_output_path, "q12_histogram.pdf"), p,  width=4.5, height=3)


p <- ggplot(reproduc_df, aes(x=Q12, y=stopping_point))
p <- p + geom_density_ridges( size=0.2, bandwidth=10)
p <- p + theme_classic_new(9.5)
p <- p + scale_x_continuous(name="Time Spent (Minutes)", breaks=seq(0,500,15), expand = c(0, 0))
p <- p + scale_y_discrete(name="Survey Ending Question", breaks=c( "Reproducibility", "Availability", "Paper Type"), labels=c( "Reproducibility of\nResults\n(Q13)", "Availability of\nArtifacts\n(Q9)", "No Artifacts\nAvailable\n(Q5)"))
p 
### Save figure
ggsave(file.path(write_output_path, "q12_ridge.png"), p,  width=4.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q12_ridge.svg"), p,  width=4.5, height=3)
ggsave(file.path(write_output_path, "q12_ridge.pdf"), p,  width=4.5, height=3)


# Boxplots by journal
p <- ggplot(reproduc_df, aes(x=stopping_point, y=Q12, fill=Q2_abbrev))
p <- p + geom_boxplot()
#p <- p + stat_summary(fun.y=mean, colour="#e41a1c", geom="point", shape=18, size=3,show_guide = FALSE)
p <- p + scale_fill_manual(name="Journal", values=journal_colors) 
p <- p + coord_flip()
p <- p + theme_classic_new(9.5)
p <- p + scale_x_discrete(name="Survey Ending Question", breaks=c( "Reproducibility", "Availability", "Paper Type"), labels=c( "Reproducibility of\nResults\n(Q13)", "Availability of\nArtifacts\n(Q9)", "No Artifacts\nAvailable\n(Q5)"))
p <- p + scale_y_continuous(name="Time Spent (Minutes)", breaks=seq(0,500,15))
p
#
### Save figure
ggsave(file.path(write_output_path, "q12_box_whiskers_byjournal.png"), p,  width=4.5, height=3, dpi=600)
ggsave(file.path(write_output_path, "q12_box_whiskers_byjournal.svg"), p,  width=4.5, height=3)
ggsave(file.path(write_output_path, "q12_box_whiskers_byjournal.pdf"), p,  width=4.5, height=3)



#################
###  Generate River Plot
################ 

#######################
###  First edge/node
#######################
reproduc_df <- reproduc_df %>%
	mutate(Q5_num = case_when(
		Q5 == "Some or all available" ~ "Q5_3",
		Q5 == "No availability" ~ "Q5_2",	
		Q5 == "Dataless or review" ~ "Q5_1",						
		TRUE ~ NA_character_)
	) 

### Calculate Edges  
q5_freq <- reproduc_df %>% 
		filter(rep_avail_clean == "avail") %>% 
		group_by(Q5_num) %>%
		summarize(Count = n()) %>%
		spread(Q5_num, Count, fill = 0) %>%
		gather() %>%
		as.data.frame()

q5_freq$N2 <- q5_freq$key
q5_freq$N1 <- "Q0_1"
q5_freq$Value <- q5_freq$value
	
paper_edges_level2 <- q5_freq %>% dplyr::select(N1, N2, Value)

#q5_labels <- levels(reproduc_df$Q5)
q5_labels <- c("Dataless\nor review [34]", "Not specified\nwhere [73]", "Some\navailability [253]")
#q5_levels <- paste0("Q5_", seq(1,length(q5_labels)))

### Calculate nodes 
paper_nodes_level1 <- data.frame(ID="Q0_1", x=1, y=400, labels="All\npapers\n[360]")
paper_nodes_level2 <- data.frame(ID=c("Q5_1", "Q5_2", "Q5_3"), x=c(2, 2, 2), y=c(0,150, 453.5), labels=q5_labels)

paper_nodes_level1$col <- c("#4477AA")
paper_nodes_level2$col <- c("#EFE8B6", "#EFE8B6", "#4477AA")

#######################
###  Q5 to Q6
#######################  
reproduc_df <- reproduc_df %>%
	mutate(Q6_num = case_when(
		Q6_grouping == "Some or All\nAvailable Online" | Q6_grouping == "Only In\nArticle"  ~ "Q6_3",
		Q6_grouping == "Author\nRequest" ~ "Q6_2",	
		Q6_grouping == "Third\nParty"  ~ "Q6_1",						
		TRUE ~ NA_character_)
	) 
		
reproduc_df$Q6_num <- factor(reproduc_df$Q6_num, levels=paste0("Q6_", seq(1,3)))		

### Calculate Edges  
q6_freq <- reproduc_df %>% 
		filter(rep_avail_clean == "avail") %>% 
		filter(Q5_num == "Q5_3") %>% 
		group_by(Q5_num, Q6_num) %>%
		summarize(Count = n()) %>%
		complete(Q6_num, fill = list(Count = 0)) %>%
		as.data.frame()


paper_edges_level3 <- q6_freq %>%
	mutate(N1=Q5_num, N2=Q6_num) %>%
	mutate(Value = Count) %>%
	dplyr::select(N1, N2, Value)


q6_labels <- c("Contact\nthird party [10]", "Contact\nfirst author [68]", "Some\nonline [175]")
#q6_labels <- c("q6_1", "q6_2", "q6_3")

### Calculate nodes 
paper_nodes_level3 <- data.frame(ID=c("Q6_1", "Q6_2", "Q6_3"), x=c(3, 3, 3), y=c(100, 250, 492.5), labels=q6_labels)


paper_nodes_level3$col <- "#EFE8B6"
paper_nodes_level3$col[paper_nodes_level3$ID == "Q6_3"] <- "#4477AA"

#######################
###  Q6 to Q7
#######################
   
reproduc_df <- reproduc_df %>%
	mutate(Q7_num = case_when(
		Q7_primary_n == 0  ~ "Q7_1",
		Q7_primary_n == 1  ~ "Q7_2",
		Q7_primary_n == 2  ~ "Q7_3",					
		Q7_primary_n == 3  ~ "Q7_4",					
		TRUE ~ NA_character_)
	) 

### Calculate Edges  
q7_freq <- reproduc_df %>% 
		filter(rep_avail_clean == "avail") %>% 
		filter(Q6_num == "Q6_3") %>% 
		group_by(Q6_num, Q7_num) %>%
		summarize(Count = n()) %>%
		complete(Q7_num, fill = list(Count = 0)) %>%
		as.data.frame()

q7_freq

paper_edges_level4 <- q7_freq %>%
	mutate(N1=Q6_num, N2=Q7_num) %>%
	mutate(Value = Count) %>%
	dplyr::select(N1, N2, Value)

### Calculate nodes 
#q7_labels <- paste0("Primary ", seq(0,3))
q7_labels <- c("No primary\nartifacts [51]", "1 of 3 primary\nartifacts [80]", "2 of 3 primary\nartifacts [24]", "Directions, code\n& data [20]")
#q7_labels <- c("q7_1", "q7_2", "q7_3", "q7_4")

### Calculate nodes 
paper_nodes_level4 <- data.frame(ID=c("Q7_1", "Q7_2", "Q7_3", "Q7_4"), x=c(4, 4, 4, 4), y=c(150,275,400,570.5), labels=q7_labels)

paper_nodes_level4$col <- "#4477AA"
paper_nodes_level4$col[paper_nodes_level4$ID == "Q7_3"] <- "#EFE8B6"
paper_nodes_level4$col[paper_nodes_level4$ID == "Q7_2"] <- "#EFE8B6"
paper_nodes_level4$col[paper_nodes_level4$ID == "Q7_1"] <- "#EFE8B6"


#######################
###  Q7 to Q11
#######################

reproduc_df <- reproduc_df %>%
	mutate(Q11_num = case_when(
		Q11 == "Availability\nFail"  ~ "Q11_1",
		Q11 == "No"  ~ "Q11_2",
		Q11 == "Some"  ~ "Q11_3",					
		Q11 == "Yes"  ~ "Q11_4",					
		TRUE ~ NA_character_)
	) 

### Calculate Edges  
q11_freq <- reproduc_df %>% 
		filter(rep_avail_clean == "repro") %>% 
		group_by( Q11_num) %>%
		summarize(Count = n()) %>%
		complete(Q11_num, fill = list(Count = 0)) %>%
		as.data.frame()

q11_freq

paper_edges_level5 <- q11_freq %>%
	mutate(N1="Q7_4", N2=Q11_num) %>%
	mutate(Value = Count) %>%
	dplyr::select(N1, N2, Value)

### Calculate nodes 
q11_labels <- c("Availability\nfailure [10]", "Not reproducible\n[4]", "Some reproducible\n[4]", "Fully reproducible\n[2]")

### Calculate nodes 
paper_nodes_level5 <- data.frame(ID=c("Q11_1", "Q11_2", "Q11_3", "Q11_4"), x=c(5, 5, 5, 5), y=c(200,300,475,579), labels=q11_labels)

paper_nodes_level5$col <- "#4477AA"
#paper_nodes_level5$col[paper_nodes_level5$ID == "Q11_3"] <- "#CCBB44"
paper_nodes_level5$col[paper_nodes_level5$ID == "Q11_2"] <- "#EFE8B6"
paper_nodes_level5$col[paper_nodes_level5$ID == "Q11_1"] <- "#EFE8B6"

################################
###  Combine all data
#################################
paper_edges <- rbind(paper_edges_level2, paper_edges_level3, paper_edges_level4, paper_edges_level5)
paper_nodes <- rbind(paper_nodes_level1, paper_nodes_level2, paper_nodes_level3, paper_nodes_level4, paper_nodes_level5)
rownames(paper_nodes) <- paper_nodes$ID

#paper_edges$edgecol <- "col"

# create a custom style by first copying from the default style
styles <- riverplot::default.style()


rp <- list(nodes = paper_nodes, edges = paper_edges, styles=styles)#, styles = styles)
class(rp) <- c(class(rp), "riverplot")
plot(rp)


river <- makeRiver( paper_nodes, paper_edges )
style <- riverplot::default.style()

style[["srt"]] <- "90"
style[["textpos"]] <- "4"
style[["textcex"]] <- 1.4

style[["srt"]] <- "0"
plot( river, default_style= style , yscale=1, fix.pdf=TRUE)


### Set up output folders and file
write_output_path <- file.path(write_figures_path, "river_plot")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)
river_file <- file.path(write_output_path, "papers_riverplot")

png(paste0(river_file, '.png'), width = 13, height = 9, units = 'in', res = 300)
plot( river, default_style= style , yscale=1, fix.pdf=TRUE)
dev.off()

svg(paste0(river_file, '.svg'), width = 13, height = 9)#, units = 'in')#, res = 300)
plot( river, default_style= style , yscale=1, fix.pdf=TRUE)
dev.off()

pdf(paste0(river_file, '.pdf'), width = 13, height = 9)#, units = 'in')#, res = 300)
plot( river, default_style= style , yscale=1, fix.pdf=TRUE)
dev.off()

###########################################################################
###  Save Figure 2 from Publication
###########################################################################
river_file <- file.path(write_pub_path, "Fig_2")

png(paste0(river_file, '.png'), width = 13, height = 9, units = 'in', res = 300)
plot( river, default_style= style , yscale=1, fix.pdf=TRUE)
dev.off()

svg(paste0(river_file, '.svg'), width = 13, height = 9)#, units = 'in')#, res = 300)
plot( river, default_style= style , yscale=1, fix.pdf=TRUE)
dev.off()

pdf(paste0(river_file, '.pdf'), width = 13, height = 9)#, units = 'in')#, res = 300)
plot( river, default_style= style , yscale=1, fix.pdf=TRUE)
dev.off()



################################
###  Save riverplot data and revised reproduc_df
#################################
### Output to csv
write.csv(paper_edges, file.path(write_output_path, "paper_edges.csv"))
write.csv(paper_nodes, file.path(write_output_path, "paper_nodes.csv"))

### Save revised reproduc_df
saveRDS(reproduc_df, file = file.path(write_output_base_path, "reproduc_df.rds"))





###########################################################################
###  Print a Completion message
###########################################################################
print('===========================================================')
print('Done. Next execute the 04_pop_estimate.R file') 
print('===========================================================')
