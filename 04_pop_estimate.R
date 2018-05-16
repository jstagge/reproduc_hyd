
### Clear any existing data or functions.
rm(list=ls())

###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "../../data"
output_path <- "../../output"
global_path <- "../global_func"
function_path <- "./functions"

### Set output location
output_name <- "reproduc"
write_output_base_path <- file.path(output_path, output_name)

dir.create(write_output_base_path)

### Set input location
data_path<- file.path(data_path, "reproduc")

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)
require(staggefuncs)
require(tidyverse)
require(colorblindr)

### Load these functions for this unique project
require(ggthemes)
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

journal_abbrev <- c("EM&S", "HESS", "JoH", "JAWRA", "JWRP&M", "WRR")
journal_colors <- cb_pal("custom", n=6, sort=FALSE)

###########################################################################
## Set Additional Output Folders
###########################################################################
### Set up output folders
write_figures_path <- file.path(write_output_base_path, "figures")
dir.create(file.path(write_figures_path,"png"), recursive=TRUE)
dir.create(file.path(write_figures_path,"pdf"), recursive=TRUE)
dir.create(file.path(write_figures_path,"svg"), recursive=TRUE)



###########################################################################
## Load data
###########################################################################
load(file=file.path(write_output_base_path, "reproduc_data.rda"))


###########################################################################
## Create categories for bootstrap resampling
###########################################################################

categories <- data.frame(journal_abbrev=pub_summary_table$journal_abbrev, keyword=TRUE, n=pub_summary_table$keyword)
categories <- rbind(categories, data.frame(journal_abbrev=pub_summary_table$journal_abbrev, keyword=FALSE, n=pub_summary_table$none))
categories

###########################################################################
## Add column for sampled papers
###########################################################################
### Calculate sampled numers of papers per category
sampled_categories <- reproduc_df %>%
	select(Q2_abbrev, keyword) %>% 
	group_by(Q2_abbrev, keyword) %>% 
	summarise(n_sampled=n())

### Join this with the original category dataframe
categories <- categories %>%
	left_join(sampled_categories, by = c("journal_abbrev" = "Q2_abbrev", "keyword"="keyword"))

### Create columns for resampling step
categories <- categories %>% 
	mutate(n_sampled_need = case_when(n_sampled > n ~ n, TRUE ~ n_sampled)) %>%
	mutate(n_unsampled_need = n - n_sampled_need)
	
### Check that numbers sum correctly
are_equal(categories$n_sampled_need + categories$n_unsampled_need, categories$n)

### Show results
categories

###########################################################################
## Loop and resample
###########################################################################
n_runs <- 5000

start_time <- Sys.time()


for (j in seq(1,n_runs)) {

cat(paste0("j: ", j, "   of: ", n_runs, "\n"))

for (i in seq(1, dim(categories)[1])){
	### Extract loop characteristics
	journal_i <- categories$journal_abbrev[i]
	keyword_i <- categories$keyword[i]
	n_i <- categories$n[i]
	n_sampled_i <- categories$n_sampled_need[i]
	n_unsampled_i <- categories$n_unsampled_need[i]
	
	### report progress
	#cat(paste0("Journal: ", journal_i, "   Keyword: ", keyword_i, "\n"))
	
	### Subset papers
	papers_i <- reproduc_df %>% 
		filter(Q2_abbrev == journal_i & keyword == keyword_i)
  
	### Extract/resample for sampled papers
	### Use sampling without replacement in case 
	papers_sampled_df <- papers_i %>%
		sample_n(size = n_sampled_i, replace = FALSE)

	### Extract/resample unsampled papers
 	papers_unsampled_df <- papers_i %>%
		sample_n(size = n_unsampled_i, replace = TRUE)
 
	### Combine for the category
	papers_temp <- rbind(papers_sampled_df, papers_unsampled_df)
	rm(papers_sampled_df, papers_unsampled_df)

	if (i == 1) {
		papers_all <- papers_temp
	} else {
		papers_all <- rbind(papers_all, papers_temp)
	}
}

### Check that it equals total n
#assertthat

### Run summary stats and return
q6_temp <- papers_all %>% 
	select(Q2_abbrev, Q6_grouping) %>%
	group_by(Q2_abbrev, Q6_grouping) %>%
	summarise(n=n()) 
	
q6_temp <- data.frame(sample=j, q6_temp)

if (j == 1){
	q6_resample <- q6_temp
} else {
	q6_resample <- rbind(q6_resample, q6_temp)
}

}


end_time <- Sys.time()

end_time - start_time







q6_totals <- q6_resample %>%
	group_by(sample, Q6_grouping) %>%
	summarise(n = sum(n)) %>% 
	mutate(Q2_abbrev = "Total") %>%
	select(sample, Q2_abbrev, Q6_grouping, n) %>%
	as.data.frame()

q6_resample <- rbind(q6_resample, q6_totals)

p <- ggplot(q6_resample, aes(x=Q2_abbrev, y=n, fill=Q6_grouping))
p <- p + geom_boxplot(position="dodge")
p <- p + theme_classic()
p

p <- ggplot(q6_resample, aes(x=Q2_abbrev, y=n, fill=Q6_grouping))
p <- p + geom_bar(position="stack", stat = "summary", fun.y = "mean")
p <- p + theme_classic()
p

 geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.25)
 
    
yup <- q6_resample %>%
	left_join(select(pub_summary_table, journal_abbrev, total), by=c("Q2_abbrev" = "journal_abbrev"))  %>%
	mutate(propor = n/total)
	
p <- ggplot(subset(yup, Q6_grouping=="Some or All\nAvailable Online"), aes(x=propor, fill=Q2_abbrev))
p <- p + geom_density()
p <- p + scale_fill_manual(values=journal_colors)
p <- p + theme_classic_new() 
p

p <- ggplot(yup, aes(x=propor, fill=Q2_abbrev))
p <- p + geom_density(alpha=0.8)
p <- p + facet_wrap(~Q6_grouping, ncol=1)
p <- p + coord_cartesian(ylim=c(0,40))
p <- p + scale_fill_manual(values=journal_colors)
p <- p + theme_classic_new()
p

p <- ggplot(yup, aes(x=Q2_abbrev, y = propor, fill=Q2_abbrev))
p <- p + geom_boxplot(position="dodge")
p <- p + facet_wrap(~Q6_grouping, ncol=1)
#p <- p + coord_cartesian(ylim=c(0,40))
p <- p + scale_x_discrete(limits = rev(unique(yup$Q2_abbrev)))
p <- p + coord_flip()
p <- p + scale_fill_manual(values=journal_colors)
p <- p + theme_classic_new()
p


p <- p + geom_bar(position="stack", stat = "summary", fun.y = "mean")
p <- p + theme_classic()
p	

	
	
q6_resample %>%
	group_by(Q2_abbrev, Q6_grouping) %>%
	summarise(mean_something=mean(n)) 


yup <- q6_resample %>%
	group_by(Q2_abbrev, Q6_grouping) 
	




	
	
	
	%>%
	as.data.frame()
	
	
	%>%
	gather()
	
	spread(var, value)
	
	
	key = flower_att, value = measurement,
            Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
            
            
            
            
	
	
}




expand.grid(journal=pub_summary_table$journal_abbrev, keyword = c(TRUE, FALSE))



reproduc_df
pub_summary_table





