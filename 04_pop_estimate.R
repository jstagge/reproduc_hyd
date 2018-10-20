# *------------------------------------------------------------------
# | PROGRAM NAME: 04_pop_estimate
# | FILE NAME: 04_pop_estimate.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code generates the population estimate for all papers
# | in 2017 through resampling.
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
require(ggthemes)
require(MultinomialCI)

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
write_figures_path <- file.path(write_output_base_path, "all_figures")
write_output_path <- file.path(write_figures_path, "population_estimate")
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)

### Set up output folders
write_pub_path <- file.path(write_output_base_path, "publication_figures")
dir.create(write_pub_path, recursive=TRUE, showWarnings = FALSE)

###########################################################################
## Load data
###########################################################################
load(file=file.path(write_output_base_path, "survey_analysis/reproduc_data.rda"))

reproduc_df <- readRDS(file.path(write_output_base_path, "reproduc_df.rds"))


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
### Don't consider the reproducibility step (i.e. use only availability papers)
sampled_categories <- reproduc_df %>%
	filter(rep_avail_clean == "avail") %>% 
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
## Create a new column for paper determination
###########################################################################

avail_only <- reproduc_df %>% filter(rep_avail_clean == "avail") 
repro_only <- reproduc_df %>% filter(rep_avail_clean == "repro") %>% select(Q3, rep_avail_clean, Q11)

final_determ <- avail_only %>% 
	left_join(repro_only, by="Q3")

final_determ <- final_determ %>% 
	mutate(final_determ = case_when(
		is.na(rep_avail_clean.y) == TRUE & Q5_6 == "Dataless or review" ~ "Dataless or review",
		is.na(rep_avail_clean.y) == TRUE & ( Q5_6 == "Author\nRequest" | Q5_6 == "Third\nParty") ~ "Author or Third Party",
		is.na(rep_avail_clean.y) == TRUE & ( Q5_6 == "No availability" | Q7_primary_n == 0) ~ "No availability",
		is.na(rep_avail_clean.y) == TRUE & Q7_primary_n >= 1 ~ "Some Availability",
		rep_avail_clean.y == "repro" & (Q11.y == "No" | Q11.y == "Availability\nFail") ~ "Available not reproducible",
		rep_avail_clean.y == "repro" & (Q11.y == "Some" | Q11.y == "Yes") ~ "Some or All Replicable",		
		TRUE ~ NA_character_)
	) 
	
final_determ$final_determ <- factor(final_determ$final_determ , levels = c("Dataless or review", "Author or Third Party", "No availability", "Some Availability", "Available not reproducible", "Some or All Replicable"))

table(final_determ$final_determ)
sum(table(final_determ$final_determ))

final_determ_sampled_papers <- final_determ %>% 
	#select(Q2_abbrev, keyword, final_determ) %>% 
	group_by(Q2_abbrev, keyword, final_determ) %>%
	summarize(count = n()) %>%
	complete(Q2_abbrev, keyword, final_determ, fill = list(count = 0)) %>%
	distinct

###########################################################################
## Loop and resample
###########################################################################

n_runs <- 5000

### Set the random seed so results can be reproduced
set.seed(6511)

### Start clock to test how long it takes
start_time <- Sys.time()

for (j in seq(1,n_runs)) {

### Output progress
cat(paste0("  j: ", j, "   of: ", n_runs, "\n"))

for (i in seq(1, dim(categories)[1])){
	### Extract loop characteristics
	journal_i <- categories$journal_abbrev[i]
	keyword_i <- categories$keyword[i]
	n_i <- categories$n[i]
	n_sampled_i <- categories$n_sampled_need[i]
	n_unsampled_i <- categories$n_unsampled_need[i]
	
	### Subset papers
	papers_i <- final_determ_sampled_papers %>% 
		filter(Q2_abbrev == journal_i & keyword == keyword_i)

	### Generate sampled data
	papers_sampled_df <- papers_i
	papers_sampled_df$sampled <- "sampled"
	
	### Generate simulated data
  	### Generate 6 random numbers
  	n_rand <- runif(6,0,1)
  	n_rand_upper <- n_rand > 0.5
  	n_rand[n_rand_upper] <- n_rand[n_rand_upper] - 0.5
  	n_rand <- n_rand * 2
  	
  	pop_est <- rep(NA, 6)
  	### Estimate population
  	for (k in seq (1,6)){  	
		multi_ci <- multinomialCI(papers_i$count, n_rand[k])  
		if(n_rand_upper[k] == FALSE){
			pop_est[k] <- multi_ci[k,1]
		} else {
			pop_est[k] <- multi_ci[k,2]
		}
  	}
  	
  	### Rescale to equal 1
  	pop_est <- pop_est/sum(pop_est)
  
  	### Create table for unsampled papers
  	papers_unsampled_df <- papers_i
  	papers_unsampled_df$count <- pop_est * n_unsampled_i
  	papers_unsampled_df$sampled <- "simulated"
  	
	### Combine for the category
	papers_temp <- rbind(papers_sampled_df, papers_unsampled_df)
	rm(papers_sampled_df, papers_unsampled_df)

	if (i == 1) {
		papers_all <- papers_temp
	} else {
		papers_all <- rbind(papers_all, papers_temp)
	}
}

### Here is where we calculate summary
papers_pop_journal <- papers_all%>%
	group_by(Q2_abbrev, final_determ) %>%
	summarize(count = sum(count))%>%
	add_column(j=j, .before="Q2_abbrev") 

papers_pop_total <- papers_all %>%
	group_by(final_determ) %>%
	summarize(count = sum(count)) %>%
	add_column(j=j, Q2_abbrev="Total", .before="final_determ")

papers_pop_temp <- bind_rows(papers_pop_journal, papers_pop_total)

if (j == 1){
	papers_pop <- papers_pop_temp
} else {
	papers_pop <- rbind(papers_pop, papers_pop_temp)
}

}

### Return how long it took
Sys.time() - start_time



###########################################################################
## Summarize results
###########################################################################
### Add column for total number of papers
### Use this to calculate proportion
paper_totals <- pub_summary_table %>% 
	select(Q2_abbrev=journal_abbrev, total) %>%
	bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
	
paper_all_sims <- papers_pop %>%
	left_join(paper_totals)

paper_all_sims <- paper_all_sims %>% mutate(proportion=count/total)

paper_all_sims$Q2_abbrev <- factor(paper_all_sims$Q2_abbrev, levels=c(levels(reproduc_df$Q2_abbrev), "Total"))

paper_summary_all_sims <- paper_all_sims%>%
	group_by(Q2_abbrev, final_determ) %>%
	summarize(median = quantile(proportion, 0.5), ll = quantile(proportion, 0.025), ul= quantile(proportion, 0.975))

### Testing figures
#ggplot(papers_pop, aes(x=final_determ, fill=Q2_abbrev, y=count)) + geom_boxplot(position="dodge") + theme_classic_new(9.5)

#ggplot(paper_all_sims, aes(x=final_determ, fill=Q2_abbrev, y=proportion)) + geom_boxplot(position="dodge") + scale_fill_manual(values=c(journal_colors,"grey50"))+ theme_classic_new(9.5)

#ggplot(paper_all_sims, aes(x=final_determ, fill=Q2_abbrev, y=proportion)) + geom_violin(position="dodge") + scale_fill_manual(values=c(journal_colors,"grey50")) + theme_classic_new(9.5)

#p <- ggplot(paper_summary_all_sims, aes(x = final_determ, group=Q2_abbrev, colour=Q2_abbrev)) + geom_errorbar(aes(ymin = ll, ymax = ul), position = position_dodge(width = 0.5), size=2) + theme_classic_new(9.5) + scale_colour_manual(values=c(journal_colors,"black"))
#p
#p + coord_flip() + scale_x_discrete(limits = rev(levels(paper_summary_all_sims$final_determ))) 

# p <- ggplot(paper_summary_all_sims, aes(x = final_determ, group=Q2_abbrev, colour=Q2_abbrev)) + geom_pointrange(aes(y=median, ymin = ll, ymax = ul), position = position_dodge(width = 0.5), size=1) + theme_classic() + scale_colour_manual(values=c(journal_colors,"black"))
#p


### Generate the final figures
plot_labels <- c("Dataless or\nReview", "Author or Third Party\nRequest Only", "No Availability", "Some Availability", "Available, but\nNot Replicable", "Some or All\nReplicable")

p <- ggplot(paper_summary_all_sims, aes(x = final_determ, group=Q2_abbrev, colour=Q2_abbrev)) %>%
	+ geom_pointrange(aes(y=median, ymin = ll, ymax = ul), position = position_dodge(width = 0.5), size=.75) %>%
	+ theme_classic_new(10) %>%
	+ scale_colour_manual(name ="Journal", values=c(journal_colors,"black")) %>%
	+ scale_x_discrete(name="Reproducibility Level", labels=plot_labels) %>%
	+ scale_y_continuous(name="Estimated Proportion", labels = scales::percent, expand = c(0, 0), limits = c(0, .7)) %>%
	+ theme(legend.position = c(0.95, 0.75))
	
p

### Save figure
ggsave(file.path(write_output_path, "pop_horizontal.png"), p,  width=7, height=4, dpi=600)
ggsave(file.path(write_output_path, "pop_horizontal.svg"), p,  width=7, height=4)
ggsave(file.path(write_output_path, "pop_horizontal.pdf"), p,  width=7, height=4)


###########################################################################
###  Save Figure 5 from Publication
###########################################################################
ggsave(file.path(write_pub_path, "Fig_5.png"), p,  width=7, height=4, dpi=600)
ggsave(file.path(write_pub_path, "Fig_5.svg"), p,  width=7, height=4, dpi=600)
ggsave(file.path(write_pub_path, "Fig_5.pdf"), p,  width=7, height=4, dpi=600)



### Plot it vertically
p <- ggplot(paper_summary_all_sims, aes(x = final_determ, group=Q2_abbrev, colour=Q2_abbrev)) %>%
	+ geom_pointrange(aes(y=median, ymin = ll, ymax = ul), position = position_dodge(width = -0.4), size=.75) %>%
	+ theme_classic_new(10) %>%
	+ scale_colour_manual(name="Journal", values=c(journal_colors,"black")) %>%
	+ coord_flip() %>%
	+ scale_x_discrete(name="Reproducibility Level", limits = rev(levels(paper_summary_all_sims$final_determ)), labels=rev(plot_labels)) %>%
	+ scale_y_continuous(name="Estimated Proportion)", labels = scales::percent, expand = c(0, 0), limits = c(0, .7)) %>%
	+ theme(legend.position = c(0.87, 0.2))

p

### Save figure
ggsave(file.path(write_output_path, "pop_vertical.png"), p,  width=4.2, height=6, dpi=600)
ggsave(file.path(write_output_path, "pop_vertical.svg"), p,  width=4.2, height=6)
ggsave(file.path(write_output_path, "pop_vertical.pdf"), p,  width=4.2, height=6)


### Output to csv
write.csv(paper_summary_all_sims, file.path(write_output_path, "population_est.csv"))



###########################################################################
###  Print a Completion message
###########################################################################
print('===========================================================')
print('Done. All products should be replicated in the Output folder.') 
print('===========================================================')