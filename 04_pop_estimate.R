
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

yup <- final_determ %>% 
	#select(Q2_abbrev, keyword, final_determ) %>% 
	group_by(Q2_abbrev, keyword, final_determ) %>%
	summarize(count = n()) %>%
	complete(Q2_abbrev, keyword, final_determ, fill = list(count = 0)) %>%
	distinct



###########################################################################
## Loop and resample
###########################################################################
require(MultinomialCI)

n_runs <- 5000

### Set the random seed so results can be reproduced
set.seed(6511)

start_time <- Sys.time()


for (j in seq(1,n_runs)) {

cat(paste0("  j: ", j, "   of: ", n_runs, "\n"))

for (i in seq(1, dim(categories)[1])){
	### Extract loop characteristics
	journal_i <- categories$journal_abbrev[i]
	keyword_i <- categories$keyword[i]
	n_i <- categories$n[i]
	n_sampled_i <- categories$n_sampled_need[i]
	n_unsampled_i <- categories$n_unsampled_need[i]
	
	### Subset papers
	papers_i <- yup %>% 
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


### Add column for total number of papers
### Use this to calculate proportion
paper_totals <- pub_summary_table %>% 
	select(Q2_abbrev=journal_abbrev, total) %>%
	bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
	
yup <- papers_pop %>%
	left_join(paper_totals)

yup <- yup %>% mutate(proportion=count/total)

yup$Q2_abbrev <- factor(yup$Q2_abbrev, levels=c(levels(reproduc_df$Q2_abbrev), "Total"))


ggplot(papers_pop, aes(x=final_determ, fill=Q2_abbrev, y=count)) + geom_boxplot(position="dodge") + theme_classic_correct()

ggplot(yup, aes(x=final_determ, fill=Q2_abbrev, y=proportion)) + geom_boxplot(position="dodge") + scale_fill_manual(values=c(journal_colors,"grey50"))+ theme_classic_correct()


ggplot(yup, aes(x=final_determ, fill=Q2_abbrev, y=proportion)) + geom_violin(position="dodge") + scale_fill_manual(values=c(journal_colors,"grey50"))+ theme_classic_correct()

yup_summary <- yup %>%
	group_by(Q2_abbrev, final_determ) %>%
	summarize(median = quantile(proportion, 0.5), ll = quantile(proportion, 0.025), ul= quantile(proportion, 0.975))

p <- ggplot(yup_summary, aes(x = final_determ, group=Q2_abbrev, colour=Q2_abbrev)) + geom_errorbar(aes(ymin = ll, ymax = ul), position = position_dodge(width = 0.5), size=2) + theme_classic() + scale_colour_manual(values=c(journal_colors,"black"))
p



p + coord_flip() + scale_x_discrete(limits = rev(levels(yup_summary$final_determ))) 

 
 p <- ggplot(yup_summary, aes(x = final_determ, group=Q2_abbrev, colour=Q2_abbrev)) + geom_pointrange(aes(y=median, ymin = ll, ymax = ul), position = position_dodge(width = 0.5), size=1) + theme_classic() + scale_colour_manual(values=c(journal_colors,"black"))
p

plot_labels <- c("Dataless or\nReview", "Author or Third Party\nRequest Only", "No Availability", "Some Availability", "Available, but\nNot Replicable", "Some or All\nReplicable")

p <- ggplot(yup_summary, aes(x = final_determ, group=Q2_abbrev, colour=Q2_abbrev)) %>%
	+ geom_pointrange(aes(y=median, ymin = ll, ymax = ul), position = position_dodge(width = 0.5), size=.75) %>%
	+ theme_classic_new(10) %>%
	+ scale_colour_manual(name ="Journal", values=c(journal_colors,"black")) %>%
	+ scale_x_discrete(name="Reproducibility Level", labels=plot_labels) %>%
	+ scale_y_continuous(name="Estimated Proportion", labels = scales::percent, expand = c(0, 0), limits = c(0, .7)) %>%
	+ theme(legend.position = c(0.95, 0.75))
	
p

### Save figure
ggsave(paste0(file.path(write_figures_path,"png/"), "pop_horizontal", ".png"), p, width=7, height=4, dpi=600)

ggsave(paste0(file.path(write_figures_path,"svg/"), "pop_horizontal", ".svg"), p, width=7, height=4)
ggsave(paste0(file.path(write_figures_path,"pdf/"), "pop_horizontal", ".pdf"), p, width=7, height=4)



p <- ggplot(yup_summary, aes(x = final_determ, group=Q2_abbrev, colour=Q2_abbrev)) %>%
	+ geom_pointrange(aes(y=median, ymin = ll, ymax = ul), position = position_dodge(width = -0.4), size=.75) %>%
	+ theme_classic_new(10) %>%
	+ scale_colour_manual(name="Journal", values=c(journal_colors,"black")) %>%
	+ coord_flip() %>%
	+ scale_x_discrete(name="Reproducibility Level", limits = rev(levels(yup_summary$final_determ)), labels=rev(plot_labels)) %>%
	+ scale_y_continuous(name="Estimated Proportion)", labels = scales::percent, expand = c(0, 0), limits = c(0, .7)) %>%
	+ theme(legend.position = c(0.87, 0.2))

p

### Save figure
ggsave(paste0(file.path(write_figures_path,"png/"), "pop_vertical", ".png"), p, width=4.2, height=6, dpi=600)



### Output to csv
write.csv(yup_summary, file.path(write_output_base_path, "population_est.csv"))





















require(ggridges)
ggplot(yup, aes(y=final_determ, fill=Q2_abbrev, x=proportion)) + geom_density_ridges()

ggplot(yup, aes(y=final_determ, fill=Q2_abbrev, x=proportion)) + geom_density_ridges2(stat="binline",alpha=0.8) + scale_fill_manual(values=c(journal_colors,"grey50"))


ggplot(yup,x = 
 geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.25)
 
 

geom_dumbbell for the 95% confidence interval
violin plot





geom_ribbon(aes(ymin = as.integer(activity), ymax = as.integer(activity) + 2 * p_smooth), color='white', size=0.4)







n_sample <- yup %>% filter(Q2_abbrev == "JAWRA" & keyword == FALSE)

require(MultinomialCI)
multinomialCI(n_sample$count, 0.05)

require(CoinMinD)
SG(n_sample$count,0.05)
GM(n_sample$count,0.05)
BMDE(n_sample$count,1)

yup3 <- capture.output(BMDE(n_sample$count,1))

pop_est <- data.frame(mean=strsplit(yup3[2], split=" "), ll=strsplit(yup3[4], split=" "), ul=strsplit(yup3[6], split=" "))

pop_est <- pop_est[seq(2,dim(pop_est)[1]),]
pop_est <- mutate_all(pop_est, funs(as.character))
pop_est <- mutate_all(pop_est, funs(as.numeric))
colnames(pop_est) <- c("mean", "ll", "ul")

yup6 <- data.frame(n_sample, pop_est)			
names(yup6)[1] <- "journal_abbrev"
yup6$journal_abbrev <- factor(yup6$journal_abbrev, journal_abbrev)


left_join(categories, yup6, by = c("journal_abbrev", "keyword"))




for (i in seq(1, dim(categories)[1])){
	### Extract loop characteristics
	journal_i <- categories$journal_abbrev[i]
	keyword_i <- categories$keyword[i]
	n_i <- categories$n[i]
	n_sampled_i <- categories$n_sampled_need[i]
	n_unsampled_i <- categories$n_unsampled_need[i]
	
	### sampled
	n_sample <- yup %>% filter(Q2_abbrev == journal_i & keyword == keyword_i)

	yup3 <- capture.output(BMDE(n_sample$count,1))

	pop_est <- data.frame(mean=strsplit(yup3[2], split=" "), ll=strsplit(yup3[4], split=" "), ul=strsplit(yup3[6], split=" "))

	pop_est <- pop_est[seq(2,dim(pop_est)[1]),]
	pop_est <- mutate_all(pop_est, funs(as.character))
	pop_est <- mutate_all(pop_est, funs(as.numeric))
	colnames(pop_est) <- c("mean", "ll", "ul")

	yup6 <- data.frame(n_sample, pop_est)			
	names(yup6)[1] <- "journal_abbrev"
	yup6$journal_abbrev <- factor(yup6$journal_abbrev, journal_abbrev)

	if (i == 1){
		categories_yup <- yup6
	} else {
		categories_yup <- rbind(categories_yup, yup6)
	}
}

uhhuh <- categories %>% 
	select(journal_abbrev, keyword, n_unsampled_need) %>%
	right_join(categories_yup)

uhhuh2 <- uhhuh %>% 
	mutate(mean_papers = n_unsampled_need * mean,
		ll_papers = n_unsampled_need * ll,
		ul_papers = n_unsampled_need * ul
		)

ggplot(uhhuh2, aes(x=journal_abbrev, group=final_determ, fill=final_determ, y=mean_papers)) + geom_bar(stat="identity")

ggplot(uhhuh2, aes(x=journal_abbrev, group=final_determ, colour=final_determ, y=mean_papers, ymin=ll_papers, ymax=ul_papers)) + geom_pointrange()

ggplot(uhhuh2, aes(x=journal_abbrev, group=final_determ, colour=final_determ, y=mean, ymin=ll, ymax=ul)) + geom_pointrange()

ggplot(uhhuh2, aes(x=journal_abbrev, group=final_determ, colour=final_determ, y=mean, ymin=ll, ymax=ul))+geom_errorbar(na.rm=TRUE,position="dodge")

### Because not doing keywords
ggplot(uhhuh2, aes(x=final_determ, group=journal_abbrev, colour=journal_abbrev, y=mean, ymin=ll, ymax=ul))+geom_errorbar(na.rm=TRUE,position="dodge")



   +geom_point(aes(shape=final_determ), na.rm=TRUE,position="dodge") 
   
   
, position="dodge"



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





