# *------------------------------------------------------------------
# | PROGRAM NAME: 01_article_analysis
# | FILE NAME: 01_article_analysis.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code processes all articles from 2017, plots their keywords,
# | separates the keyword or non-keyword papers, and randomly assigns to reviewers.
# | This code will randomly assign papers, so it will not exactly reproduce
# | results from Stagge et al. (2018)
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

dir.create(write_output_base_path)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
#require(colorout)
require(assertthat)
require(staggefuncs)
require(tidyverse)
require(colorblindr)

### Load these functions for this unique project
require(stringr)

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

### Load global functions
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)


###########################################################################
## Read in Data
###########################################################################
### Read in article data
ems_df <- read.csv(file.path(data_path, "articles/ems_2017.csv"))
hess_df <- read.csv(file.path(data_path, "articles/hess_2017.csv"))
jawra_df <- read.csv(file.path(data_path, "articles/jawra_2017.csv"))
joh_df <- read.csv(file.path(data_path, "articles/joh_2017.csv"))
jwrpm_df <- read.csv(file.path(data_path, "articles/jwrpm_2017.csv"))
wrr_df <- read.csv(file.path(data_path, "articles/wrr_2017.csv"))

###########################################################################
## Combine into single dataframe
###########################################################################
all_journals_df <- rbind(ems_df, hess_df, jawra_df, joh_df, jwrpm_df, wrr_df)
journal_names <- levels(all_journals_df$Publication.Title)

###########################################################################
## Initial analysis - read through all tags and create a table showing tag frequency
###########################################################################
for (i in seq(1, length(journal_names))){

	journal_i <- journal_names[i]
	journal_subset <-  all_journals_df[all_journals_df$Publication.Title==journal_i,]

	### Extract tags
	tags_i <- journal_subset$Manual.Tags
	### Separate list organized by semicolons
	tags_i <- unlist(sapply(tags_i, function(x){strsplit(as.character(x), split="; ")}))
	### Shift everything to lower case
	tags_i <- tolower(tags_i)

	### First time through loop, create full data frame, all other times bind to bottom
	tag_i_df <- data.frame(journal=journal_i, tags=tags_i)
	if (i == 1){
		tags_df <- tag_i_df
	} else {
		tags_df <- rbind(tags_df, tag_i_df)
	}	
}

tags_table <- as.data.frame(table(tags_df$tags))
tags_by_journal <- as.data.frame(table(tags_df))

tag_test <- tags_table$Var1[tags_table$Freq > 250]

tag_sort <- tags_table$Var1[order(tags_table$Freq, decreasing=TRUE)]

#12,976 total tags

###########################################################################
## Plot tags frequency
###########################################################################
### Create output path
write_output_path <- file.path(write_output_base_path, "article_analysis")
dir.create(write_output_path)

### Set journal colors
journal_colors <- cb_pal(6)

### Plot first 40 tags
plot_df <- tags_by_journal[tags_by_journal$tags %in% tag_sort[1:40],]
plot_df$tags <- factor(plot_df$tags, levels=tag_sort)

p <- ggplot(plot_df, aes(x=tags, y=Freq))
p <- p + geom_bar(aes(fill = journal), position="stack", stat="identity")
p <- p + theme_classic_new()
p <- p + scale_fill_manual(name="Journal", values=journal_colors)
p <- p + theme(legend.position="bottom")
p <- p + theme(axis.text.x = element_text(angle = 70, hjust = 1))
p

ggsave(file.path(write_output_path,"tags_1-40.pdf"),  p, width=8, height=5)


### Plot next 40 tags
plot_df <- tags_by_journal[tags_by_journal$tags %in% tag_sort[41:80],]
plot_df$tags <- factor(plot_df$tags, levels=tag_sort)

p <- ggplot(plot_df, aes(x=tags, y=Freq))
p <- p + geom_bar(aes(fill = journal), position="stack", stat="identity")
p <- p + theme_classic_new()
p <- p + scale_fill_manual(name="Journal", values=journal_colors)
p <- p + theme(legend.position="bottom")
p <- p + theme(axis.text.x = element_text(angle = 70, hjust = 1))
p

ggsave(file.path(write_output_path,"tags_41-80.pdf"),  p, width=8, height=5)


### Plot next 40 tags
plot_df <- tags_by_journal[tags_by_journal$tags %in% tag_sort[81:120],]
plot_df$tags <- factor(plot_df$tags, levels=tag_sort)

p <- ggplot(plot_df, aes(x=tags, y=Freq))
p <- p + geom_bar(aes(fill = journal), position="stack", stat="identity")
p <- p + theme_classic_new()
p <- p + scale_fill_manual(name="Journal", values=journal_colors)
p <- p + theme(legend.position="bottom")
p <- p + theme(axis.text.x = element_text(angle = 70, hjust = 1))
p

ggsave(file.path(write_output_path,"tags_81-120.pdf"),  p, width=8, height=5)


###########################################################################
## Choose keywords manually by looking through previous tags for reproducibility terms
###########################################################################
#### Set an index column to use for all calculations
all_journals_df$index <- seq(1,dim(all_journals_df)[1])

### Name keywords
term_list <- c("analyticalsoftware", "applicationprograms", "c(programminglanguage)", "c\\+", "cloudcomputing", "computationalreproducibility", "computermodeling", "computerprogramming", "computersoftware", "computersoftwarereusability", "computer-basedmodels", "developmentandtesting", "engineeringsoftware", "fortran", "freelyavailabledata","freelyavailablesoftware", "github", "hardwareandsoftware", "java", "opencode", "opensource", "replicativevalidation", "scientificsoftware", "code", "python", "cran", "http")

abstract_list <- tolower(as.character(all_journals_df$Abstract.Note))
abstract_list <- gsub("[[:space:]]", "", abstract_list)

### Create list of all tags
keyword_list <- tolower(as.character(all_journals_df$Manual.Tags))
keyword_list <- gsub("[[:space:]]", "", keyword_list)

### Loop through each term counting the number of articles with each word in either the keyword or abstract
### Save the index number of each article with keywords to retrieve later
for (i in seq(1,length(term_list))){
	term_i <- term_list[i]
	
	abstract_i <- grep(term_i, abstract_list)
	keyword_i <- grep(term_i, keyword_list)
	
	length_abstract <- length(abstract_i)
	length_keyword <- length(keyword_i)
	
	index_i <- sort(unique(c(abstract_i, keyword_i)))
	
	results_i <- data.frame(term=term_i, keyword_n=length_keyword, abstract_n=length_abstract)
	
	if (i == 1){
		results_df <- results_i
		index_all <- index_i 
	} else {
		results_df <- rbind(results_df, results_i)
		index_all <- sort(unique(c(index_all, index_i)))
	}
}

	length_all <- length(index_all)

### Frequency table for keywords
results_df

### Report number of articles with keywords
length_all

### Report index of articles with keywords
index_all


###########################################################################
## Cut based on keywords
###########################################################################
keyword_subset <- all_journals_df[index_all,]

nonkeyword_index <- all_journals_df$index[ !(all_journals_df$index %in% index_all)]
nonkeyword_subset <- all_journals_df[nonkeyword_index,]

###########################################################################
## Summarize based on keywords and journal
###########################################################################

pub_table <- cbind(table(keyword_subset$Publication.Title),
table(nonkeyword_subset$Publication.Title), table(all_journals_df$Publication.Title))

pub_table <- data.frame(journal = rownames(pub_table), pub_table)
rownames(pub_table) <- as.character(seq(1,dim(pub_table)[1]))
pub_table <- pub_table %>% 
	rename(keyword = X1, none = X2, total = X3)
	
pub_table$journal_abbrev <- c("EM&S", "HESS", "JAWRA", "JoH", "JWRP&M", "WRR" )

###########################################################################
## Write publication summary table
###########################################################################
### View keyword table on screen
pub_table

### Save to CSV file
write_file <- file.path(write_output_path, "pub_summary_table.csv")
write.csv(pub_table, write_file)
  
###########################################################################
## Prepare to randomly sample
###########################################################################
### Estimate how many papers would be sampled per journal if sampling was proportional and n equal 360
common_ratio_est <- round((pub_table[,4]/sum(pub_table[,4]))*360)
names(common_ratio_est) <- pub_table$journal
common_ratio_est

### Manually modify the ratios to ensure at least 30 articles and at least 15 non-keywords
corrected_ratio_est <- common_ratio_est
corrected_ratio_est[1] <- 49+15 #49 in keywords, make sure at least 15 random
corrected_ratio_est[3] <- 30  ### Minimum of 30 articles
corrected_ratio_est[5] <- 30  ### Minimum of 30 articles

### Print the resulting number of articles and the overage (more than 360)
sum(corrected_ratio_est)
corrected_ratio_est
360 - sum(corrected_ratio_est)

### Manually reduce the non-modified journals to produce exactly 360 sampled articles
corrected_ratio_est[2] <- corrected_ratio_est[2] - 8
corrected_ratio_est[4] <- corrected_ratio_est[4] - 18
corrected_ratio_est[6] <- corrected_ratio_est[6] - 15

### Double check the corrected proportions equal 360
sum(corrected_ratio_est)

### Calculate the number of non-keyword papers by subtracting 
required_nonkeyword <- corrected_ratio_est - pub_table[,2]


### Randomly sample Non-Keyword papers, save the index number for each article
for (j in seq(1,length(required_nonkeyword))){
	
	 pub_test <- nonkeyword_subset$Publication.Title %in% names(required_nonkeyword)[j]
	 pubs_j <- nonkeyword_subset$index[pub_test]
	 
	sample_j <- sample(pubs_j, size=required_nonkeyword[j], replace = FALSE)
	
	if(j ==1){
		sample_nonkeyword <- sample_j
	} else {
		sample_nonkeyword <- c(sample_nonkeyword, sample_j)
	}
	
}	 
	
sample_nonkeyword <- sort(sample_nonkeyword)

  
###########################################################################
## Output random sample indices
###########################################################################
### Reminder that these files will be saved in a separate github folder because they do not match
### exactly with the randomly sampled articles in the paper
### Create output path
write_output_path <- file.path(write_output_base_path, "article_analysis_github")
dir.create(write_output_path)

### Output indices of keyword papers
write.csv( index_all, file.path(write_output_path,"sampled_keywords_github.csv"))
### Output indices of non-keyword papers
write.csv( sample_nonkeyword, file.path(write_output_path,"sampled_nonkeywords_github.csv"))

### Combine and output indices of all sampled papers
sampled_indices <- c(index_all, sample_nonkeyword)
write.csv( sampled_indices, file.path(write_output_path,"sampled_indices_github.csv"))


###########################################################################
## Output random sample indices
###########################################################################
### Assign reviewers
reviewers <- c("Reviewer1", "Reviewer2","Reviewer3", "Reviewer4", "Reviewer5", "Reviewer6")

### Randomly sorted publications
random_indices <- sample(sampled_indices)

### Randomly assign 60 papers to each of 6 reviewers (360 papers)
### Create a CSV spreadsheet to provide citation information to reviewers
paper_assign <- data.frame(Reviewer=rep(reviewers, each=60))
paper_assign$index <- random_indices
paper_assign$DOI <- all_journals_df$DOI[random_indices]
paper_assign$URL <- paste0("https://doi.org/", paper_assign$DOI)
paper_assign$Citation <- paste0(all_journals_df$Author[random_indices], ' (', all_journals_df$Publication.Year[random_indices], ') ', all_journals_df$Title[random_indices], '. ', all_journals_df$Publication.Title[random_indices], '. ', all_journals_df$Volume[random_indices], ', ', all_journals_df$Pages[random_indices],'.')
paper_assign$Journal <- as.character(all_journals_df$Publication.Title[random_indices])

paper_assign <- with(paper_assign, paper_assign[order(Reviewer, index),])

### Output paper assignments
write.csv(paper_assign, file.path(write_output_path,"paper_assign_github.csv"), row.names=FALSE)


