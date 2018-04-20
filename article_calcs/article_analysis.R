


ems_df <- read.csv("ems_2017.csv")
hess_df <- read.csv("hess_2017.csv")
jawra_df <- read.csv("jawra_2017.csv")
joh_df <- read.csv("joh_2017.csv")
jwrpm_df <- read.csv("jwrpm_2017.csv")
wrr_df <- read.csv("wrr_2017.csv")

all_journals_df <- rbind(ems_df, hess_df, jawra_df, joh_df, jwrpm_df, wrr_df)

journal_names <- levels(all_journals_df$Publication.Title)

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

require(ggplot2)
require(staggefuncs)

journal_colors <- cb_pal(6)

plot_df <- tags_by_journal[tags_by_journal$tags %in% tag_sort[1:40],]
plot_df$tags <- factor(plot_df$tags, levels=tag_sort)

p <- ggplot(plot_df, aes(x=tags, y=Freq))
p <- p + geom_bar(aes(fill = journal), position="stack", stat="identity")
p <- p + theme_classic_new()
p <- p + scale_fill_manual(name="Journal", values=journal_colors)
p <- p + theme(legend.position="bottom")
p <- p + theme(axis.text.x = element_text(angle = 70, hjust = 1))
p

ggsave("tags_1-40.pdf",  p, width=8, height=5)



plot_df <- tags_by_journal[tags_by_journal$tags %in% tag_sort[41:80],]
plot_df$tags <- factor(plot_df$tags, levels=tag_sort)

p <- ggplot(plot_df, aes(x=tags, y=Freq))
p <- p + geom_bar(aes(fill = journal), position="stack", stat="identity")
p <- p + theme_classic_new()
p <- p + scale_fill_manual(name="Journal", values=journal_colors)
p <- p + theme(legend.position="bottom")
p <- p + theme(axis.text.x = element_text(angle = 70, hjust = 1))
p

ggsave("tags_41-80.pdf",  p, width=8, height=5)



plot_df <- tags_by_journal[tags_by_journal$tags %in% tag_sort[81:120],]
plot_df$tags <- factor(plot_df$tags, levels=tag_sort)

p <- ggplot(plot_df, aes(x=tags, y=Freq))
p <- p + geom_bar(aes(fill = journal), position="stack", stat="identity")
p <- p + theme_classic_new()
p <- p + scale_fill_manual(name="Journal", values=journal_colors)
p <- p + theme(legend.position="bottom")
p <- p + theme(axis.text.x = element_text(angle = 70, hjust = 1))
p

ggsave("tags_81-120.pdf",  p, width=8, height=5)




strsplit(as.character(ems_tags[[10]]), split="; ")




all_journals_df[ grep("algorithm", as.character(all_journals_df$Manual.Tags)),]



all_journals_df[ grep('software', as.character(all_journals_df$Manual.Tags)),]

yup <- gsub("[[:space:]]", "", as.character(all_journals_df$Manual.Tags))
yup <- tolower(yup)

all_journals_df[grep('analyticalsoftware', yup),]



all_journals_df$Abstract.Note



grep('github', as.character(all_journals_df$Abstract.Note))
 
 
yup <- tolower(as.character(all_journals_df$Abstract.Note))
yup <- gsub("[[:space:]]", "", yup)
all_journals_df[grep('opensource', yup),]

 
 
#############################################################################
# as.character(all_journals_df$Manual.Tags)
# "software",

#### Set an index column to use for all calculations
all_journals_df$index <- seq(1,dim(all_journals_df)[1])
 
term_list <- c("analyticalsoftware", "applicationprograms", "c(programminglanguage)", "c\\+", "cloudcomputing", "computationalreproducibility", "computermodeling", "computerprogramming", "computersoftware", "computersoftwarereusability", "computer-basedmodels", "developmentandtesting", "engineeringsoftware", "fortran", "freelyavailabledata","freelyavailablesoftware", "github", "hardwareandsoftware", "java", "opencode", "opensource", "replicativevalidation", "scientificsoftware", "code", "python", "cran", "http")

abstract_list <- tolower(as.character(all_journals_df$Abstract.Note))
abstract_list <- gsub("[[:space:]]", "", abstract_list)

keyword_list <- tolower(as.character(all_journals_df$Manual.Tags))
keyword_list <- gsub("[[:space:]]", "", keyword_list)

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


results_df
length_all

index_all



keyword_subset <- all_journals_df[index_all,]

nonkeyword_index <- all_journals_df$index[ !(all_journals_df$index %in% index_all)]
nonkeyword_subset <- all_journals_df[nonkeyword_index,]


table(keyword_subset$Publication.Title)
table(nonkeyword_subset$Publication.Title)

pub_table <- cbind(table(keyword_subset$Publication.Title),
table(nonkeyword_subset$Publication.Title), table(all_journals_df$Publication.Title))

common_ratio_est <- round((pub_table[,3]/sum(pub_table[,3]))*360)
common_ratio_est

corrected_ratio_est <- common_ratio_est
corrected_ratio_est[1] <- 49+15 #49 in keywords, make sure at least 15 random
corrected_ratio_est[3] <- 30  ### Minimum of 30 articles
corrected_ratio_est[5] <- 30  ### Minimum of 30 articles
sum(corrected_ratio_est)
corrected_ratio_est
360 - sum(corrected_ratio_est)

corrected_ratio_est[2] <- corrected_ratio_est[2] - 8
corrected_ratio_est[4] <- corrected_ratio_est[4] - 18
corrected_ratio_est[6] <- corrected_ratio_est[6] - 15

sum(corrected_ratio_est)

required_nonkeyword <- corrected_ratio_est - pub_table[,1]

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


write.csv( index_all, "sampled_keywords.csv")
write.csv( sample_nonkeyword, "sampled_nonkeywords.csv")

sampled_indices <- c(index_all, sample_nonkeyword)
write.csv( sampled_indices, "sampled_indices.csv")

### Assign reviewers
reviewers <- c("Adel", "David","Hadia", "Jim", "Nour", "Ryan")

### Randomly sorted publications
random_indices <- sample(sampled_indices)


paper_assign <- data.frame(Reviewer=rep(reviewers, each=60))
paper_assign$index <- random_indices
paper_assign$DOI <- all_journals_df$DOI[random_indices]
paper_assign$URL <- paste0("https://doi.org/", paper_assign$DOI)
paper_assign$Citation <- paste0(all_journals_df$Author[random_indices], ' (', all_journals_df$Publication.Year[random_indices], ') ', all_journals_df$Title[random_indices], '. ', all_journals_df$Publication.Title[random_indices], '. ', all_journals_df$Volume[random_indices], ', ', all_journals_df$Pages[random_indices],'.')
paper_assign$Journal <- as.character(all_journals_df$Publication.Title[random_indices])

paper_assign <- with(paper_assign, paper_assign[order(Reviewer, index),])

write.csv(paper_assign, "paper_assign.csv", row.names=FALSE)






https://doi.org/10.5194/hess-21-3619-2017


Pages
Volume
Publication.Title
Title
Author
Publication.Year

DOI 

URL
