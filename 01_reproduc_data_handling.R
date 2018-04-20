# *------------------------------------------------------------------
# | PROGRAM NAME: 03_ap_model_fit
# | FILE NAME: 03_ap_model_fit.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This is a code wrapper to fit the Annual Percentile (AP) model.
# | It fits cumulative probability distributions for annual and monthly flows.
# |
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  
# |  2: 
# |  3: 
# |*------------------------------------------------------------------
# | DATA USED:               
# | USGS gauge flow data
# | Annual reconstructions from:
# | Allen, E.B., Rittenour, T.M., DeRose, R.J., Bekker, M.F., Kjelgren, R., Buckley, B.M., 2013. A tree-ring based reconstruction of Logan River streamflow, northern Utah. Water Resources Research 49, 8579–8588. doi:10.1002/2013WR014273.
# |
# | DeRose, R.J., Bekker, M.F., Wang, S.Y., Buckley, B.M., Kjelgren, R.K., Bardsley, T., Rittenour, T.M., Allen, E.B., 2015. A millennium-length reconstruction of Bear River stream flow, Utah. Journal of Hydrology 529, Part 2, 524–534. doi:10.1016/j.jhydrol.2015.01.014.
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  
# |  PART 2: 
# |  PART 3: 
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# *------------------------------------------------------------------

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

### Load these functions for this unique project
require(stringr)

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

### Load global functions
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)



###########################################################################
## Set Initial Values
###########################################################################

###########################################################################
###  Read in Data
###########################################################################

### Read in reproducibility data
read_location <- file.path(data_path, "Reproducibility_survey_April18.csv")

reproduc_df <- read.csv(file = read_location)

### Consider trying the function str_clean()  from explarotaory packages to clean up \n and things like that

### Can use this to pull out comma delimited data into separate rows
#mutate(`Select Investors` = str_split(`Select Investors`, ", "))
#unnest(`Select Investors`)

###########################################################################
###  Drop duplicates
###########################################################################
### Check for duplicates
duplicate_df <- reproduc_df %>% 
	group_by(Q3) %>% 
	filter(n()>1) %>% 
	summarize(n=n(), Reviewer=Q1[1], Title=Q4[1]) %>%
	arrange(Reviewer, Q3)

data.frame(duplicate_df)


### Remove duplicates
reproduc_df <- reproduc_df %>% 
  	distinct("Q3") %>%
  	arrange(Q2, Q1, Q3)
  	

###########################################################################
###  Drop missing rows
###########################################################################
#df %>% drop_na(x)

# Q2 = journal
# Q3 = doi
# Q4 = citation
# Q5 = Some or all available?*** Data-less? Not specified?
# Q6 = Author request, third party, available only in article, Some or all found online*** [[[Comma separated column, can be multiples]]]
# Q7 = ***Directions to run, Code/Model/Software, Input Data,      Hardware/Software requirements, File format, instructions to open [[[Comma separated column]]]
# Q8 = comments
# Q9 = Do I think i can do it: yes**, no, not sure**, not familiar with computational**


stats_class <- reproduc_df %>% 
	group_by(Q2, Q5) %>%
	summarize(Count = n()) %>%
	spread(Q5, Count, fill = 0) %>%
	as.data.frame()
 
yup <- stats_class %>%
	select(-Q2) %>%
	mutate(total=rowSums(.)) %>%
	as.data.frame()

yuppers <- reproduc_df %>% 
	select(Q2, Q5) %>%
	count(Q2, Q5) %>%  
	mutate(prop_all = prop.table(n)) %>%
	group_by(Q2) %>% 
	mutate(prop_by_journal = n/sum(n)) %>% 
	as.data.frame()
 
 
  
  p <- ggplot(data = yuppers, aes(x = Q2, y = prop, fill = Q5)) 
  p <- p + geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3)
  p <- p + scale_y_continuous(labels = scales::percent) 
  #p <- p + scale_fill_few('medium', drop = FALSE) 
  p <- p +  labs(x = 'Cylinders', y = NULL, fill = 'Gears', title = 'Proportions in Sub-groups by Group')  # what's the reader looking at?
  
  	
  p <- ggplot(data = yuppers, aes(x = Q2, y = n, fill = Q5)) 
  p <- p + geom_bar(stat = 'identity', position = 'stack')
  p <- p + scale_y_continuous()
  #p <- p + scale_fill_few('medium', drop = FALSE) 
  p <- p +  labs(x = 'Cylinders', y = NULL, fill = 'Gears', title = 'Proportions in Sub-groups by Group')  # what's the reader looking at?
  p <- p + theme_classic_new() +   theme(legend.position="bottom")
  p 
  


 
 #################
 ###  River Plot
 ################ 
  
  #######################
  ###  First edge/node
  #######################
  
  ### Data prept
  reproduc_df$Q5_num <- as.numeric(reproduc_df$Q5)
  head(reproduc_df$Q5_num )
  q5_options <- seq(1,length(levels(reproduc_df$Q5)))
  q5_levels <- paste0("Q5_", q5_options)
  reproduc_df$Q5_num <- factor(reproduc_df$Q5_num, levels=q5_options, labels=q5_levels)
  levels(reproduc_df$Q5)
  q5_labels <- c("Dataless", "Not\nSpecified", "Some\nAvailable")

### Calculate Edges  
q5_freq <- reproduc_df %>% 
		group_by(Q5_num) %>%
		summarize(Count = n()) %>%
		spread(Q5_num, Count, fill = 0) %>%
		gather() %>%
		as.data.frame()

q5_freq$N2 <- q5_freq$key
q5_freq$N1 <- "Q0_1"
q5_freq$Value <- q5_freq$value
	
paper_edges_level2 <- q5_freq %>% select(N1, N2, Value)

### Calculate nodes 
paper_nodes_level1 <- data.frame(ID="Q0_1", x=1, y=1, labels="All Papers")
paper_nodes_level2 <- data.frame(ID=q5_levels, x=2, y=c(0,1,2), labels=q5_labels)

  #######################
  ###  Q5 to Q6
  #######################
  
  ### Data prep
reproduc_df$Q6_grouping <- "No"
### If it contains Tin article at all
reproduc_df$Q6_grouping[str_detect(reproduc_df$Q6, "figures/tables")] <- "In\nArticle"
### If available in paper
reproduc_df$Q6_grouping[str_detect(reproduc_df$Q6, "online")] <- "Some or\nAll Available"
### If it contains Author Request at all
reproduc_df$Q6_grouping[str_detect(reproduc_df$Q6, "Author")] <- "Author\nRequest"
### If it contains Third Party Request at all (trumps Author Request)
reproduc_df$Q6_grouping[str_detect(reproduc_df$Q6, "Third Party")] <- "Third\nParty"

q6_labels <- c("Some or\nAll Available", "In\nArticle", "Author\nRequest", "Third\nParty", "No")
reproduc_df$Q6_grouping <- factor(reproduc_df$Q6_grouping, levels=q6_labels)
 
reproduc_df$Q6_num <- as.numeric(reproduc_df$Q6_grouping)
head(reproduc_df$Q6_num )
q6_options <- seq(1,length(levels(reproduc_df$Q6_grouping)))
q6_levels <- paste0("Q6_", q6_options)
reproduc_df$Q6_num <- factor(reproduc_df$Q6_num, levels=q6_options, labels=q6_levels)
   
### Calculate Edges  
q6_freq <- reproduc_df %>% 
		group_by(Q5_num, Q6_num) %>%
		summarize(Count = n()) %>%
		as.data.frame()

paper_edges_level3 <- q6_freq %>%
	mutate(N1=Q5_num, N2=Q6_num) %>%
	mutate(Value = Count) %>%
	select(N1, N2, Value)

### Calculate nodes 
paper_nodes_level3 <- data.frame(ID=q6_levels, x=3, y=c(4,3,2,1,0), labels=q6_labels)



################################
###  Combine all data
#################################
paper_edges <- rbind(paper_edges_level2, paper_edges_level3)
paper_nodes <- rbind(paper_nodes_level1, paper_nodes_level2, paper_nodes_level3)
rownames(paper_nodes) <- paper_nodes$ID
 
library(riverplot)

# create a custom style by first copying from the default style
styles <- riverplot::default.style()
# change the font size
#styles$textcex <- 0.01

rp <- list(nodes = paper_nodes, edges = paper_edges, styles=styles)#, styles = styles)
class(rp) <- c(class(rp), "riverplot")
rp

png('papers_riverplot.png', width = 12, height = 9, units = 'in', res = 300)
plot(rp)
dev.off()

svg('papers_riverplot.svg', width = 12, height = 9)#, units = 'in')#, res = 300)
plot(rp)
dev.off()
























  
  
edges = data.frame(N1 = paste0(rep(LETTERS[1:4], each = 4), rep(1:5, each = 16)),
                   N2 = paste0(rep(LETTERS[1:4], 4), rep(2:6, each = 16)),
                   Value = runif(80, min = 2, max = 5) * rep(c(1, 0.8, 0.6, 0.4, 0.3), each = 16),
                   stringsAsFactors = F)

edges = edges[sample(c(TRUE, FALSE), nrow(edges), replace = TRUE, prob = c(0.8, 0.2)),]
head(edges)

nodes = data.frame(ID = unique(c(edges$N1, edges$N2)), stringsAsFactors = FALSE)
nodes$x = as.integer(substr(nodes$ID, 2, 2))
nodes$y = as.integer(sapply(substr(nodes$ID, 1, 1), charToRaw)) - 65

rownames(nodes) = nodes$ID
head(nodes)

library(RColorBrewer)

palette = paste0(brewer.pal(4, "Set1"), "60")
styles = lapply(nodes$y, function(n) {
  list(col = palette[n+1], lty = 0, textcol = "black")
})
names(styles) = nodes$ID

library(riverplot)

rp <- list(nodes = nodes, edges = edges, styles = styles)
class(rp) <- c(class(rp), "riverplot")

plot(rp, plot_area = 0.95, yscale=0.06)





require(ggalluvial)



titanic_wide <- data.frame(Titanic)
head(titanic_wide)

p <- ggplot(data = titanic_wide, aes(axis1 = Class, axis2 = Sex, axis3 = Age, weight = Freq))
p <- p + scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.1, .05))
#p <- p + geom_alluvium(aes(fill = Survived)) 
p <- p + geom_alluvium() 
p <- p + geom_stratum() 
p


test_data <- data.frame(Q2=c("Pass", "Fail"), Q4=c(), Freq)



+ geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")




head(as.data.frame(UCBAdmissions), n = 12)


p <- ggplot(as.data.frame(Titanic), aes(weight = Freq, axis1 = Survived, axis2 = Sex, axis3 = Class))
	p <- p + geom_alluvium(aes(fill = Class), width = 0, knot.pos = 0, reverse = FALSE)
	
	 +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
  ggtitle("Titanic survival by class and sex")
  
  
 