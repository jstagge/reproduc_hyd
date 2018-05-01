
avail_journal_keyword <- reproduc_df %>% 
	select(Q2_abbrev, Q5, keyword) %>%
	count(Q2_abbrev, Q5, keyword) %>% 
	complete(Q2_abbrev, Q5, keyword, fill = list(n = 0)) %>%
	mutate(prop_all = prop.table(n)) %>%
	group_by(Q2_abbrev, keyword) %>% 
	mutate(prop_by_journal = n/sum(n)) %>% 
	as.data.frame()

### Reorder the levels for question Q5
avail_journal_keyword$Q5 <- factor(avail_journal_keyword$Q5, levels=levels(avail_journal_keyword$Q5)[c(2,1,3)])
avail_journal_keyword$Q5_leading <- factor(avail_journal_keyword$Q5, levels=levels(avail_journal_keyword$Q5)[c(3,2,1)])

  avail_journal_keyword$keyword <- factor(avail_journal_keyword$keyword, levels=c(TRUE, FALSE), labels=c("Keyword", "None"))
  
### Plot as separate bars  
  p <- ggplot(data = reproduc_df, aes(x = Q2_abbrev, fill=keyword)) 
  p <- p + geom_bar(position = position_stack())
  #p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  #p <- p + scale_fill_manual(name="Availability claim", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
 # p <- p + facet_wrap(~keyword, drop=FALSE)
  p 
  
  
### Plot as separate bars  
  p <- ggplot(data = avail_journal_keyword, aes(x = Q2_abbrev, y = prop_by_journal, fill = Q5_leading)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
  p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Availability claim", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p <- p + facet_wrap(~keyword, drop=FALSE)
  p 
  

  p <- ggplot(data = avail_journal_keyword, aes(x = Q2_abbrev, y = prop_by_journal, fill = keyword)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
  p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Keyword", values=c("black", "grey60")) 
  #p <- p + scale_fill_manual(name="Availability claim", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  p <- p + facet_wrap(~ Q5_leading , drop=FALSE, ncol=1)
  p 
  
 
  p <- ggplot(data = subset(avail_journal_keyword, Q5_leading=="Some or all available"), aes(x = Q2_abbrev, y = prop_by_journal, fill = keyword)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
  p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  p <- p + scale_fill_manual(name="Keyword", values=c("black", "grey60")) 
  #p <- p + scale_fill_manual(name="Availability claim", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  #p <- p + facet_wrap(~ Q5_leading , drop=FALSE, ncol=1)
  p 
  
  
  
  
 ### How many papers in each category for the year?
 
yup <- pub_summary_table %>%
 	select(-journal, -X, -total) %>%
 	gather(keyword, count, -journal_abbrev)

yup$keyword[yup$keyword == "keyword"] <- "Keyword"
yup$keyword[yup$keyword == "none"] <- "None"
 
 	
yup2 <-  avail_journal_keyword %>%
 	left_join(yup, by = c("Q2_abbrev" = "journal_abbrev", "keyword" = "keyword"))
 
yup2$estimate <- yup2$count * yup2$prop_by_journal 
  
  
  p <- ggplot(data = yup2, aes(x = Q2_abbrev, y = estimate, fill = Q5_leading, alpha=keyword)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))
  #p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  #p <- p + scale_fill_manual(name="Keyword", values=c("black", "grey60")) 
  #p <- p + scale_fill_manual(name="Availability claim", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  #p <- p + facet_wrap(~ Q5_leading , drop=FALSE, ncol=1)
  p 
  
 yup2$interaction <- paste0(yup2$Q2_abbrev, " - ", yup2$Q5_leading)
 #yup2$keyword <- factor(yup2$keyword, levels=c("None", "Keyword"))
 
  p <- ggplot(data = yup2, aes(x = Q2_abbrev, y = estimate, fill = Q5_leading)) 
  p <- p + geom_bar(stat = 'identity', width=0.8, position = position_dodge(width=0.8))#, color="black")
  #p <- p + scale_y_continuous(name="Proportion of Articles", labels = scales::percent, expand = c(0, 0), limits = c(0, 1))
  p <- p + scale_x_discrete(name="Journal")
  #p <- p + scale_fill_manual(name="Keyword", values=c("black", "grey60")) 
  #p <- p + scale_fill_manual(name="Availability claim", values=cb_pal("custom", n=3, sort=FALSE), limits=levels(avail_journal$Q5_leading)) 
  p <- p + theme_classic_new(9.5) +   theme(legend.position="bottom")
  #p <- p + facet_wrap(~ Q5_leading , drop=FALSE, ncol=1)
  p 
  
    
  


