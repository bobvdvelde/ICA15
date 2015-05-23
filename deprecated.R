
#### old stuff ####

### noun based
adj_tokens        <- tokens[tokens$pos %in% nouns,]
adj_dtm           <- dtm.create(documents = adj_tokens$aid, terms=adj_tokens$lemma )
adj_termstats     <- term.statistics(adj_dtm)
adj_filter        <- adj_termstats[adj_termstats$docfreq >- 5 & adj_termstats$number==F, ]
adj_filtered      <- adj_dtm[,adj_filter$term]
noun_model        <- lda.fit(adj_filtered , K = 50, num.iterations = 1000)
meta2             <- meta[match(noun_model@documents,meta$id),]
assignment        <- data.frame(i=noun_model@wordassignments$i, j=noun_model@wordassignments$j, v=noun_model@wordassignments$v)
docsums           <- acast(assignment, i ~ v, value.var='j', fun.aggregate=length) 
colnames(docsums) <- paste("Topic_",1:50, sep="")
full_meta_noun    <- cbind(meta2,docsums)

### comb based

adj_tokens        <- tokens[tokens$pos %in% c(adjs,nouns),]
adj_dtm           <- dtm.create(documents = adj_tokens$aid, terms=adj_tokens$lemma )
adj_termstats     <- term.statistics(adj_dtm)
adj_filter        <- adj_termstats[adj_termstats$docfreq >- 5 & adj_termstats$number==F, ]
adj_filtered      <- adj_dtm[,adj_filter$term]
both_model         <- lda.fit(adj_filtered , K = 50, num.iterations = 1000)
meta2             <- meta[match(both_model@documents,meta$id),]
assignment        <- data.frame(i=both_model@wordassignments$i, j=both_model@wordassignments$j, v=both_model@wordassignments$v)
docsums           <- acast(assignment, i ~ v, value.var='j', fun.aggregate=length) 
colnames(docsums) <- paste("Topic_",1:50, sep="")
full_meta_both   <- cbind(meta2,docsums)

## Compare their predictive validity

adj_res <- mclapply(unique(full_meta$division), label_test,label_column="division", dataset=full_meta)
adj_matrix  <- cbind(sapply(adj_res,getElement, "best_coefs"))
adj_bics    <- c(sapply(adj_res,getElement,"best_bic"))
colnames(adj_matrix) <- unique(full_meta$division)

## Plot the results
library(ggplot2)
m_adj <- melt(adj_matrix)
m_adj$model <- "adj"
m_noun <- melt(noun_matrix)
m_noun$model <- "noun"
m_both <- melt(both_matrix)
m_both$model <- "both"
all <- rbind(m_adj,m_noun,m_both)
ggplot(all, aes(Var1,Var2,fill=ifelse(value>0,value,0)))+geom_tile()+scale_fill_gradient(high = "steelblue", low="white") + theme(axis.text.x=element_text(angle=90))+facet_grid(~model)
