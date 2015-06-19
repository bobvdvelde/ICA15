######### MODEL VALIDATOR SCRIPT ###########
#
#
# Runs several LDA models & compares outcome
# and compare their predictive validity on 
# meta-data labels 
#
############################################

library(corpustools)
library("MASS")
library("car")
library("parallel")
library(ggplot2)
source("mclapply.hack.R")
source("adjacency_functions.R")
source("backbone.r")
## Load files

tokens <- read.csv("ica_tokens.csv")
meta   <- read.csv("ica_meta.csv")

## subset corpus

adjs  <- c("JJ","JJS","JJR")
nouns <- c("NN","NNP","NNPS","NN")

## Run Topic models
models <- list()

### adj based
make_full_meta <- function(tokens,meta, alpha)
  {
  adj_tokens        <- tokens
  adj_dtm           <- dtm.create(documents = adj_tokens$aid, terms=adj_tokens$lemma )
  adj_termstats     <- term.statistics(adj_dtm)
  adj_filter        <- adj_termstats[adj_termstats$docfreq >- 5 & adj_termstats$number==F, ]
  adj_filtered      <- adj_dtm[,adj_filter$term]
  adj_model         <- lda.fit(adj_filtered , K = 50, num.iterations = 1000, alpha=alpha)
  meta2             <- meta[match(adj_model@documents,meta$id),]
  assignment        <- data.frame(i=adj_model@wordassignments$i, j=adj_model@wordassignments$j, v=adj_model@wordassignments$v)
  docsums           <- acast(assignment, i ~ v, value.var='j', fun.aggregate=length) 
  colnames(docsums) <- paste("Topic_",1:50, sep="")
  full_meta         <- cbind(meta2,docsums)
  list(meta=full_meta, model=adj_model)
}

label_test <- function(test_label, dataset, label_column)
{
  d <- dataset
  d$div <-as.numeric(d[[label_column]]==test_label)
  mod   <- glm.nb( paste("div~" ,paste("Topic_",1:50,collapse="+",sep="")),d)
  umod  <- step(mod)
  
  norm_coefs <- coef(mod)
  norm_coefs[summary(mod)$coef[,4]>0.05] <- 0
  best_coefs <- norm_coefs
  best_coefs[!rownames(best_coefs) %in% names(coef(umod)) | summary(umod)$coef[,4]>.05] <- 0
  
  results <- list(
    best_coefs = best_coefs,
    best_bic   = BIC(umod),
    norm_coefs = norm_coefs,
    norm_bic   = BIC(mod)
    )
  
}

analyse_labels <- function(dataset, label_column)
{
  res <- mclapply(unique(dataset[[label_column]]), label_test, label_column=label_column, dataset=dataset)
  norm_mat = cbind(sapply(res,getElement,"norm_coefs"))
  colnames(norm_mat) <- unique(dataset[[label_column]])
  best_mat = cbind(sapply(res,getElement,"best_coefs"))
  colnames(best_mat) <- unique(dataset[[label_column]])
  bics <- cbind(sapply(res, getElement, "norm_bic"))
  bics <- cbind(bics, cbind(sapply(res, getElement, "best_bic")) )
  rownames(bics) <- unique(dataset[[label_column]])
  colnames(bics) <- c("normal","updated")
  list(best=best_mat,norm=norm_mat, bics = bics)
  
}

get_bics <- function(analyse_labels_result)
{
  m <- cbind(sapply(analyse_labels_result,"best_bic"))
  
}

plot_matrix <- function(dataset,label,type="pos")
{
  if (type=="pos"){
    ggplot(melt(dataset[[label]]),aes(Var1,Var2,fill=ifelse(value>0,value,0)))+
    geom_tile(alpha=.7)+
    scale_fill_gradient(high="steelblue",low="transparent")+
    scale_x_discrete(labels = c("int",paste(1:50)))
  } else
  {
    ggplot(melt(dataset[[label]]),aes(Var1,Var2,fill=ifelse(value<0,value,0)))+
      geom_tile(alpha=.7)+
      scale_fill_gradient(high="transparent",low="red")+
      scale_x_discrete(labels = c("int",paste(1:50)))
  }
}

by_topic <- function(tokens, meta, model, topic_nr)
{
  # subset meta ids
  mids <- meta$id[meta[[paste("Topic_",topic_nr,sep="")]]>0]
  topic_tokens <- tokens[tokens$aid %in% mids & tokens$lemma %in% terms(model,100)[,topic_nr],]
  
  # create dtm for topic
  topic_dtm   <- dtm.create(documents = topic_tokens$aid, terms=topic_tokens$lemma )
  
  # create graph
  topic_graph <- cooccurenceNetwork(topic_dtm, measure = "conprob")
  
  # apply backbone filtering
  bb <- backbone(topic_graph)
  topic_graph <- delete.edges(topic_graph,c(1:length(bb))[bb>0.025])
  topic_graph <- delete.vertices(topic_graph, c(1:length(V(topic_graph)))[degree(topic_graph)<1])
  
  # plot network
  igraph::plot.igraph(topic_graph, vertex.size=1+degree(topic_graph, mode="in"), vertex.label.cex=.7*log(degree(topic_graph)),
                      vertex.label.color="Black", vertex.color="steelblue", vertex.frame.color=NA,
                      edge.arrow.size=.5*E(topic_graph)$weight,
                      layout=layout.fruchterman.reingold(topic_graph, weights=E(topic_graph)$weight/4)
                      )
  
}

# make topic models at three alpha levels
fm_05 <- make_full_meta(tokens[tokens$pos %in% nouns, ], meta, alpha=.5)
fm_10 <- make_full_meta(tokens[tokens$pos %in% nouns, ], meta, alpha=1)
fm_15 <- make_full_meta(tokens[tokens$pos %in% nouns, ], meta, alpha=1.5)

# analyse 
fma_05 <- analyse_labels(fm_05$meta, "division")
fma_10 <- analyse_labels(fm_10$meta, "division")
fma_15 <- analyse_labels(fm_15$meta, "division")

best_bics <- data.frame(k0.5=fma_05$bics[,2],k1.0=fma_10$bics[,2],k1.5=fma_15$bics[,2])
crossload_count <- function(z) apply(z,1,function(x){sum(x>0)})
best_crossloads <- data.frame(k.05=crossload_count(fma_05$best),k.10=crossload_count(fma_10$best),k.15=crossload_count(fma_15$best))
fmelter <- function(x,y){ m<- melt(x$best); m$model <- y; return(m)}
melted_models <- rbind(fmelter(fma_05, "K=0.5"),fmelter(fma_10, "K=1.0"),fmelter(fma_15, "K=1.5"))

# plot 
## BIC boxplots
ggplot(melt(best_bics),aes(x=1,y=value))+geom_boxplot(fill="steelblue",alpha=.8)+facet_grid(~variable)+xlab("Models")+ggtitle("Model BIC boxplot")+ylab("BIC")
## BIC whiskerplot
ggplot(melt(best_bics),aes(x=1,y=value,fill=variable, colour=variable))+stat_summary(fun.y="mean",geom="point")+stat_summary(fun.data=mean_cl_normal,geom="errorbar",alpha=.7,size=.5)+facet_grid(~variable)
## Term overlap histograms
ggplot(melt(best_crossloads),aes(x=value,fill=variable))+geom_histogram(alpha=.5,position="dodge",binwidth=1)+xlab("number of overlapping topics")+ylab("")+theme(legend.text=element_text(size = 20))
## Full overview
ggplot(melted_models,aes(x=Var1,y=Var2,fill=ifelse(value>0,value,0),alpha=.8))+geom_tile()+scale_x_discrete(labels = c("int",paste(1:50)))+scale_fill_gradient(high="steelblue",low="white")+facet_grid(~model)+theme(legend.position="none")

plot_matrix(fma_10,"best")

# graph 
by_topic(tokens, fm_10$meta, fm_10$model, 17) # unique
by_topic(tokens, fm_10$meta, fm_10$model, 1) # shared

cprob <- function(dataset, x, y){dataset[[x]]*dataset[[y]]/sum(dataset[[x]])+sum(dataset[[y]])}