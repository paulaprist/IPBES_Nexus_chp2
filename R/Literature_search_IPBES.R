#install.packages("remotes")
library(remotes)

#Install litsearchr 
#install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)
library(igraph)
library(dplyr)
library(ggplot2)
library(here)
library(ggraph)

#------set directory
search_directory <- here("data")

#----Naive Search
#----import txt file from our scopus search
naiveimport <-
  litsearchr::import_results(directory = search_directory, verbose = TRUE)

#-----remove duplicates
naiveresults <-
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")
names(naiveresults)

#-----extract the keywords from title, abstract and keywords
rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",min_freq = 2,
    ngrams = TRUE,min_n = 2,
    language = "English")

taggedkeywords <-
  litsearchr::extract_terms(keywords = naiveresults$keywords,
                            method = "tagged",min_freq = 2,
                            ngrams = TRUE,min_n = 2,
                            language = "English")

#----build the keyword co-occurrence network
all_keywords <- unique(append(taggedkeywords,rakedkeywords))##keywords identified without duplicates
docs <- paste(naiveresults[, "title"], naiveresults[, "abstract"])#joining title and abstract
dfm <- create_dfm(elements=docs, features=all_keywords)#creating a matrix that records which terms appear in which articles
dfm[1:3,1:4]##checking
g <- create_network(dfm, min_studies=20)
head(g)

#-------network graph
memory.limit(size=30000)
ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="blue") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE)+
  guides(edge_alpha=FALSE)

#-----ranking our search terms by importance
strengths <- strength(g)
st_kw <- data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) -> term_strengths #arrange the terms in ascending order of strength
head(term_strengths)##weak terms
tail(term_strengths)##strong terms
write.csv(st_kw,"strength_keywords.csv")

#----creating a graph to make a cut
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=dplyr::filter(term_strengths, rank>20), hjust="right", nudge_y=30, check_overlap=TRUE)
cutoff_fig

#----retaining 80% of the search terms
cutoff_cum <- litsearchr::find_cutoff(g, method="cumulative", percent=0.8)
cutoff_cum
cutoff_fig +geom_hline(yintercept=cutoff_cum, linetype="dashed")

#-----takes only the remaining terms from the reduced network
cut <- litsearchr::get_keywords(litsearchr::reduce_graph(g, cutoff_cum))
head(cut)
length(cut)
cut

#---adding extra terms that were not selected
extra_terms <- c("biodiversity","zoonotic","mental Health","nutrition")

#----final_terms to be used
final_terms <- c(cut, extra_terms)
final_terms

#----grouping the terms (we removed some non sense words)
grouped_terms <-list(biodiversity=final_terms[c(6,7,21,23,39,42,54)],
                     health=final_terms[c(16,17,55,56)],
                     water=final_terms[c(25,26,27,28,29,51,52,53)],
                     food=final_terms[c(5,10,11,30,45,58)],
                     climate=final_terms[c(1,2,3,4,13)],
                     drivers=final_terms[c(8,9,15,18,19,20,38,41,44)])

grouped_terms

###doing a new search
litsearchr::write_search(grouped_terms,
                         languages="English",
                         exactphrase=TRUE,
                         stemming=FALSE,
                         closure="left",
                         writesearch=T,
                         directory = here("output/"))


#----do not run----###
#---As we end up with many keywords lets try another method
#----identifying places where the ascending line jumps up'
cutoff_change <- litsearchr::find_cutoff(g, method="changepoint", knot_num=3)
cutoff_change
cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

#----choosing the 1th cut (to keep the largest amount of words) 
g_redux <- litsearchr::reduce_graph(g, cutoff_change[1])
selected_terms <- litsearchr::get_keywords(g_redux)
selected_terms
length(selected_terms)

#---adding extra terms that were not selected
extra_terms <- c("biodiversity","zoonotic","mental Health","human wll-being")

#----final_terms to be used
final_terms <- c(selected_terms, extra_terms)
final_terms

#----grouping the terms (we removed some non sense words)
grouped_terms <-list(biodiversity=final_terms[c(7,8,21,22,23,40,45)],
                     health=final_terms[c(16,46,47,48)],
                     water=final_terms[c(28,29,30,31,32,33,34)],
                     food=final_terms[c(1,10,11,25,41)],
                     climate=final_terms[c(2,3,4,12,13,14,38)],
                     drivers=final_terms[c(9,15,17,18,19,20,24,36)])
               
grouped_terms

###doing a new search
litsearchr::write_search(grouped_terms,
                         languages="English",
                         exactphrase=TRUE,
                         stemming=FALSE,
                         closure="left",
                         writesearch=T,
                         directory = here("output/"))



 

