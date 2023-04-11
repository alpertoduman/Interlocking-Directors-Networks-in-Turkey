library(tidyverse)
library(rvest)
library(igraph)

i <-seq(1:4129)
paths <- c()
for (j in i){
  path <- paste0("//*[@id='printAreaDiv']/a[", j, "]/div[2]")
  paths <- append(paths, path)
}
head(paths)

url <- "https://www.kap.org.tr/en/tumKalemler/kpy41_acc6_yonetim_kurulu_uyeleri"
table_page <- read_html(url)

directors <- c()
for (p in paths){
  director <- table_page %>% html_elements(xpath = p) %>% html_text()
  directors <- append(directors, director)
}

directors <- directors %>% trimws()
head(directors)

paths2 <- c()
for (j in i){
  path2 <- paste0("//*[@id='printAreaDiv']/a[", j, "]/div[1]")
  paths2 <- append(paths2, path2)
}
head(paths2)


firms <- c()
for (d in paths2){
  firm <- table_page %>% html_elements(xpath = d) %>% html_text()
  firms <- append(firms, firm)
}

firms <- firms %>% trimws() 
head(firms)

bistBD <- tibble(firms, directors)
bistBD

firms %>% trimws() %>% head()

### Data cleaning
sum(str_count(bistBD$directors, "A.Ş."))
sum(str_count(bistBD$directors, "------"))
sum(str_count(bistBD$directors, "ANONİM"))
sum(str_count(bistBD$directors, "ŞİRKETİ"))

bistBD <- bistBD[-1,]

BD <- bistBD
FR <- BDfirmRep

as <- which(str_detect(BD$directors, "A.Ş"))
BD$directors[as] <- FR$BD[as]

blank <- which(str_detect(BD$directors, "------"))
BD$directors[blank] <- FR$BD[blank]

blank2 <- which(str_detect(BD$directors, "-"))
BD$directors[blank2] <- FR$BD[blank2]

anonim <- which(str_detect(BD$directors, "ANONİM"))
BD$directors[anonim] <- FR$BD[anonim]

anonim2 <- which(str_detect(BD$directors, "ANONIM"))
BD$directors[anonim2] <- FR$BD[anonim2]

holding <- which(str_detect(BD$directors, "HOLDING"))
BD$directors[holding] <- FR$BD[holding]

sirketi <- which(str_detect(BD$directors, "ŞİRKETİ"))
BD$directors[sirketi] <- FR$BD[sirketi]

ticaret <- which(str_detect(BD$directors, "TİCARET"))
BD$directors[ticaret] <- FR$BD[ticaret]

hazine <- which(str_detect(BD$directors, "HAZİNE"))
BD$directors[hazine] <- FR$BD[hazine]

vakf <- which(str_detect(BD$directors, "VAKFI"))
BD$directors[vakf] <- FR$BD[vakf]

borusan <- which(str_detect(BD$directors, "BORUSAN"))
BD$directors[borusan] <- FR$BD[borusan]

BD <- BD[-which(BD$directors=="------"),]
BD <- BD[-which(BD$directors=="-"),]

BDupper <- BD
BDupper$directors <- str_to_upper(BD$directors)

BDupper <- drop_na(BDupper)




#### IGRAPH

gBD <- graph.data.frame(BDupper, directed= F )

V(gBD)$type <- V(gBD)$name %in% get.edgelist(gBD)[,1]

table(V(gBD)$type)
length(unique(BDupper$firms))
head(V(gBD)$name)


is.bipartite(gBD)

bproj <- bipartite.projection(gBD, multiplicity = T)

Director <- bproj$proj1

Firm <- bproj$proj2



comps_Firm <- components(Firm)
table(comps_Firm$csize)
# 1     2   3   4   5   6   7   8     9   231 
# 215  42  11   3   2   3   1   1     2   1 

FGC <- which.max(comps_Firm$csize)

plot(Firm, vertex.label=NA, vertex.size=3, vertex.color="red")

Fdegree <- degree(Firm)
V(Firm)$name[which.max(Fdegree)]
# YAPI VE KREDİ BANKASI A.Ş."
head(sort(Fdegree, decreasing=T))

Ddegree <- degree(Director)
V(Director)$name[which.max(Ddegree)]
# LEVENT ÇAKIROĞLU 75
head(sort(Ddegree, decreasing = T))
                  

# plot(Firm,vertex.size = 4, vertex.label=NA, mark.groups = list(comps_Firm$membership == FGC), mark.col = "blue")

### FGC relative share 
#  231/636 = %36.3

### Firm  Giant Component

FGcomps <- decompose.graph(Firm)
FGCno <- which.max(sapply(FGcomps, vcount))
FGC <- FGcomps[[FGCno]]

summary(FGC) 


plot(FGC, vertex.label=NA, vertex.size=3, vertex.color="blue")
plot(DGC, vertex.label=NA, vertex.size=3, vertex.color="blue", layout= layout.fruchterman.reingold)

aplFGC <- average.path.length(FGC)
# 6.73

transFGC <- transitivity(FGC)
# 0.567

d <- edge_density(FGC)

# centralities

### Centrality of Firms

ecFGC <- evcent(FGC)$vector
V(FGC)$name[which.max(ecFGC)]
# "TÜPRAŞ-TÜRKİYE PETROL RAFİNERİLERİ A.Ş."

betFGC <- betweenness(FGC)
V(FGC)$name[which.max(betFGC)]
# "DOĞAN ŞİRKETLER GRUBU HOLDİNG A.Ş."



### Directors Giant Component

DirectorGcomps <- decompose.graph(Director)
DGCno <- which.max(sapply(DirectorGcomps, vcount))
DGC <- DirectorGcomps[[DGCno]]

summary(DGC)


## centrality of directors

ecDGC <- evcent(DGC)$vector
V(DGC)$name[which.max(ecDGC)]
# "TUNCAY ÖZİLHAN"

betDGC <- betweenness(DGC)
V(DGC)$name[which.max(betDGC)]
# "AGAH UĞUR"


