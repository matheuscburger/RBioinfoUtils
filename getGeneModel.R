library(GenomicRanges)
library(ggbio)
library(rtracklayer)
unfactor <- function(x){
  levels(x)[x]
}


getwd()
setwd("~")
gff <- read.table("ensGene.gff", sep = "\t", stringsAsFactors=F)

head(gff)

colnames(gff) <- c("seqname", "source", "feature", "start", "end", "score", "strand", "frame", "attributes")


## Obtem identificadores     
aux <- matrix(unlist(strsplit(gff$attributes, ";")), byrow=T, ncol=2)
aux <- gsub('gene_id|transcript_id| ', "",  aux)
aux <- cbind( aux, matrix(unlist(strsplit(aux[,1], "\\|")), byrow=T, ncol=2))
aux <- aux[,2:4]
colnames(aux) <- c('transcript_id', 'gene', 'gene_id')
head(aux)
gff <- cbind(gff, aux)
head(gff)

## Cria objeto GRanges
gff.gr <- with(gff, GRanges( seqnames= seqname, 
                   ranges=IRanges(start=start, end= end), 
                   strand=strand, source=source, feature=feature, 
                   score=score, frame=frame, transcript_id=as.character(transcript_id), 
                   gene= as.character(gene), gene_id=as.character(gene_id)))


## Divide objeto por genes
gff.gr.spt <- split(gff.gr, as.factor(mcols(gff.gr)$gene_id))


## Reduz objeto
#The reduce will align the ranges and merge overlapping ranges to produce a simplified set.
gff.mrg <- reduce(gff.gr.spt)

gff.mrg

autoplot(gff.mrg[['ENSG00000000003']])

autoplot(gff.gr.spt[['ENSG00000000003']])

export(gff.mrg, "geneModel.bed", format="bed")

gff.mrg.unl <- unlist(gff.mrg)

mcols(gff.mrg.unl)$geneId <- names(gff.mrg.unl)
names(gff.mrg.unl) <- 1:length(gff.mrg.unl)

g2gid <- unique(with(gff, data.frame(gene, gene_id)))
rownames(g2gid) <- g2gid$gene_id
mcols(gff.mrg.unl)$gene <- g2gid[mcols(gff.mrg.unl)$geneId, "gene"]

gene.model <- as.data.frame(gff.mrg.unl)

gene.model$name <- with(gene.model, paste(gene,geneId, sep=":"))
gene.model$score <- 1000

write.table(gene.model[,c("seqnames", 'start', 'end', 'name', 'score', 'strand')], file="geneModel.bed", sep="\t", quote=F, row.names=F)