# Structural covariance support, independent of the fitted variance magnitudes.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R')
out<-'3. Output/paper_A_final_revision';local<-file.path(out,'local')
o<-geto('B');z<-prepare_model(o$long,o$t)
m<-readRDS(file.path(local,'selected_B.rds'))$model
Z<-as.matrix(z[,paste0(attrs,'_c')]);groups<-split(seq_len(nrow(z)),z$id)
designs<-unique(vapply(groups,function(ix)paste(apply(Z[ix,,drop=FALSE],1,paste,collapse=','),collapse=';'),character(1)))
groups<-groups[match(designs,vapply(groups,function(ix)paste(apply(Z[ix,,drop=FALSE],1,paste,collapse=','),collapse=';'),character(1)))]
pairs<-which(upper.tri(matrix(0,6,6),diag=TRUE),arr.ind=TRUE)
checks<-lapply(c('full','domains','diagonal'),function(kind){
 p<-pairs;if(kind=='diagonal')p<-p[p[,1]==p[,2],,drop=FALSE]
 if(kind=='domains')p<-p[p[,1]==p[,2]|(p[,1]==1&p[,2]==2)|(p[,1]==3&p[,2]==4)|(p[,1]==5&p[,2]==6),,drop=FALSE]
 A<-do.call(rbind,lapply(groups,function(ix){Q<-Z[ix,,drop=FALSE];sel<-lower.tri(matrix(0,length(ix),length(ix)),diag=TRUE);a<-sapply(seq_len(nrow(p)),function(k){u<-p[k,1];v<-p[k,2];S<-outer(Q[,u],Q[,v]);if(u!=v)S<-S+t(S);S[sel]});cbind(a,diag(length(ix))[sel])}))
 norms<-sqrt(colSums(A*A));sv<-svd(sweep(A,2,norms,'/'))$d
 data.frame(model=kind,columns=ncol(A),rank=qr(A)$rank,scaled_condition=max(sv)/min(sv),minimum_scaled_singular=min(sv))
})
csv(bind_rows(checks),'covariance_identification');stopifnot(all(vapply(checks,function(d)d$rank==d$columns,logical(1))))
centers<-data.frame(attribute=attrs,center=sapply(z[,attrs],mean));csv(centers,'centering')
csv(data.frame(rank=qr(getME(m,'X'))$rank,columns=ncol(getME(m,'X')),condition=kappa(getME(m,'X'))),'fixed_identification')
dg<-as.matrix(VarCorr(m)[[1]]) # explicit sum audit must include all six blocks
v1<-sum(vapply(VarCorr(m),function(S){S<-as.matrix(S);Q<-as.matrix(z[,colnames(S),drop=FALSE]);mean(rowSums((Q%*%S)*Q))},numeric(1)))
v2<-sum(vapply(VarCorr(m),function(S)as.numeric(S)*mean(z[[colnames(S)]]^2),numeric(1)))
stopifnot(abs(v1-v2)<1e-12,length(VarCorr(m))==6)
stopifnot(!anyDuplicated(o$long[,c('id','card')]),max(o$t$n_cards)<=8)
hh<-hist(100*o$t$hb,breaks=seq(-106.25,106.25,12.5),plot=FALSE)
csv(data.frame(left=head(hh$breaks,-1),right=tail(hh$breaks,-1),count=hh$counts,share=100*hh$counts/sum(hh$counts)),'harshness_histogram','B')
writeLines(c('Selected diagonal: sole stable interior candidate under recorded selection rule.',
 'Full and domain covariance fits agree across optimizers but lie at or near the boundary.',
 'Their better REML AIC is disclosed; diagonal zero correlations are a working restriction, not an empirical claim.',
 'All covariance bases including residual are full rank; all six slope blocks are included in the allocation.',
 'Centering is recomputed over each scenario complete-case decisions and in each bootstrap replicate, matching the full estimator.',
 'The primary bootstrap uses 199 draws for standard-error estimation, not percentile confidence limits.'),file.path(out,'model_selection_decision.txt'))
