library(MVA); library(HSAUR2) ; library(ggplot2) 

data(heptathlon)
View(heptathlon)

heptathlon$hurdles = with(heptathlon, max(hurdles) - hurdles)
heptathlon$run200m = with(heptathlon, max(run200m) - run200m)
heptathlon$run800m = with(heptathlon, max(run800m) - run800m)

score = which(colnames(heptathlon) == 'score')
round(cor(heptathlon[, -score]), 2)
plot(heptathlon[, -score])
plot(heptathlon[, -score], pch='.', cex=1.5)

heptathlon = heptathlon[-grep('PNG', rownames(heptathlon)),]
score = which(colnames(heptathlon) == 'score')
round(cor(heptathlon[, -score]), 2)

# prcomp()
hept_pca = prcomp(heptathlon[, -score], scale=TRUE)
print(hept_pca)
plot(heptathlon[, -score], pch='.', cex=1.5)

summary(hept_pca)

# princomp()
hept_pca_prin = princomp(heptathlon[, -score], cor=TRUE)
print(hept_pca_prin)
plot(heptathlon[, -score], pca='.', cex=1.5)

summary(hept_pca_prin)

a1 = hept_pca$rotation[, 1]
a1

a2 = hept_pca$rotation[, 2]
a2

a3 = hept_pca$rotation[, 3]
a3

hept_pca_prin$loadings
hept_pca_prin$scores


screeplot(hept_pca, type='lines', main='scree for pca_pr')
screeplot(hept_pca_prin, type='lines', main='scree for pca_prin')


biplot(hept_pca, colour = c('blue', 'black'))

temp = heptathlon[, -score]
rownames(temp) = gsub(" \\(.*", "", rownames(temp))
biplot(prcomp(temp, scale=TRUE), colour=c('black', 'blue'), xlim=c(-0.5, 0.7), cex=0.7)

biplot(princomp(temp, cor=TRUE), colour=c('black', 'blue'), xlim= c(-0.5, 0.7), cex=0.7)

df = read.csv("C:/Users/dohyeong/Downloads/csv.csv")
df

colnames(df)
df$X100m = with(df, max(X100m) - X100m)
df$X400m = with(df, max(X400m) - X400m)
df$X110m.hurdle = with(df, max(X110m.hurdle) - X110m.hurdle)
df$X1500m = with(df, max(X1500m) - X1500m)


row.names(df) = df$Athlets
df = subset(df, select = -Competition)
df = subset(df, select = -Athlets)
df

Points = which(colnames(df) == 'Points')
round(cor(df[, -Points]), 2)
plot(df[, -Points])
plot(df[, -Points], pch='.', cex=1.5)

Points = which(colnames(df) == 'Points')
round(cor(df[, -Points]), 2)

df_pca = prcomp(df[, -Points], scale=TRUE)
print(df_pca)
plot(df[, -Points], pch='.', cex=1.5)

summary(df_pca)

screeplot(df_pca, type='lines', main='scree for pca_pr')

biplot(df_pca, colour = c('blue', 'black'))

temp = df[, -Points]
rownames(temp) = gsub(" \\(.*", "", rownames(temp))
biplot(prcomp(temp, scale=TRUE), colour=c('black', 'blue'), xlim=c(-0.5, 0.7), cex=0.7)

val = cbind(rownames(df), df$Points)
val
