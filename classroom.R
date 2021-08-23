library("readxl")
f <- "Classroom HVAC Master Sheet.xlsx"
d <- read_excel(f, sheet="Studley", skip=1)
names(d)
ACH <- d[["Air Changes per Hr (ACH)"]]
if (!interactive())
    pdf("classroom_ACH.pdf", height=4)
par(mar=c(3,3,3,1), mgp=c(2,0.7,0)) # tighten margins
hist(ACH, breaks=40, main="Classroom Air Changes per Hour (Studley campus)")
cla <- 7.1                             # ACH for LSC-3655
med <- median(ACH, na.rm=TRUE)
mea <- mean(ACH, na.rm=TRUE)
abline(v=c(cla, med, mea), col=c(1,4,2), lwd=3)
legend("right", lwd=3, col=c(1,4,2),
    legend=c("LSC-3655", "Median", "Mean"))
if (!interactive())
    dev.off()
focus <- is.finite(ACH) & ACH < 2
df <- d[focus,]
names(df)
df[, c(1,2,8)]
message(sprintf("3655: %.2f, median: %.2f, mean: %.2f",
        cla, med, mea))
