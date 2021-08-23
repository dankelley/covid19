library("readxl")
f <- "Classroom HVAC Master Sheet.xlsx"
d <- read_excel(f, sheet="Studley", skip=1)
names(d)
ACH <- d[["Air Changes per Hr (ACH)"]]
pdf("classroom_ACH.pdf", height=4)
par(mar=c(3,3,3,1), mgp=c(2,0.7,0)) # tighten margins
hist(ACH, breaks=40, main="Air changes per hour (red=LSC3655, 7.1/h)")
cla <- 7.1                             # ACH for LSC-3655
avg <- mean(ACH, na.rm=TRUE)
med <- median(ACH, na.rm=TRUE)
abline(v=c(cla, avg, med), col=c(1, 2, 4), lwd=3)
mtext("black: LSC-3655, blue: median, red: mean")
dev.off()
focus <- is.finite(ACH) & ACH < 2
df <- d[focus,]
names(df)
df[, c(1,2,8)]
