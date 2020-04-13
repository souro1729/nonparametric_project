#####
print("Run this Code....")



Dirname=getwd()
Dirname
subDir <- "Group-8"
file=file.path(Dirname,subDir)
dir.create(file, showWarnings = FALSE)
setwd(file.path(Dirname, subDir))


download.file("https://github.com/souro1729/nonparametric_project/souro.zip",file.path(Dirname, subDir))

dir.create("New")
unzip()





