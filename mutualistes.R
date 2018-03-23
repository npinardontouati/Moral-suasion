################# Packages ####################

# install.packages("rJava", "http://rforge.net/", type="source")
# library(rJava)
# .jinit()
# .jcall("java/lang/System", "S", "getProperty", "java.runtime.version") ## Java has to be version 1.8
# install.packages("devtools")
# library(devtools)
# devtools::install_github("ropensci/tabulizer")
# devtools::install_github("ropensci/tabulizerjars")
#install.packages("stringr")
#install.packages("shiny")
#install.packages("miniUI")
#install.packages("taRifx")
library(readstata13)
library(foreign)
library(shiny)
library(miniUI)
library(stringr)
library(dplyr)
library(tabulizerjars)
library(tabulizer)
library(data.table)
library(taRifx)

############## Setup #############
rm(list=ls())


folder <- "/Users/npinardontouati/Documents/Data_boards_mutualistes/01_Data/CA"
files <- list.files(folder,recursive = TRUE)
print(files)

# Set up metadata matrix: i, path, flag_extract_text, flag_extract_table_1, flag_extract_table_2, checktable_1, checktable_2
metadata <- matrix(data = NA,nrow = length(files),ncol=8)
pagenbs <- matrix(data = NA,nrow = length(files),ncol=2)

# Column names
raw <- data.table(i=c(0),file=c(""))
namesCA <- subset(raw,i>0)
others <- subset(raw,i>0)
options(warn=2)

######## Recording page numbers #########
for (i in 1:length(files)) {
  location <- paste0(folder,"/",files[i])
  metadata[i,1] <- i
  metadata[i,2] <- location
  
  # Extract all the text
  out <- tryCatch({extract_text(location)}, error = function(e) e)
  
  # Record errors in the vector "flag"
  metadata[i,3] <- any(class(out) == "error")
  
  # if the file can be read
  if (metadata[i,3]==FALSE) {
    #### Record the number of the pages of interest ####
    string <- str_extract(out,"COMPOSITION.(.|\n)*?CONFLIT")
    pagenb1<-str_extract_all(string,"[1-9][0-9][[:space:]]")
    pagenb1<-pagenb1[[1]]
    pagenbs[i,1] <- as.numeric(pagenb1[[length(pagenb1)]])
    string <- str_extract(out,"CONFLIT.(.|\n)*?INFORMATION")
    pagenb2<-str_extract_all(string,"[1-9][0-9][[:space:]]")
    pagenb2<-pagenb2[[1]]
    pagenbs[i,2]<-as.numeric(pagenb2[[1]])
  }
  metadata[i,4] <- metadata[i,3]
  if (is.na(pagenbs[i,1]) & is.na(pagenbs[i,2])) {
    metadata[i,4] <- TRUE
  }
}

########## Function that takes one pdf page and returns a list of tables ###########

table_onepage_extraction <-function(i,col) {
  location <- paste0(folder,"/",files[i])
  pagenb <-  pagenbs[i,col]
  
  ### Extract all tables in the PDF page
  table <- tryCatch({extract_tables(location, page = pagenb,columns=list(c(70, 180, 326, 425,524)))}, error = function(e) e)
  flag <- any(class(table) == "error")
  flag_length <- (length(table)>0)                  
                    
  ### Create an emply list of datatables to store the tables of the page
  tables <- vector(mode = "list", length = 4)
  tables[[1]] <- NA
  tables[[2]] <- NA
  if (flag==FALSE) {
    if (length(table)>0L) {
      ### Create data.tables that will store board tables and other tables
      rawnames <- subset(raw,i>0)
      rawothers <- subset(raw,i>0)
      for (k in 1:length(table)){
        table_k <- table[[k]]
        table_k <- as.data.table(table_k)
        nbcols <-length(colnames(table_k))
        newnames <- paste0("v",c(1:nbcols))
        setnames(table_k,newnames)
        table_k <- table_k[,"file":=location]
        table_k <- table_k[,"i":=i]
        checktable <- grep("Administrateur|ADMINISTRATEUR|administrateur",as.matrix(table_k))
        if (length(checktable)>0L) {
          l=list(rawnames,table_k)
          rawnames <- rbindlist(l,use.names=TRUE,fill=TRUE,idcol=NULL)
        }
        if (length(checktable)==0L) {
          l=list(rawothers,table_k)
          rawothers <- rbindlist(l,use.names=TRUE,fill=TRUE,idcol=NULL)
        }
      }
      tables[[1]] <- rawnames
      tables[[2]] <- rawothers
      tables[[3]] <- flag
      tables[[4]] <- flag_length
    }
  }
  return(tables)
}

#### For each PDF i, get the appropriate tables #####
                    
for (i in 1:length(files)) {
  location <- paste0(folder,"/",files[i])
  pagenb1 <- pagenbs[i,1]
  pagenb2 <- pagenbs[i,2]
  print(i)
  if (metadata[i,4]==FALSE) {
    if (pagenb1==pagenb2) {
      tables<-table_onepage_extraction(i,1)
      names_add <- tables[[1]]
      metadata[i,5]<-tables[[3]]
      metadata[i,7]<-tables[[4]]
      if (is.data.table(names_add)) {
        l<-list(namesCA,names_add)
        namesCA <- rbindlist(l,use.names=TRUE,fill=TRUE,idcol=NULL)
      }
      others_add <- tables[[2]]
      if (is.data.table(others_add)) {
        l<-list(others,others_add)
        others <- rbindlist(l,use.names=TRUE,fill=TRUE,idcol=NULL)
      }
    }
    if (pagenb1!=pagenb2) {
      for (col in 1:2) {
        tables<-table_onepage_extraction(i,col)
        names_add <- tables[[1]]
        metadata[i,4+col]<-tables[[3]]
        metadata[i,6+col]<-tables[[4]]
        if (is.data.table(names_add)) {
          l<-list(namesCA,names_add)
          namesCA <- rbindlist(l,use.names=TRUE,fill=TRUE,idcol=NULL)
        }
        others_add <- tables[[2]]
        if (is.data.table(others_add)) {
          l<-list(others,others_add)
          others <- rbindlist(l,use.names=TRUE,fill=TRUE,idcol=NULL)
        }
      }
    }
  }
}

##### Deal with the cases where FLAG=TRUE ###############
metadata[metadata[,4]==TRUE,] ## no observations
metadata[metadata[,7]==0,]

####### Clean the names table ############

errors <- subset(others,others[, .I>=113 & .I<=122 | .I>=209 & .I<=229 ])
l<-list(namesCA,errors)
namesCA <- rbindlist(l,use.names=TRUE,fill=TRUE,idcol=NULL)
rm(errors,others,others_add)

namesCA <- namesCA[,c("v6","v7"):=NULL] 

test <- subset(namesCA,!is.na(v5)) ## No names
test <- subset(namesCA,!is.na(v4))
test <- test[grep("[0-9]",v4),flag:=1]
test <- subset(test,is.na(flag)) ## No names
test <- subset(namesCA,!is.na(v3))
test <- test[grep("[0-9]",v3),flag:=1]
test <- subset(test,is.na(flag)) ## Some names

namesCA <- namesCA[,c("v4","v5"):=NULL] ## No names in these columns

############# Clean v3 #############
namesCA <- namesCA[grep("[0-9][0-9][0-9][0-9]",v3),flag:=1]
namesCA <- namesCA[grep("Renouvellement",v3),flag:=1]
namesCA <- namesCA[grep("AG",v3),flag:=1]
namesCA <- namesCA[v3=="de",flag:=1]
namesCA <- namesCA[flag==1,v3:=""]
namesCA <- subset(namesCA,!(v1=="" & v2==""  & v3==""))
namesCA <- subset(namesCA,! (grepl("Fonction",v1,ignore.case = TRUE) & grepl("Nom",v2,ignore.case = TRUE) & v3==""))
namesCA <- subset(namesCA,! (grepl("Fonction",v1,ignore.case = TRUE) & grepl("Nom",v2,ignore.case = TRUE) & grepl("nomination",v3,ignore.case = TRUE)))
namesCA <- subset(namesCA,! (grepl("Fonction",v2,ignore.case = TRUE) & grepl("Nom",v3,ignore.case = TRUE) & v1==""))
namesCA <- subset(namesCA,! (grepl("QUALIT",v1,ignore.case = TRUE) & grepl("NOM",v2,ignore.case = TRUE) & v3==""))
namesCA <- subset(namesCA,! (grepl("Titre",v1,ignore.case = TRUE) & grepl("Nom",v2,ignore.case = TRUE) & v3==""))
namesCA <- subset(namesCA,! (grepl("Titre",v1,ignore.case = TRUE) & grepl("Nom",v2,ignore.case = TRUE) & grepl("expiration",v3,ignore.case = TRUE)))
namesCA <- subset(namesCA,! (grepl("FONCTION",v1,ignore.case = TRUE) & grepl("Nom",v2,ignore.case = TRUE) & grepl("Mandat",v3,ignore.case = TRUE)))
namesCA <- subset(namesCA,! (grepl("FONCTION",v1,ignore.case = TRUE) & v2==""  & v3==""))
namesCA <- subset(namesCA,! (v1=="" & grepl("NOM",v2,ignore.case = TRUE) & v3==""))
namesCA <- subset(namesCA,!(v1=="" & v2==""  & v3=="ADRESSE"))
namesCA <- subset(namesCA,!(v1=="" & v3==""  & v2=="ADRESSE"))
namesCA <- subset(namesCA,! (v1=="" & v2=="" & grepl("mandat",v3,ignore.case = TRUE)))
namesCA <- subset(namesCA,!(grepl("Administrateur",v1,ignore.case = TRUE) & v2==""  & v3==""))
namesCA <- subset(namesCA,!(grepl("Administrateur",v2,ignore.case = TRUE) & v1==""  & v3==""))
namesCA <- subset(namesCA,!(v1=="Les" & v2==""  & v3==""))
namesCA <- subset(namesCA,!(v1=="LES MEMBRES DU BUREAU" & v2==""  & v3==""))
namesCA <- subset(namesCA,!(v1=="LE DIRECTEUR GENERAL" & v2==""  & v3==""))
namesCA <- subset(namesCA,!(grepl("VICE-PR",v1) & v2==""  & v3==""))
namesCA <- subset(namesCA,!(grepl("PR.*SIDENT",v1,ignore.case = TRUE) & v2==""  & v3==""))
namesCA <- subset(namesCA,!(grepl("FONCTION",v1,ignore.case = TRUE) & grepl("renouvellement",v2,ignore.case = TRUE)  ))
namesCA <- subset(namesCA,!( v2=="mandat lors de l'AG de"  & v3=="de l'exercice clos le"))
namesCA <- subset(namesCA,!(v1=="" & v2==""  & grepl("Date",v3)))
namesCA <- subset(namesCA,!(v1=="" & v2==""  & v3=="Suivante)"))
namesCA <- subset(namesCA,!(v1=="" & v2==""  & v3=="renouvellement"))
namesCA <- subset(namesCA,!(v1=="er" & v2==""  & v3==""))
namesCA <- subset(namesCA,!(v1=="ème" & v2==""  & v3==""))
namesCA <- namesCA[,N:=.N,by="i"]

############# Long and wide sets ##################

path <- "/Users/npinardontouati/Documents/Data_boards_mutualistes/03_Temp/"
wide <- subset(namesCA,N<5)
wide <- wide[v1=="",v1:=v2]
wide <- wide[v1==v2,v2:=v3]
wide <- wide[,v3:=NULL]
wide <- wide[,v2:=gsub("[[:space:]]"," ",v2)]
bdd<-paste0(path,"wide_CA.csv")
write.table(wide, paste(bdd), fileEncoding="latin1",row.names=FALSE,sep=";")


long <- subset(namesCA,N>=5)
long <- long[,v1:=gsub("[[:space:]]"," ",v1)]
long <- long[,v2:=gsub("[[:space:]]"," ",v2)]
long <- long[,v3:=gsub("[[:space:]]"," ",v3)]
long <- long[,v1:=gsub("[\r\n\t]"," ",v1)]
long <- long[,v2:=gsub("[\r\n\t]"," ",v2)]
long <- long[,v3:=gsub("[\r\n\t]"," ",v3)]
long <- long[,v1:=gsub(";"," ",v1)]
long <- long[,v2:=gsub(";"," ",v2)]
long <- long[,v3:=gsub(";"," ",v3)]
long <- long[,v1:=gsub(","," ",v1)]
long <- long[,v2:=gsub(","," ",v2)]
long <- long[,v3:=gsub(","," ",v3)]
long <- long[,v1:=gsub("É", "e",v1)]
long <- long[,v1:=gsub("Ï", "i",v1)]
long <- long[,v1:=gsub("é", "e",v1)]
long <- long[,v1:=gsub("è", "e",v1)]
long <- long[,v1:=gsub("ë", "e",v1)]
long <- long[,v1:=gsub("ç", "c",v1)]
long <- long[,v1:=gsub("ô", "o",v1)]
long <- long[,v1:=gsub("ï", "i",v1)]
long <- long[,v1:=gsub("î", "i",v1)]
long <- long[,v2:=gsub("É", "e",v2)]
long <- long[,v2:=gsub("Ï", "i",v2)]
long <- long[,v2:=gsub("é", "e",v2)]
long <- long[,v2:=gsub("è", "e",v2)]
long <- long[,v2:=gsub("ë", "e",v2)]
long <- long[,v2:=gsub("ç", "c",v2)]
long <- long[,v2:=gsub("ô", "o",v2)]
long <- long[,v2:=gsub("ï", "i",v2)]
long <- long[,v2:=gsub("î", "i",v2)]
long <- long[,v3:=gsub("É", "e",v3)]
long <- long[,v3:=gsub("Ï", "i",v3)]
long <- long[,v3:=gsub("é", "e",v3)]
long <- long[,v3:=gsub("è", "e",v3)]
long <- long[,v3:=gsub("ë", "e",v3)]
long <- long[,v3:=gsub("ç", "c",v3)]
long <- long[,v3:=gsub("ô", "o",v3)]
long <- long[,v3:=gsub("ï", "i",v3)]
long <- long[,v3:=gsub("î", "i",v3)]

long <- long[,flag2:=1]
long <- subset(long,flag2==1,c("i","v1","v2","v3"))

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
sapply(long, trim)
long <- long[v1=="",v1:="EMPTY"]
long <- long[v2=="",v2:="EMPTY"]
long <- long[v3=="",v3:="EMPTY"]

bdd<-paste0(path,"long_CA.dta")
write.dta(long,bdd)



######## OLD #############
# 
# nn = c(a=0.1, b=0.2, c=0.3, d=0.4)
# as.data.table(nn)
# as.data.table(nn, keep.rownames=TRUE)
# as.data.table(nn, keep.rownames="rownames")
# 
# 
# # 
# # 
# # if (pagenb1==pagenb2) {
# #   out1 <- extract_text(location, page = pagenb1)
# #   out1 <- str_replace(out1, "CONFLIT.(.|\n)*","")
# #   out1 <- str_replace(out1, "(.|\n)*COMPOSITION","")
# #   print(cat(out1, sep = "\n"))
# # }
# # 
# # if (pagenb1!=pagenb2) {
# #   out1 <- extract_text(location, page = pagenb1)
# #   out2 <- extract_text(location, page = pagenb2)
# #   out1 <- paste(out1,out2,sep="\n")
# #   out1 <- str_replace(out1, "CONFLIT.(.|\n)*","")
# #   out1 <- str_replace(out1, "(.|\n)*COMPOSITION","")
# #   print(cat(out1, sep = "\n"))
# # }
# # 
# # 
# # 
# # 
# # 
# # # install.packages("pdftools")
# # # library(pdftools)
# # # 
# # out1 <- pdf_text(location)
# # info <- pdf_info(location)
# # 
# # print(text)
# # print(out)
# # 
# # install.packages("ghit")
# # #library(ghit)
# # ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"),verbose = TRUE)
# 
# 
# 
# 
# 
# tryCatch(1, finally = print("Hello"))
# e <- simpleError("test error")
# ## Not run: 
# stop(e)
# tryCatch(stop(e), finally = print("Hello"))
# tryCatch(stop("fred"), finally = print("Hello"))
# 
# ## End(Not run)
# tryCatch(stop(e), error = function(e) e, finally = print("Hello"))
# tryCatch(stop("fred"),  error = function(e) e, finally = print("Hello"))
# withCallingHandlers({ warning("A"); 1+2 }, warning = function(w) {})
# 
# 
# is_simple_error <- function(x) inherits(x, "simpleWarning")
# is_try_error <- function(x) inherits(x, "try-error")
# is_simple_error(extract_tables(location, page = pagenb, method = "data.frame",columns=list(c(70, 180, 326, 425,524))))
# 
# is.condition <- function(x) {
#   require(taRifx)
#   any(class(x)=="condition")
# }
# 
# is.condition(extract_tables(location, page = pagenb, method = "data.frame",columns=list(c(70, 180, 326, 425,524))))
# 
# catchToList <- function(expr) {
#   val <- NULL
#   myWarnings <- NULL
#   wHandler <- function(w) {
#     myWarnings <<- c(myWarnings, w$message)
#     invokeRestart("muffleWarning")
#   }
#   myError <- NULL
#   eHandler <- function(e) {
#     myError <<- e$message
#     NULL
#   }
#   val <- tryCatch(withCallingHandlers(expr, warning = wHandler), error = eHandler)
#   list(value = val, warnings = myWarnings, error=myError)
# } 
# 
# table <- catchToList({extract_tables(location, page = pagenb, method = "data.frame",columns=list(c(70, 180, 326, 425,524)))})
# 
# 
# get_simple_error_message <- function(e) e$message
# get_simple_error_call <- function(e) deparse(e$call)
# get_simple_error_message(table)
# sapply(results[the_fails], get_simple_error_message)
# sapply(results[the_fails], get_simple_error_call)
