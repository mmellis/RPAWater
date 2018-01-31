## Moving data input to separate file to improve readability

# population data:
popdataA1 <-  read.table("rawdata/pop_A1B_crb.txt")
rownames(popdataA1) <- watershed
colnames(popdataA1) <- year

popdataA2 <-  read.table("rawdata/pop_A2_crb.txt")
rownames(popdataA2) <- watershed
colnames(popdataA2) <- year

popdataB2 <-  read.table("rawdata/pop_B2_crb.txt")
rownames(popdataB2) <- watershed
colnames(popdataB2) <- year

# income data
incdataA1 <-  read.table("rawdata/income_A1B_crb.txt")
rownames(popdataA1) <- watershed
colnames(popdataA1) <- year

incdataA2 <-  read.table("rawdata/income_A2_crb.txt")
rownames(popdataA2) <- watershed
colnames(popdataA2) <- year

incdataB2 <-  read.table("rawdata/income_B2_crb.txt")
rownames(popdataB2) <- watershed
colnames(popdataB2) <- year

# precipitaion data - growing season precip
precipdata_CGCA1B <- read.table("rawdata/precip_CGCA1B_crb.txt", header=TRUE)
rownames(precipdata_CGCA1B) <- watershed
colnames(precipdata_CGCA1B) <- year

precipdata_CGCA2 <- read.table("rawdata/precip_CGCA2_crb.txt", header=TRUE)
rownames(precipdata_CGCA2) <- watershed
colnames(precipdata_CGCA2) <- year

precipdata_CGCB2 <- read.table("rawdata/precip_CGCB2_crb.txt", header=TRUE)
rownames(precipdata_CGCB2) <- watershed
colnames(precipdata_CGCB2) <- year

precipdata_CSIROA1B <- read.table("rawdata/precip_CSIROA1B_crb.txt", header=TRUE)
rownames(precipdata_CSIROA1B) <- watershed
colnames(precipdata_CSIROA1B) <- year

precipdata_CSIROA2 <- read.table("rawdata/precip_CSIROA2_crb.txt", header=TRUE)
rownames(precipdata_CSIROA2) <- watershed
colnames(precipdata_CSIROA2) <- year

precipdata_CSIROB2 <- read.table("rawdata/precip_CSIROB2_crb.txt", header=TRUE)
rownames(precipdata_CSIROB2) <- watershed
colnames(precipdata_CSIROB2) <- year

precipdata_MIROCA1B <- read.table("rawdata/precip_MIROCA1B_crb.txt", header=TRUE)
rownames(precipdata_MIROCA1B) <- watershed
colnames(precipdata_MIROCA1B) <- year

precipdata_MIROCA2 <- read.table("rawdata/precip_MIROCA2_crb.txt", header=TRUE)
rownames(precipdata_MIROCA2) <- watershed
colnames(precipdata_MIROCA2) <- year

precipdata_HADB2 <- read.table("rawdata/precip_HADB2_crb.txt", header=TRUE)
rownames(precipdata_HADB2) <- watershed
colnames(precipdata_HADB2) <- year

# evapotranspiration data - growing season et

etdata_CGCA1B <- read.table("rawdata/et_CGCA1B_crb.txt", header=TRUE)
rownames(etdata_CGCA1B) <- watershed
colnames(etdata_CGCA1B) <- year

etdata_CGCA2 <- read.table("rawdata/et_CGCA2_crb.txt", header=TRUE)
rownames(etdata_CGCA2) <- watershed
colnames(etdata_CGCA2) <- year

etdata_CGCB2 <- read.table("rawdata/et_CGCB2_crb.txt", header=TRUE)
rownames(etdata_CGCB2) <- watershed
colnames(etdata_CGCB2) <- year

etdata_CSIROA1B <- read.table("rawdata/et_CSIROA1B_crb.txt", header=TRUE)
rownames(etdata_CSIROA1B) <- watershed
colnames(etdata_CSIROA1B) <- year

etdata_CSIROA2 <- read.table("rawdata/et_CSIROA2_crb.txt", header=TRUE)
rownames(etdata_CSIROA2) <- watershed
colnames(etdata_CSIROA2) <- year

etdata_CSIROB2 <- read.table("rawdata/et_CSIROB2_crb.txt", header=TRUE)
rownames(etdata_CSIROB2) <- watershed
colnames(etdata_CSIROB2) <- year

etdata_MIROCA1B <- read.table("rawdata/et_MIROCA1B_crb.txt", header=TRUE)
rownames(etdata_MIROCA1B) <- watershed
colnames(etdata_MIROCA1B) <- year

etdata_MIROCA2 <- read.table("rawdata/et_MIROCA2_crb.txt", header=TRUE)
rownames(etdata_MIROCA2) <- watershed
colnames(etdata_MIROCA2) <- year

etdata_HADB2 <- read.table("rawdata/et_HADB2_crb.txt", header=TRUE)
rownames(etdata_HADB2) <- watershed
colnames(etdata_HADB2) <- year


# initial withdrawals for year 2010
wd0 <-  read.table("rawdata/wd0_crb.txt", header=TRUE)
rownames(wd0)=watershed
wd0