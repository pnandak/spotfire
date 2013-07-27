# store joint density as three dimensional array

# problem 1

jd = array(c(.3, .099, .001, .096, .001, .003, .25, .05, 0, .17, .03,0), c(3,2,2),
  dimnames = list(c("Y=0", "Y=1", "Y=2"), c("X=0", "X=1"), c("Z=0", "Z=1")))

# question a

pyx00 = (jd["Y=0","X=0","Z=0"] + jd["Y=0","X=0","Z=1"])/sum(jd[,"X=0",])
pyx01 = (jd["Y=1","X=0","Z=0"] + jd["Y=1","X=0","Z=1"])/sum(jd[,"X=0",])
pyx02 = (jd["Y=2","X=0","Z=0"] + jd["Y=2","X=0","Z=1"])/sum(jd[,"X=0",])
pyx10 = (jd["Y=0","X=1","Z=0"] + jd["Y=0","X=1","Z=1"])/sum(jd[,"X=1",])
pyx11 = (jd["Y=1","X=1","Z=0"] + jd["Y=1","X=1","Z=1"])/sum(jd[,"X=1",])
pyx12 = (jd["Y=2","X=1","Z=0"] + jd["Y=2","X=1","Z=1"])/sum(jd[,"X=1",])

q1_mat = t(matrix(c(pyx00, pyx10, pyx01, pyx11, pyx02, pyx12), nrow = 2, ncol = 3))
colnames(q1_mat) = c("X=0", "X=1")
rownames(q1_mat) = c("Y=0", "Y=1", "Y=2")
    
library(xtable)
xtable(q1_mat, digits = c(0,4,4))

# question b
# post-intervention probabilities p(Y(X=x)=y) calculated via backdoor conditioning

pyx00b = sum(jd[,,"Z=0"])*jd["Y=0","X=0","Z=0"]/sum(jd[ ,"X=0","Z=0"]) +
        sum(jd[,,"Z=1"])*jd["Y=0","X=0","Z=1"]/sum(jd[,"X=0","Z=1"])
pyx01b = sum(jd[,,"Z=0"])*jd["Y=1","X=0","Z=0"]/sum(jd[,"X=0","Z=0"]) +
        sum(jd[,,"Z=1"])*jd["Y=1","X=0","Z=1"]/sum(jd[,"X=0","Z=1"])
pyx02b = sum(jd[,,"Z=0"])*jd["Y=2","X=0","Z=0"]/sum(jd[,"X=0","Z=0"]) +
        sum(jd[,,"Z=1"])*jd["Y=2","X=0","Z=1"]/sum(jd[,"X=0","Z=1"])
pyx10b = sum(jd[,,"Z=0"])*jd["Y=0","X=1","Z=0"]/sum(jd[,"X=1","Z=0"]) +
        sum(jd[,,"Z=1"])*jd["Y=0","X=1","Z=1"]/sum(jd[,"X=1","Z=1"])
pyx11b = sum(jd[,,"Z=0"])*jd["Y=1","X=1","Z=0"]/sum(jd[,"X=1","Z=0"]) +
        sum(jd[,,"Z=1"])*jd["Y=1","X=1","Z=1"]/sum(jd[,"X=1","Z=1"])
pyx12b = sum(jd[,,"Z=0"])*jd["Y=2","X=1","Z=0"]/sum(jd[,"X=1","Z=0"]) +
        sum(jd[,,"Z=1"])*jd["Y=2","X=1","Z=1"]/sum(jd[,"X=1","Z=1"])

q1b_mat = q1_mat[]
q1b_mat[,"X=0"] = c(pyx00b, pyx01b, pyx02b)
q1b_mat[,"X=1"] = c(pyx10b, pyx11b, pyx12b)

xtable(q1b_mat, digits = c(0,4,4))

## ace -- using post-intervention distribution
(ace1 = 2*q1b_mat["Y=2","X=1"] + q1b_mat["Y=1", "X=1"] -
  (2*q1b_mat["Y=2","X=0"] + q1b_mat["Y=1", "X=0"]))
  
# using direct adjustment -- ace by strata

ace_z0 = (2*jd["Y=2","X=1","Z=0"]+1*jd["Y=1","X=1","Z=0"])/sum(jd[,"X=1","Z=0"]) -
  (2*jd["Y=2","X=0","Z=0"]+1*jd["Y=1","X=0","Z=0"])/sum(jd[,"X=0","Z=0"])

ace_z1 = (2*jd["Y=2","X=1","Z=1"]+1*jd["Y=1","X=1","Z=1"])/sum(jd[,"X=1","Z=1"]) -
  (2*jd["Y=2","X=0","Z=1"]+1*jd["Y=1","X=0","Z=1"])/sum(jd[,"X=0","Z=1"])

# weight by Z marginals
(ace2 = sum(jd[,,"Z=0"])*ace_z0 + sum(jd[,,"Z=1"])*ace_z1)

# alternate table showing outcomes by level of X and Z

q1b_matz1 = q1_mat[]
q1b_matz1[,"X=0"] = jd[,"X=0","Z=1"]/sum(jd[,"X=0","Z=1"])
q1b_matz1[,"X=1"] = jd[,"X=1","Z=1"]/sum(jd[,"X=1","Z=1"])

q1b_matz0 = q1_mat[]
q1b_matz0[,"X=0"] = jd[,"X=0","Z=0"]/sum(jd[,"X=0","Z=0"])
q1b_matz0[,"X=1"] = jd[,"X=1","Z=0"]/sum(jd[,"X=1","Z=0"])

xtable(q1b_matz0, digits = c(0,4,4))
xtable(q1b_matz1, digits = c(0,4,4))
