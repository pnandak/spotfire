
# Load in the data to R

rtw.table <- read.table("http://www.ilr.cornell.edu/~hadi/RABE4/Data4/P005.txt", header=T, sep='\t')

# This tells R 'where to find the variables named in the table'
attach(rtw.table)

        png("rtwl_col.png", height=600, width=600)
        

# Boxplot of income versus right-to-work

boxplot(COL ~ RTWL, col='orange', pch=23, bg='red')

        dev.off()
        
        png("rtwl_income.png", height=600, width=600)
        

# Boxplot of income versus right-to-work

boxplot(Income ~ RTWL, col='orange', pch=23, bg='red')

        dev.off()
        
        png("union_col.png", height=600, width=600)
        

# Plot of unionization rate against COL

plot(URate, COL, pch=23, bg='red')

        dev.off()
        
        png("union_income.png", height=600, width=600)
        

# Plot of unionization rate against income

plot(URate, Income, pch=23, bg='red')

        dev.off()
        
        png("pop_income.png", height=600, width=600)
        

# Plot of population against income

plot(Pop, Income, pch=23, bg='red')

        dev.off()
        
        png("col_income.png", height=600, width=600)
        

# Plot of income against cost of living

plot(Income, COL, pch=23, bg='red')

        dev.off()
        
        png("pairs.png", height=600, width=600)
        
pairs(rtw.table)

        dev.off()
        
        png("pairs_noNY.png", height=600, width=600)
        
pairs(rtw.table[-27,], pch=23, bg='red')

        dev.off()
        