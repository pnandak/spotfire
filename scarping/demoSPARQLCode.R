#never ever ever convert strings to factors - we're not using factors in the data below
options(stringsAsFactors = FALSE)

library(SPARQL)
library(ggplot2)
library(igraph)

#SPARQL endpoint for the live (synchronized) version of DBpedia
endpoint = "http://live.dbpedia.org/sparql"

#Note: there may be duplicates in here if the company has multiple values for the same property
queryString = "PREFIX dbprop: <http://dbpedia.org/property/>
              PREFIX template: <http://dbpedia.org/resource/Template:>
              PREFIX dbpedia-owl: <http://dbpedia.org/ontology/>
              PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              select * where {
                ?company dbprop:wikiPageUsesTemplate template:Infobox_company . 
                ?company dbpedia-owl:numberOfEmployees ?numberOfEmployees . 
                ?company dbpedia-owl:netIncome ?netIncome . 
                ?company foaf:name ?name . 
              }"

#send query to endpoint & retrieve results
d <- SPARQL(url=endpoint, query=queryString, format='csv', extra='&format=text%2Fcsv')
#we get a list back, the actual tabular data is in d$results
data <- d$results

data$netIncomePerEmployee <- data$netIncome/data$numberOfEmployees

#plot net income per employee versus number of employees
ggplot(data=data, aes(x=data$numberOfEmployees, y=data$netIncomePerEmployee)) + 
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() + 
  xlab('Number of Employees') + 
  ylab('Net Income per Employee') + 
  opts(title = 'Net Income per Employee vs. Number of Employees')

#this query looks at the multiple ways to specify relationships between and owner company and its subsidiaries
queryString = "PREFIX dbprop: <http://dbpedia.org/property/>
              PREFIX template: <http://dbpedia.org/resource/Template:>
              PREFIX dbpedia-owl: <http://dbpedia.org/ontology/>
              PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              select ?companyName ?subsidiaryName where {
                ?company dbprop:wikiPageUsesTemplate template:Infobox_company . 
                ?company foaf:name ?companyName . 
                {?subsidiary dbpedia-owl:parentCompany ?company} UNION 
                {?subsidiary dbpedia-owl:parent ?company} UNION
                {?subsidiary dbpedia-owl:owner ?company } UNION 
                {?company dbpedia-owl:subsidiary ?subsidiary } UNION
                {?subsidiary dbpedia-owl:owningCompany ?company } .
                ?subsidiary foaf:name ?subsidiaryName .
              }"

d <- SPARQL(url=endpoint, query=queryString, format='csv', extra='&format=text%2Fcsv')

#load the results into a graph.  First column is the from node (company), second column is the to node (subsidiary)
g = graph.data.frame(d$results, directed=TRUE)

#use fruchterman reingold layout to make it look awesome
g <- simplify(g)
l <- layout.fruchterman.reingold(g)
l <- layout.norm(l, -1,1, -1,1) 
#this looks good as a 10,000 x 10,000 px image - looks like a hairball at default resolution
plot(g, layout=l, vertex.color='white', vertex.size=0.5, vertex.label=V(g)$name, vertex.label.cex=0.5, vertex.label.font=2, edge.arrow.size=0.1, xlim=range(l[,1]), ylim=range(l[,2]))

