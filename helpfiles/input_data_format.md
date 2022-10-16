### Input Data Format
***
Input network data to specify the edges in the network should be in the form of a csv file having the following columns:
- <b>NodeIdentifier1:</b> Node identifier of a node \(u\) for an edge \((u, v)\)
- <b>NodeIdentifier2:</b> Node identifier of a node \(v\) for an edge \((u, v)\)
- <b>InTestSet:</b> True or False, indicating whether the specified edge is in the test set or the training set. 

To see an example, please visit the [Github page](https://github.com/serhan-yilmaz/CoLiPE) for sample networks.  

Note that, for bipartite networks involving two node types, the first identifier should correspond to one type and the second identifier to the other one. For example, for kinase-substrate network involving kinases and phosphosites, <em>NodeIdentifier1</em> corresponds to a kinase and <em>NodeIdentifier2</em> corresponds to a phosphosite for all edges. 