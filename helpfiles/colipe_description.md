### Colipe - Assessing the bias in evaluation data
***
<p style='text-align: justify; margin:0px; margin-bottom:6px;'> Colipe is a tool for evaluating a benchmarking data (i.e., the training and test sets) for a link prediction problem in the context of network biology in terms of the bias in data and evaluation setting towards the well-studied biological entities. For this purpose, three types of analysis are provided: </p>
- <p style='text-align: justify; margin:0px; margin-bottom:2.5px;'> <b>Imbalance in edge distributions:</b> This analysis provides the percentage of edges for different categories (e.g., Rich-Rich and Rich-Poor) based on a subset of nodes designated as rich nodes (with the highest degrees in the network). </p>
- <p style='text-align: justify; margin:0px; margin-bottom:2.5px;'> <b>Influence on the evaluation:</b> This analysis estimates how much the under-studied entities (with the lowest degrees in the network) are under-represented in the evaluation setting. </p>
- <p style='text-align: justify; margin:0px; margin-bottom:2.5px;'> <b>Separability analysis for predictive power of node degree info:</b> This analysis investigates how much an evaluation setting favors bias in predictions (i.e., how much does it incentivize an algorithm to bring forward well-studied entities in their predictions?). 

