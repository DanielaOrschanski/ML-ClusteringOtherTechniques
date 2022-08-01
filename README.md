# ML-ClusteringOtherTechniques

Your company needs to understand the operation of the drilling machine they just purchased. Unfortunately, they only know that the machine operates in different states, but instructions are in Chinese and the producer does not reply.
The engineers take some operational measurements (400 observations of two variables) and ask you to analyze the data to
- find out the number of states in which the machine operates
- find out the state for each of the 400 observations

The data is stored in drilling.csv. Without labels, you decide to approach the problem with a clustering technique. 
Try with all techniques learned in the course:

1. k-means: Different choices of K --> Elbow --> Choose K --> Final cluster Memberships

2. Hierarchical clustering: Different Linkage --> Choose Linkage --> Look Dendrogram --> Choose cut --> Final cluster memberships

3. DBscan: Set MinPTS --> Knee --> Set eps --> Find cluster memberships

4. OPTIONAL (Bonus +2 pts): OPTICS and HDBscan 
Do all of them produce the same result?
As usual, please comment code/plots.
