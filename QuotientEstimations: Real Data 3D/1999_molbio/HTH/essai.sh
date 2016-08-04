#!/bin/csh
# computation of all the geometric transforms

Protein -m 2CRO -s 2WRP -out HTH

CrestRegister -m HTH_0.CKeys1 -s HTH_0.CKeys2 -type 0 -e HEME.error -pr ProtMatch_0

# add parenthesis 
cfilter HTH_0.CKeys1.inliers HTH_0.CKeys1_2.inliers -tr ProtMatch_0.trsf

l2avs HTH_0.CKeys1_2.inliers HTH_0.CKeys1_2.inliers.geom -kpoints 
l2avs HTH_0.CKeys2.inliers HTH_0.CKeys2.inliers.geom -kpoints 
