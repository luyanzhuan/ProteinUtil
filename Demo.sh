
###
 # @Author       : luyz
 # @Date         : 2024-08-16 18:35:54
 # @LastEditors  : luyz
 # @LastEditTime : 2024-08-16 19:14:45
 # @Description  : Demo for ProteinUtil
 # Copyright (c) 2024 by luyz && luyz@aptbiotech.com, All Rights Reserved. 
### 

# Venn Diagram demo
# Venn plot png/pdf in /data2/cloud/Code/ProteinUtil/Demo/VennDemo
docker run -it --rm --name protein_util -v /data2/cloud/Code/ProteinUtil/:/ProteinUtil/  protein_util:0.1 Rscript /ProteinUtil/Demo/VennDemo.R