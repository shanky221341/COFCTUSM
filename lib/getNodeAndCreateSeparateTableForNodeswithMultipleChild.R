getNodeAndCreateSeparateTableForNodeswithMultipleChild<-function(node){
  
  table_name<-node
  nodeAddress<-paste("//",node,sep="")
  subnode<<-getNodeSet(xmltop,nodeAddress)
  #print(node)
  #print(length(subnode))
  
  xmlSubNodes<-other_tables[[node]]
  counter=0
  for(node in subnode){
    counter=counter+1
    sub_node<<-node
    
    # Here all the childrens for table with multiple childrens will be created with the same names as objects  
    # ------------------------------------------------------------------------
    
    for (node in xmlSubNodes){
      nodeName<-node
      nodeAddress<-paste("//",node,sep="")
      node<-getNodeSet(sub_node,nodeAddress)
      if(nodeName!='nct_id')
        create_cell(nodeName,node)
    }
    
    if(counter==1){
      xmlSubNodes<-append("nct_id",xmlSubNodes)
    }
    
    for(i in 1:length(xmlSubNodes)){
      if(grepl("/",xmlSubNodes[i])){
        xmlSubNodes[i]<-unlist(strsplit(xmlSubNodes[i],split="/"))[1]
      }
    }
    
    # Then after the childerens are created for a particualr table then using the following commnad the chararcter
    # names of the object can parsed with their values and then converted to the data frame
    
    # ------------------------------------------------------------------------------------------------------------
    
    char_vect<-sapply(xmlSubNodes,function(x)eval(parse(text=x)))
    assign(table_name,data.frame(as.list(char_vect)),envir = .GlobalEnv)
    
    #print(primary_outcome)
    var_name<-paste(table_name,"temp",sep="_")
    #print(var_name)
    
    # This will append the new tables to the previoulsy created tables
    # ----------------------------------------------------------------
    
    assign(var_name,rbind(eval(parse(text=var_name)),eval(parse(text=table_name))),envir = .GlobalEnv)
  }
}