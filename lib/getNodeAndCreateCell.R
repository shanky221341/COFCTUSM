getNodeAndCreateCell<-function(node){
  if(node %in% names(other_tables))
  {
    #if(node=='overall_official')
    print(node)
    getNodeAndCreateSeparateTableForNodeswithMultipleChild(node)
    #print(nrow(temp1))
    #assign(node,temp1,envir = .GlobalEnv)
    var_name<-paste(node,"temp",sep="_")
    assign(node,eval(parse(text=var_name)),envir = .GlobalEnv)
    
  }else{
    nodeName<-node
    nodeAddress<-paste("//",node,sep="")
    node<-getNodeSet(xmltop,nodeAddress)
    create_cell(nodeName,node)
  }
}