data.prep <-
function(
  data, factor.structure,                                               #simple prerequisites
  
  number.of.subtests=1, items.per.subtest=NULL, invariance='parallel',  #subtest relations
  repeated.measures=NULL, long.invariance='strict',                     #longitudinal relations
  mtmm=NULL, mtmm.invariance='congeneric',                              #mtmm relations
  grouping=NULL, group.invariance='strict',                             #grouping relations

  item.invariance='congeneric',                                         #invariance between items
  item.long.invariance='strict',                                        #longitudinal item inv.
  item.mtmm.invariance='congeneric',                                    #mtmm item inv.
  item.group.invariance='strict',                                       #group item invariance

  auxiliary=NULL,                                                       #add variables
  ...
) { #function begin

  #check for correct variable naming
  tmp <- c(unlist(factor.structure),grouping) #all names provided

  if (any(!is.element(tmp,names(data)))) {
    stop('The variable names you provided do not match the variable names in your dataset.\n',call.=FALSE)
  }


  #create phantom longitudinal data, if only cross-sectional
  if (is.null(repeated.measures)) {
    repeated.measures <- as.list(names(factor.structure))
    names(repeated.measures) <- names(factor.structure)
    long.invariance <- 'congeneric'
  }

  #create phantom mtmm data, if only one method
  if (is.null(mtmm)) {
    mtmm <- as.list(names(factor.structure))
    names(mtmm) <- names(factor.structure)
    mtmm.invariance <- 'congeneric'
  }
  
  #create a short factor structure
  short.factor.structure <- list(NA)
  for (i in 1:length(repeated.measures)) {
    short.factor.structure[i] <- factor.structure[names(factor.structure)==repeated.measures[[i]][1]]
    names(short.factor.structure)[i] <- repeated.measures[[i]][1]
  }

  #check for correct number of subtests
  if (length(number.of.subtests)!=1 & length(number.of.subtests)!=length(factor.structure)) {
    stop('The length of the vector for the number of items is not compatible with the number of factors.\n',call.=FALSE)
  }

  number.of.subtests <- as.list(array(number.of.subtests,length(short.factor.structure)))

  #create a vector of items
  items <- unlist(factor.structure,use.names=FALSE)
  
  #create the subset of auxiliary variables
  auxi <- data[,auxiliary]
  names(auxi) <- auxiliary
  
  #create a vector of invariance assumptions
  if (length(invariance)!=1 & length(invariance)!=length(factor.structure)) {
    stop('The number of invariance levels and the number of factors are not compatible.\n',call.=FALSE)
  }
  
  invariance <- as.list(array(invariance,length(short.factor.structure)))

  #create a vector of longitudial invariance assumptions
  if (length(long.invariance)!=1 & length(long.invariance)!=length(factor.structure)) {
    stop('The number of longitudinal invariance levels and the number of factors are not compatible.\n',call.=FALSE)
  }

  long.invariance <- as.list(array(long.invariance,length(short.factor.structure)))

  #implement invariances of subtests
  long.equal <- invariance.implementation(data,
    factor.structure,short.factor.structure,repeated.measures,
    number.of.subtests,
    invariance,long.invariance,group.invariance,
    grouping,
    label.change=TRUE)


  #number of items per parcel
  #preset
  if (is.null(items.per.subtest)) {
    number.of.items <- list(NA)
    for (i in 1:length(short.factor.structure)) {
      if (number.of.subtests[[i]]>1) {
        number.of.items[[i]] <- as.numeric(table(cut(1:length(short.factor.structure[[i]]),number.of.subtests[[i]])))
      }
      else {
        stop('If you request only one subtest, you must provide items.per.subtest.',call.=FALSE)
      }
    }
  }
    
  #providing one-size-fits-all
  if (is.numeric(items.per.subtest)) {
    if (length(items.per.subtest)==1) {
      number.of.items <- list(NA)
      for (i in 1:length(short.factor.structure)) {
        number.of.items[[i]] <- array(items.per.subtest,number.of.subtests[[i]])
      }
    }
    else {
      stop('items.per.subtest must a numeric or a list.\n',call.=FALSE)
    }    
  }
    
  #providing specific numbers of items.per.parcel
  if (is.list(items.per.subtest)) {
    if (length(items.per.subtest)==length(factor.structure)) {
      number.of.items <- list(NA)
      for (i in 1:length(short.factor.structure)) {
        number.of.items[[i]] <- as.numeric(array(items.per.subtest[[i]],number.of.subtests[[i]]))
      }
    }
    else {
    stop('The list you supplied for items.per.subtest is not compatible with the factor.structure you provided.\n',call.=FALSE)
    }
  }      

  #create a vector of item invariance assumptions
  if (length(item.invariance)!=1 & length(item.invariance)!=length(factor.structure)) {
    stop('The number of item invariance levels and the number of factors are not compatible.\n',call.=FALSE)
  }
  
  item.invariance <- as.list(array(item.invariance,length(short.factor.structure)))

  #create a vector of longitudial item invariance assumptions
  if (length(item.long.invariance)!=1 & length(item.long.invariance)!=length(factor.structure)) {
    stop('The number of longitudinal item invariance levels and the number of factors are not compatible.\n',call.=FALSE)
  }

  item.long.invariance <- as.list(array(item.long.invariance,length(short.factor.structure)))
  
  #implement invariances of items
  item.long.equal <- invariance.implementation(data,
    factor.structure,short.factor.structure,repeated.measures,
    lapply(short.factor.structure,length),
    item.invariance,item.long.invariance,item.group.invariance,
    grouping)

  
  #implement mtmm invariances
  
#   # exclude all non-reference methods
#   short.factor.structure[!(names(short.factor.structure)%in%sapply(mtmm,function(x) x[1]))] <- NULL
#   
#   #create a vector of longitudial invariance assumptions
#   if (length(mtmm.invariance)!=1 & length(mtmm.invariance)!=length(factor.structure)) {
#     stop('The number of MTMM invariance levels and the number of factors are not compatible.\n',call.=FALSE)
#   }
#   
#   mtmm.invariance <- as.list(array(mtmm.invariance,length(short.factor.structure)))
#   
#   #implement invariances of subtests
#   mtmm.equal <- invariance.implementation(data,
#     factor.structure,short.factor.structure,mtmm,
#     number.of.subtests,
#     invariance,mtmm.invariance,group.invariance,
#     grouping,
#     label.change=TRUE)
#   
#   mtmm.item.invariance

  output <- list(short.factor.structure,long.equal,item.long.equal,
      number.of.items,data,factor.structure,auxi,number.of.subtests,invariance,
      repeated.measures,long.invariance,grouping,group.invariance,
      item.invariance,item.long.invariance,item.group.invariance)
  names(output) <- c('short.factor.structure','long.equal','item.long.equal',
      'number.of.items','data','factor.structure','auxi','number.of.subtests','invariance',
      'repeated.measures','long.invariance','grouping','group.invariance',
      'item.invariance','item.long.invariance','item.group.invariance')

  return(output)

}
