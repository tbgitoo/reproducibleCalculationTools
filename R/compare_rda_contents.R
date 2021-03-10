compare_rda_contents<-function(path1,path2,numerical_tolerance=0,verbose=TRUE,exclude_variables=vector(mode="character",length=0),relative_tolerance_evaluation=FALSE){
    if(!file.exists(path1))
    {
        if(verbose){cat(paste("\n\tFile ",path1," does not exist",sep=""))}
        return(FALSE)
    }
    if(!file.exists(path2))
    {
        if(verbose){cat(paste("\n\tFile ",path2," does not exist",sep=""))}
        return(FALSE)
    }
    if(verbose & length(exclude_variables)>0)
    {
        cat("Excluded from comparison: ",exclude_variables,"\n")
    }
    e1=new.env()
    e2=new.env()
    load(path1,envir=e1)
    load(path2,envir=e2)
    # \nCompare the file contents
    if(verbose){
        
        cat("\tComparing data file contents by number of variables\n")
        cat("\tSame number of non-excluded variables?\n")
    }
    
    listing_1 = ls(e1)
    listing_1 = listing_1[!(listing_1 %in% exclude_variables)]
    
    listing_2 = ls(e2)
    listing_2 = listing_2[!(listing_2 %in% exclude_variables)]
    
    
    same_number_of_variables = (length(listing_1)==length(listing_2))
    
    if(verbose){
        cat("\t\t")
        print(same_number_of_variables)
    }
    
    if(!same_number_of_variables)
    {
        return(FALSE)
    }
    
    
    if(verbose){
        
        cat("\tComparing data file contents by variable names\n")
        cat("\tAll variable names identical?\n")
    }
    
    variables_identical=(all(sort(listing_1)==sort(listing_2)))
    
    if(variables_identical)
    {
        
        if(verbose)
        {
            cat("\t\t")
            print(variables_identical)
        }
        
    }
    
    
    
    
    
    if(!variables_identical)
    return(FALSE)
    
    all_comparisons_succesful=TRUE
    
    varnames=sort(ls(e1))
    to_exclude=exclude_variables[exclude_variables %in% varnames]
    if(length(to_exclude))
    {
        
        if(verbose)
        {
            cat("\tExcluding variables from comparison:\n\t\t")
            for(ex in to_exclude)
            {
                cat(ex)
                cat("\t")
            }
            cat("\n")
        }
        varnames=varnames[!(varnames %in% exclude_variables)]
    }
    
    
    if(verbose)
    {
        cat("\tComparing individual variables\n")
        cat("\tAre all the column names the same?")
    }
    
    
    
    
    for(varname in varnames)
    {
        v1=get(varname,envir=e1)
        if(!(is.function(v1) | class(v1)=="list"))
        {
            
            v2=get(varname,envir=e2)
            listing_colnames_1 = colnames(v1)
            listing_colnames_2 = colnames(v2)
            # Exclude excluded columns
            listing_colnames_1=listing_colnames_1[
                !(paste(varname,listing_colnames_1,sep="/") %in% exclude_variables )]
            listing_colnames_2=listing_colnames_2[
                    !(paste(varname,listing_colnames_2,sep="/") %in% exclude_variables )]
                
            
            if(length(listing_colnames_1)!=length(listing_colnames_2))
            {
                all_comparisons_succesful=FALSE
                colnames_identical=FALSE
            } else {
                colnames_identical=all(listing_colnames_1==listing_colnames_2)
                if(!colnames_identical)
                {
                    all_comparisons_succesful=FALSE
                }
            }
            
            
            if(verbose)
            {
                cat(paste("\n\t\tComparing variable ",varname,sep=""))
                cat(paste(": ",colnames_identical,sep=""))
            }
            
        } else
        {
            if(is.function(v1))
            {
                if(verbose)
                {
                    cat(paste("\n\t\t",varname," is a function",sep=""))
                }
            }
            if(class(v1)=="list")
            {
                cat(paste("\n\t\tComparing variable ",varname,sep=""))
                v2=get(varname,envir=e2)
                list_comparison_OK=all(names(v1)==names(v2)) & (length(v1)==length(v2))
                if(list_comparison_OK)
                {
                    if(verbose)
                    {
                        cat(": This is a list, names and lengths are identical")
                    }
                } else
                {
                    if(verbose)
                    {
                        cat(": This is a list, names or lengths differ")
                        all_comparisons_succesful=FALSE
                    }
                }
            }
            
        }
        
    }
    
    if(!all_comparisons_succesful)
    { return(FALSE)}
    
    
    cat("\n\tAre all the values in the variables the same?")
    if(numerical_tolerance>0){
        cat(paste("\n\t\t(Numerical tolerance ",numerical_tolerance,")",sep=""))
        if(relative_tolerance_evaluation)
        {
            cat(" - relative to mean of absolute values ")
        }
    }
    for(varname in varnames)
    {
        v1=get(varname,envir=e1)
        if(!(is.function(v1) | class(v1)=="list"))
        {
            v2=get(varname,envir=e2)
            if(verbose){cat(paste("\n\t\tComparing variable ",varname,sep=""))}
            
            if(!is.null(colnames(v1)) & !is.null(colnames(v2)))
            {
                c1 = colnames(v1)[!(paste(varname,colnames(v1),sep="/") %in% exclude_variables )]
                c2 = colnames(v2)[!(paste(varname,colnames(v2),sep="/") %in% exclude_variables )]
                v1 = v1[,c1]
                v2 = v2[,c2]
                
            }
            
            comparison_OK=value_comparison(v1,v2,verbose=verbose,
            numerical_tolerance=numerical_tolerance,relative_tolerance_evaluation=relative_tolerance_evaluation)
            
            if(!comparison_OK) { all_comparisons_succesful=FALSE}
            
        } else {
            if(is.function(v1))
            {
                if(verbose){cat(paste("\n\t\t",varname," is a function, not comparing values",sep=""))}
            }
            if(class(v1)=="list")
            {
                v2=get(varname,envir=e2)
                
                if(verbose){cat(paste("\n\t\tComparing list variable ",varname,sep=""))}
                # We consider the values equivalent
                # if they are either the same or both NA
                list_OK=TRUE
                for(index in 1:length(v1))
                {
                    if(!value_comparison(v1[[index]],v2[[index]],verbose=TRUE,numerical_tolerance=numerical_tolerance,
                    relative_tolerance_evaluation=relative_tolerance_evaluation))
                    {
                        list_OK=FALSE
                    }
                    
                    
                }
                if(list_OK) {cat(": all elements match") } else {
                    all_comparisons_succesful=FALSE
                    cat(": Differences found")
                }
                
            }
        }
        
    }
    return(all_comparisons_succesful)
    
    
}


