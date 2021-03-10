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


value_comparison<-function(v1,v2,verbose=TRUE,numerical_tolerance=0,relative_tolerance_evaluation=FALSE)
{
    # check dimensionality, must be the same (or possibly null)
    if(!all(dim(v1)==dim(v2)))
    {
        if(verbose){cat(": unequal size")}
        return(FALSE)
    }
    
    # Check also the length in case it's vectors
    if(!(length(v1)==length(v2)))
    {
        if(verbose){cat(": unequal length")}
        return(FALSE)
    }
    
    # Either both, or none of the two variables should be NA at the same place
    if(any(xor(is.na(v1),is.na(v2))))
    {
        if(verbose){cat("Mismatch in NA elements") }
        return(FALSE)
    }
    
    
    
    comparison=(!is.na(v1) & v1==v2)|(is.na(v1) & is.na(v2))
    comparison_OK=all(comparison)
    # There are sometimes issues with numerical conversions in bulk comparison, so if there are problems, compare individually
    
    if(!comparison_OK)
    {
        # Special case: dataframes, these have different types
        if(class(v1)=="data.frame" & class(v2)=="data.frame")
        {
            
            for(colIndex in 1:(dim(v1)[2]))
            {
                remaining_v1 = v1[,colIndex]
                remaining_v2 = v2[,colIndex]
                # Mismatch in NA values
                if(any(xor(is.na(remaining_v1),is.na(remaining_v2))))
                {
                    if(verbose){cat(paste(": Not all NA values match (column ", colIndex," )",sep="")) }
                    return(FALSE)
                    
                }
                
                remaining_v1=remaining_v1[!is.na(remaining_v1)]
                remaining_v2=remaining_v2[!is.na(remaining_v2)]
                
                # examine the critical cases that are not anyways identical
                
                
                
                
                # Test whether they are all the same numerically
                if(!all(remaining_v1==remaining_v2))
                {
                    
                    not_identical=!(remaining_v1==remaining_v2)
                    
                    remaining_v1=remaining_v1[not_identical]
                    remaining_v2=remaining_v2[not_identical]
                    
                    
                    if(numerical_tolerance>0)
                    {
                        # The values are numeric and so no conversion is needed
                        if(is.numeric(remaining_v1) & is.numeric(remaining_v2) )
                        {
                            mean_val=(abs(remaining_v1)+abs(remaining_v2))/2
                            deviation=abs(remaining_v2-remaining_v1)
                            if(relative_tolerance_evaluation)
                            {
                                deviation=deviation/mean_val
                            }
                            if(any(deviation>numerical_tolerance))
                            {
                                if(verbose){cat(paste(": Numerical tolerance exceeded  (column ", colIndex," )",sep="")) }
                                return(FALSE)
                            }
                        } else {
                            # Not numeric, but is conversion possible?
                            if(suppressWarnings(!any(is.na(as.numeric(remaining_v1)))) & suppressWarnings(!any(is.na(as.numeric(remaining_v2)))))
                            {
                                remaining_v1=as.numeric(remaining_v1)
                                remaining_v2=as.numeric(remaining_v2)
                                mean_val=(abs(remaining_v1)+abs(remaining_v2))/2
                                deviation=abs(remaining_v2-remaining_v1)
                                if(relative_tolerance_evaluation)
                                {
                                    deviation=deviation/mean_val
                                }
                                if(any(deviation>numerical_tolerance))
                                {
                                    if(verbose){cat(paste(": Numerical tolerance exceeded  (column ", colIndex," )",sep="")) }
                                    return(FALSE)
                                }
                                
                            } else {
                                # We can't convert all values that were unequal, and so there must be an issue
                                if(verbose){cat(paste(": Column with unequal values is not numeric, tolerance cannot be applied  (column ", colIndex," )",sep="")) }
                                return(FALSE)
                            }
                            
                        }
                        
                        
                        
                        
                    } else {
                        if(verbose){cat(paste(": Numerical tolerance 0, but non-identical values  (column ", colIndex," )",sep="")) }
                        return(FALSE)
                    }
                    
                    
                }
                
            }
            
            if(verbose){cat(paste(": ",TRUE,sep=""))}
            
            
            return(TRUE)
            
            
        }
        
        
        problematic_v1=v1[!comparison]
        problematic_v2=v2[!comparison]
        # Check the the NA entries are the same
        NA_OK=all(is.na(problematic_v1[is.na(problematic_v2)])) &
        all(is.na(problematic_v2[is.na(problematic_v1)]))
        if(NA_OK){
            # Compare the potentially problematic values directly (to exclude minor issues with numerical imprecision)
            values_OK=all(problematic_v1[!is.na(problematic_v1)]==problematic_v2[!is.na(problematic_v2)])
            
            if(values_OK)
            {
                comparison_OK=TRUE
            } else {
                # This is not a reading problem, but rather a true numerical problem. In least squares fitting, particulary, this can be an issue since the implementations are not necessary exactly the same
                
                # In this case, it makes sense to allow for some relative numerical tolerance
                if(numerical_tolerance>0)
                {
                    numerical1=suppressWarnings(as.numeric(problematic_v1[!is.na(problematic_v1)]))
                    numerical2=suppressWarnings(as.numeric(problematic_v2[!is.na(problematic_v2)]))
                    if(any(is.na(numerical1)) | any(is.na(numerical2)))
                    {
                        cat(": Numerical comparison not applicable to all values with differences")
                    } else {
                        mean_val=(abs(numerical1)+abs(numerical2))/2
                        deviation=abs(numerical2-numerical1)
                        if(relative_tolerance_evaluation)
                        {
                            deviation=deviation/mean_val
                        }
                        if(all(deviation<numerical_tolerance))
                        {
                            comparison_OK=TRUE
                        }
                        
                    }
                    
                }
                
                
            }
            
        }
        
        
    }
    
    if(verbose){cat(paste(": ",comparison_OK,sep=""))}
    return(comparison_OK)
    
    
}

