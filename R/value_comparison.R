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

