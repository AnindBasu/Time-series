testdf <- function(variable, max.augmentations, varname)
	{
  
    require(fUnitRoots)
    require(lmtest)
  
	results_adf <- data.frame(augmentations = -1, adf = 0, p_adf = 0, bgodfrey = 0, p_bg = 0)
	variable <- coredata(variable[!is.na(variable)])
	
	for(augmentations in 0:max.augmentations)
		{
		df.test_ <- adfTest(variable, lags = augmentations, type = "c")
		df_ <- as.numeric(df.test_@test$statistic)
		p_adf <- as.numeric(df.test_@test$p.value)
		resids_ <- df.test_@test$lm$residuals
		bgtest_ <- bgtest(resids_~1, order = 1)
		bgodfrey <- bgtest_$statistic
		names(bgodfrey) <- NULL
		p_bg <- bgtest_$p.value
		
		results_adf <- rbind(results_adf, data.frame(augmentations = augmentations, adf = df_, p_adf = p_adf,
                                                 bgodfrey = bgodfrey, p_bg = p_bg))
		rm(df.test_, df_, resids_, bgtest_, bgodfrey, p_bg)
		}
	
	results_adf <- results_adf[results_adf$augmentations >= 0,]
	
	row.names(results_adf) <- NULL
	
	#plot(variable, type = "l", col = "darkblue", lwd = 2, 
	#     main = paste("Plot of the examined variable",varname))

	return(results_adf)
	}	