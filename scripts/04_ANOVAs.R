
# Check for differences in `curious` between conditions -------------------

pilot |> 
        select(stim, curious) |> 
        na.omit() |> 
        anova_test(curious ~ stim) # ns

pilot |> 
        select(stim, curious) |> 
        na.omit() |> 
        group_by() |> 
        pairwise_t_test(curious ~ stim, p.adjust.method = "bonferroni")
        
pilot |> 
        select(stimQ, curious) |>
        na.omit() |> 
        group_by() |> 
        t_test(curious ~ stimQ) # ns

pilot |> 
        select(stimC, curious) |> 
        na.omit() |> 
        group_by() |> 
        t_test(curious ~ stimC) # ns
