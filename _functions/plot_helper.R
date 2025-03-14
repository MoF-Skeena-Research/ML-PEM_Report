# plot helper


# functions to help standardise plot s

format_accuracy_measures <- function(indata) {
  
  # testing line 
 #indata <- bsRes2_all

indata <- indata %>%
  mutate(type = case_when(
    str_detect(accuracy_type, "aspat") ~ "aspatial",
    str_detect(accuracy_type, "spat") ~ "spatial"))  %>%
  mutate(type_model = case_when(
    str_detect(accuracy_type, "_overall") ~ "area-weighted",
    str_detect(accuracy_type, "_meanacc") ~ "unweighted")) %>%
  mutate(accuracy_type_label = case_when(
    str_detect(accuracy_type, "_p_") ~ "p",
    str_detect(accuracy_type, "_pa_") ~ "pa",
    str_detect(accuracy_type, "_fp_") ~ "fp",
    str_detect(accuracy_type, "_pf_") ~ "fp",
    str_detect(accuracy_type, "_fpa_") ~ "fpa")) %>%
  mutate(type_label = paste0(type, "_", type_model))


# set up order for plots 
indata$type_f = factor(indata$type_label, levels = c("spatial_area-weighted","aspatial_area-weighted", "spatial_unweighted", "aspatial_unweighted"))

indata$accuracy_type_label = factor(indata$accuracy_type_label, levels = c("p","pa", "fp","fpa"))

return(indata)
}



# generate plots for overall accuracy based on accuracy outputs 


generate_accuracy_plots <- function(acc){
    
    acc <- acc %>%
      dplyr::filter(acc_type == "test_estimate") 
    
    acc_sum <- acc %>%
      mutate(across(ends_with("overall"), ~.x *100)) %>%
      mutate(across(ends_with("meanacc"), ~.x *100)) %>%
      dplyr::select(slice, acc_type, transect_no,
                    aspat_p_overall,  aspat_p_meanacc, 
                    aspat_fp_overall,  aspat_fp_meanacc,
                    spat_p_overall, spat_p_meanacc,
                    spat_pf_overall,  spat_pf_meanacc, 
                    aspat_pa_overall,  aspat_pa_meanacc,
                    aspat_fpa_overall, aspat_fpa_meanacc,
                    spat_pa_overall,  spat_pa_meanacc,
                    spat_fpa_overall, spat_fpa_meanacc ) %>%
      distinct() 
      
      acc_sum_long <- acc_sum %>%
        pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>%
        filter(!accuracy_type == "transect_no") %>%
        mutate(type = case_when(
          str_detect(accuracy_type, "aspat") ~ "aspatial",
          str_detect(accuracy_type, "spat") ~ "spatial"))  %>%
        mutate(type_model = case_when(
          str_detect(accuracy_type, "_overall") ~ "overall",
          str_detect(accuracy_type, "_meanacc") ~ "average")) %>%
        mutate(accuracy_type_label = case_when(
          str_detect(accuracy_type, "_p_") ~ "p",
          str_detect(accuracy_type, "_pa_") ~ "pa",
          str_detect(accuracy_type, "_fp_") ~ "fp",
          str_detect(accuracy_type, "_pf_") ~ "fp",
          str_detect(accuracy_type, "_fpa_") ~ "fpa")) %>%
        mutate(type_label = paste0(type, "_", type_model))

# calculate the weighted mean and st dev summary 
      
      acc_wt_ave <- acc_sum %>%
        summarise(mutate(across(where(is.numeric), ~ weighted.mean(.x, transect_no, na.rm = FALSE)))) %>%
        pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "ave_wt") 
      
      acc_wt_sd <- acc_sum %>%         
        summarise(mutate(across(where(is.numeric), ~ sqrt(wtd.var(.x, transect_no, na.rm = FALSE))))) %>%
        pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "sd_wt") 
      
      acc_wt_sum <- left_join(acc_wt_ave, acc_wt_sd ) %>%
        filter(!accuracy_type == "transect_no") 

      acc_wt_sum <- acc_wt_sum  %>%
        mutate(type = case_when(
          str_detect(accuracy_type, "aspat") ~ "aspatial",
          str_detect(accuracy_type, "spat") ~ "spatial")) %>%
        mutate(type_model = case_when(
          str_detect(accuracy_type, "_overall") ~ "overall",
          str_detect(accuracy_type, "_meanacc") ~ "average")) %>%
        mutate(accuracy_type_label = case_when(
          str_detect(accuracy_type, "_p_") ~ "p",
          str_detect(accuracy_type, "_pa_") ~ "pa",
          str_detect(accuracy_type, "_fp_") ~ "fp",
          str_detect(accuracy_type, "_pf_") ~ "fp",
          str_detect(accuracy_type, "_fpa_") ~ "fpa")) %>%
        mutate(type_label = paste0(type, "_", type_model))
        
        
        # set up order for plots 
        acc_sum_long$type_f = factor(acc_sum_long$type_label, levels = c("spatial_overall","aspatial_overall", "spatial_average", "aspatial_average"))
        
        acc_sum_long$accuracy_type_label = factor(acc_sum_long$accuracy_type_label, levels = c("p","pa", "fp","fpa"))

        # join the weighted vak=lues
        test_sum_long <- acc_sum_long %>%
            left_join( acc_wt_sum )

        test_sum_long$accuracy_type_label = factor(test_sum_long$accuracy_type_label, levels = c("p","pa", "fp","fpa"))


        p3 <- ggplot(aes(y = value, x = accuracy_type_label), data = test_sum_long) + 
          geom_boxplot() +
          facet_wrap(~type_f, scales = "free_y", nrow = 2) +
          geom_hline(yintercept = 65,linetype ="dashed", color = "black") + 
          ggtitle("Accuracy measures (median + quartiles)") + 
          xlab("Mapunit") + ylab("Accuracy") + 
          ylim(-0.05, 100) +
          #theme_bw() + 
          theme_pem_facet() + 
          scale_fill_discrete_sequential(palette = "Light Grays")+ 
          geom_point(aes(y = ave_wt, x = accuracy_type_label), data = test_sum_long, shape = 5, size = 2) 
        
        
        p3


        # 
        # 
        # ## Accuracy per mapunit figures : Accuracy per unit 
        # mu_acc <- acc %>%
        #   dplyr::select(slice, target, acc_type, transect_no,
        #                 aspat_p_unit_pos, aspat_p_meanacc, 
        #                 aspat_fp_unit_pos, aspat_fp_meanacc,
        #                 aspat_pa_unit_pos, aspat_pa_meanacc,
        #                 aspat_fpa_unit_pos,aspat_fpa_meanacc,
        #                 spat_p_unit_pos, spat_p_meanacc,
        #                 spat_pf_unit_pos, spat_pf_meanacc, 
        #                 spat_pa_unit_pos, spat_pa_meanacc,
        #                 spat_fpa_unit_pos, spat_fpa_meanacc
        #   ) %>%
        #   dplyr::filter(acc_type == "test_estimate") %>%
        #   pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "value") %>% filter(accuracy_type != "transect_no")
        # 
        # 
        # mu_acc <- mu_acc %>%
        #   mutate(type = case_when(
        #     str_detect(accuracy_type, "aspat") ~ "aspatial",
        #     str_detect(accuracy_type, "spat") ~ "spatial")) %>%
        #   mutate(type_model = case_when(
        #     str_detect(accuracy_type, "_unit_pos") ~ "mapunit",
        #     str_detect(accuracy_type, "_meanacc") ~ "average")) %>%
        #   mutate(accuracy_type_label = case_when(
        #     str_detect(accuracy_type, "_p_") ~ "p",
        #     str_detect(accuracy_type, "_pa_") ~ "pa",
        #     str_detect(accuracy_type, "_fp_") ~ "fp",
        #     str_detect(accuracy_type, "_pf_") ~ "fp",
        #     str_detect(accuracy_type, "_fpa_") ~ "fpa")) %>%
        #   mutate(type_label = paste0(type, "_", type_model))
        # 
        # 
        # mu_unit <- mu_acc %>%
        #   filter(type_model == "mapunit") %>%
        #   dplyr::select(-c(acc_type, type_model ))
        # 
        # ## set up order for plots 
        # #bsRes_temp$type_f = factor(bsRes_temp$type_label, levels = #c("spatial_overall","aspatial_overall", "spatial_average", "aspatial_average"))
        # 
        # mu_unit$accuracy_type_label = factor(mu_unit$accuracy_type_label, levels = c("p","pa", "fp","fpa"))
        # 
        # p4 <- ggplot(aes(y = value, x = accuracy_type_label , fill = type), data = mu_unit  ) + 
        #   geom_boxplot() +
        #   facet_wrap(~target, scales = "free_x", nrow = 2) +
        #   ggtitle("Mapunit accuracy measures ") + 
        #   xlab("accuracy measure") + ylab("Proportion of Accurate calls") + 
        #   ylim(-0.05, 1)+
        #   theme_pem_facet()+ 
        #   scale_fill_discrete_sequential(palette = "Light Grays")
        # 
        # 
        # p4

}




# output plots as png
#install_github("bcgov/envreportutils")
#library(envreportutils)

multi_plot <- function(plotdata, filename){
  #  svg_px(paste0(filename, ".svg"), width = 400, height = 300)
  #  plot(plotdata)
  #  dev.off()
  png_retina(paste0(filename, ".png"), width = 700, height = 700,
             units = "px", type = "cairo-png", antialias = "default")
  plot(plotdata)
  dev.off()
}


