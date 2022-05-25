library(scales)
library(nseval)
library(ggmosaic)





# plot.1 distribution by Age - bar plot
# plot.2 distribution by Balance - bar plot
# plot.3 distribution by gender - pie plot
# distribution by job classification - mosaic plot

  
  # 3.0 create bin for 
  #dplyr::mutate(balance.bin=cut(Balance, seq(min(Balance), max(Balance) + 4, 5), right = FALSE)) %>% 
  #dplyr::select(balance.bin) %>% unique()



###################################################################################################
# map function (aggregation variable, plot type)
##################################################################################################

# aggregate by age.bin and balance.bin
simple_paste<-function(...){
  
 purrr::map_chr(dplyr::enquos(...),rlang::as_label ) %>%
    paste0()
}





update_plots<-function(temp=df.filter, regional.filter=c() ){


  temp<-temp %>%
    dplyr::filter(Region %in% regional.filter) %>% 
    
    # transform the data to create bins for 1_ age and 2-income
    # 1.0 create bin for age
    dplyr::mutate(age.bin=cut(Age, seq(min(Age), max(Age) + 4, 5), right = FALSE)) %>% 
    #dplyr::select(age.bin) %>% unique()
    # 2.0 create bin for Balance - Divide by 1000
    #dplyr::mutate(balance.bin=cut(Balance, seq(min(Balance), max(Balance) + 4, 5), right = FALSE)) %>%
    dplyr::mutate(balance.bin=cut(Balance/1000, pretty(Balance/1000,15),right = FALSE ) )# %>%

  #dplyr::mutate_at(dplyr::vars(balance.bin), function(x) as.character(paste0(as.character(x)," K") )  )
  
  

graph.list<-list("Balance"=list("type"=c("bar"),"Title"="Distribution of Balance","grp.var"=dplyr::quo(balance.bin) ),
                 "Age"=list("type"=c("bar"),"Title"="Distribution of Age Groups","grp.var"=dplyr::quo(age.bin) ),
                 "Gender"=list("type"=c("pie"),"Title"="Distribution of Gender","grp.var"=dplyr::quo(Gender)),
                 "Job"=list("type"=c("mosaic-plot"),"Title"="Distribution of JOb Classifications","grp.var"=dplyr::quo(Gender)),
                 "count"=list("type"=c("count"),"Title"=NULL,"grp.var"=NULL )
                
                  )

graph.df_list<- purrr::map(.x=graph.list,
                       .f=~{
                     
                         grp.var=.x$grp.var
                         plot.type=.x$type
                         graph.title=.x$Title
                    
                         print(dplyr::quo_name(grp.var))
                         
                         vars<-simple_paste(grp.var)
                         
                         #generate aggregate table
                         tbl<-temp %>%
                           dplyr::select(!!grp.var) %>%
                           dplyr::group_by(!!grp.var) %>%
                           dplyr::summarise(count=dplyr::n() ) %>%
                           dplyr::mutate(pct = prop.table(count))
                         
                         #ggplot based on plot type
                         
                         if(plot.type=="bar"){
                           plot<-ggplot2::ggplot(tbl,ggplot2::aes(x=!!grp.var ,y=count)) +
                             ggplot2::geom_col(fill="skyblue",position = 'dodge') +
                             geom_text(aes(label = scales::percent(pct,2)), 
                                       position = position_dodge(width = .9),
                                       vjust = -0.5,
                                       size = 3) +
                             # Title
                             ggplot2::labs(title = graph.title) +
                             
                             #ggplot2::scale_y_continuous(labels = scales::percent(pct)))+
                             ggplot2::theme_minimal()+
                             ggplot2::theme(axis.text=element_text(size=7),
                                            axis.title=element_text(size=14,face="bold"))
                           
                         } else if(plot.type=="pie"){
                          
                           tbl <- tbl %>% 
                             #dplyr::arrange(desc(grp.var)) %>%
                             #mutate(prop = value / sum(data$value) *100) %>%
                             dplyr::mutate(lables=scales::percent(pct))
                             #dplyr::mutate(ypos = cumsum(pct)- 0.5*pct )
                           
                           
                           plot<-ggplot(tbl, aes(x = "", y = pct, fill = !!grp.var)) +
                             geom_col(color = "black") +
                             geom_label(aes(label = lables),
                                        position = position_stack(vjust = 0.5),
                                        show.legend = FALSE) +
                             guides(fill = guide_legend(title = graph.title )) +
                             scale_fill_brewer(palette="Set1")+
                             coord_polar(theta = "y") + 
                             theme_void()
                           
                           
                         } else if(plot.type=="mosaic-plot"){
                           
                           plot<- ggplot(temp%>% dplyr::mutate_at(vars(Gender,Job.Classification), function(x) as.factor(x)  )) +
                              
                             ggmosaic::geom_mosaic(aes(x=product(Gender,Job.Classification ), fill = Job.Classification ) )  + 
                             scale_alpha_manual(values =c(.7,.9)) +
                             theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
                             labs(y="Gender", x="Job Classification", title = "Relationship between Gender and JOb classfication")
                           
                         } else if (plot.type=="count"){
                           plot<-temp %>%
                             dplyr::summarise(count=dplyr::n()) %>%
                             dplyr::select(count) %>% dplyr::pull()
                         }
                         
                         plot
                       }
)%>% #map2 %>%
purrr::set_names(c("Balance", "Age", "Gender", "Job", "count"))

} #end of function


