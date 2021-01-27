# 提交成功显示
observeEvent(input$submit_qpcr, { 
  if (input$submit_qpcr>0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success")
  }
})
# 导入数据
# 运行正常20210111
user_data_qpcr <- reactive({
  table_in_test <- data.table::fread(input$data_input_qpcr$datapath, encoding = 'UTF-8') %>% 
    melt(id.vars = c('基因名称', '处理'))

  table_in_test <<- table_in_test
})

# 下载示例数据
# 运行正常20210111
output$download_demo_data_qpcr <- downloadHandler(
  filename = '示例数据.csv',
  content = function(file){
    file.copy('./demo_data/示例数据.csv',file)
  }
)



# 展示统计分析结果
output$taboutput_qpcr_view <- renderDataTable(df_table_qpcr(),
                                               options = list(
                                                 pageLength = 4
                                               ))

df_table_qpcr <- eventReactive(input$submit_qpcr,{
  if (input$submit_qpcr > 0) {
    
    df <- user_data_qpcr()
    
    res = NULL
    
    gene = unique(df$基因名称)
    
    for (i in gene) {
      if (i != input$reference_gene) {
        df1  = df[df$基因名称 == input$reference_gene,]
        df2 = df[df$基因名称 == i,]
        
        df3 = cbind(df1, df2)
        df3 = df3[,c(1,2,4,5,6,8)]
        colnames(df3) = c('内参基因','处理1','内参基因表达量','目的基因','处理2','目的基因表达量')
        df3$dd1 = df3$目的基因表达量 - df3$内参基因表达量
        
        mean1 = df1[df1$处理 == 'CK',]$value %>% mean() # 内参在CK中均值
        mean2 = df2[df2$处理 == 'CK',]$value %>% mean() # 内参在处理中的均值
        
        dd = mean2 - mean1
        df3$dd2 = -(df3$dd1 - dd)
        
        df3$fold_change = 2^(df3$dd2)
        
        lx = df3[df3$处理2 == 'CK',]
        mean(lx$fold_change)
        df3 = df3[,c('目的基因','处理2','fold_change')]
        df3$temp = paste(df3$目的基因, df3$处理2, sep = '_')
        
        mean = aggregate(df3$fold_change, by = list(df3$temp), FUN = mean)
        colnames(mean) = c('temp','mean')
        df3 = merge(df3, mean, by = 'temp')
        
        sd = aggregate(df3$fold_change, by = list(df3$temp), FUN = sd)
        colnames(sd) = c('temp','sd')
        df3 = merge(df3, sd, by = 'temp')
        
        lx = table(df3$temp) %>% as.data.frame()
        colnames(lx) = c('temp','freq')
        
        df3 = merge(df3, lx, by = 'temp')
        
        
        df3$se  = df3$sd/sqrt(df3$freq)
        df3 = df3[,-1]
        colnames(df3)[2] = '处理'
       
        
        # 统计检验
        if (length(unique(df3$处理)) > 2) {
          df3$处理 = as.factor(df3$处理)
          fit <- aov(fold_change ~ 处理, data = df3)
          pvalue <- summary(fit)[[1]][["Pr(>F)"]][1]
          
          # 判断是否进行多重校验
          if (pvalue < 0.05) {
            tuk <- glht(fit, linfct = mcp(处理 = 'Tukey'))
            sig <- cld(tuk, level = 0.95, decreasing = TRUE)[["mcletters"]][["Letters"]] %>%
              as.data.frame()
            colnames(sig) <- 'significance'
            sig$处理 <- rownames(sig)
            sig$pvalue <- pvalue
            
            df3 <- merge(df3, sig, by = '处理')
          }else{
            df3$pvalue <- pvalue
            df3$significance <- 'NS'
          }
        }else{
          fit <- t.test(fold_change ~ 处理, data = df3)
          df4 <- data.frame(pvalue = round(fit$p.value,4),
                            处理 = unique(df3$处理))
          df4$significance <- ifelse(df4$pvalue < 0.001,'***',
                                     ifelse(df4$pvalue > 0.001 & df4$pvalue < 0.01,'**',
                                            ifelse(df4$pvalue > 0.05,'NS','*')))
          
          df3 <- merge(df3, df4, by = '处理')
        }
        
        
        res = rbind(res, df3)
      }
      
    }
    
    
    res$temp = paste(res$目的基因, res$处理, sep = '_')
    res = res[!duplicated(res$temp),]
    res = dplyr::select(res, -'temp')
    res = dplyr::select(res, -'freq')
    res = dplyr::select(res, c('目的基因','处理','fold_change',
                               'mean','sd','se','pvalue','significance'))

    # 保存分析结果
    write.csv(res, file = './results/rest_tab.csv',row.names = FALSE)
    write.table(res, file = './results/rest_tab.txt',row.names = FALSE)
    xlsx::write.xlsx(res, file = './results/rest_tab.xlsx',row.names = FALSE)
    # 保存分析结果结束
  }
  res # 返回要展示的结果
})






# 进行绘图
output$plot_qpcr <- renderPlot(plot_res_qpcr(),
                                height = 300,
                                width = 600)
plot_res_qpcr <- eventReactive(input$submit_qpcr,{
  if (input$submit_qpcr > 0) {
    df <- user_data_qpcr()
    
    res = NULL
    
    gene = unique(df$基因名称)
    
    for (i in gene) {
      if (i != input$reference_gene) {
        df1  = df[df$基因名称 == input$reference_gene,]
        df2 = df[df$基因名称 == i,]
        
        df3 = cbind(df1, df2)
        df3 = df3[,c(1,2,4,5,6,8)]
        colnames(df3) = c('内参基因','处理1','内参基因表达量','目的基因','处理2','目的基因表达量')
        df3$dd1 = df3$目的基因表达量 - df3$内参基因表达量
        
        mean1 = df1[df1$处理 == 'CK',]$value %>% mean() # 内参在CK中均值
        mean2 = df2[df2$处理 == 'CK',]$value %>% mean() # 内参在处理中的均值
        
        dd = mean2 - mean1
        df3$dd2 = -(df3$dd1 - dd)
        
        df3$fold_change = 2^(df3$dd2)
        
        lx = df3[df3$处理2 == 'CK',]
        mean(lx$fold_change)
        df3 = df3[,c('目的基因','处理2','fold_change')]
        df3$temp = paste(df3$目的基因, df3$处理2, sep = '_')
        
        mean = aggregate(df3$fold_change, by = list(df3$temp), FUN = mean)
        colnames(mean) = c('temp','mean')
        df3 = merge(df3, mean, by = 'temp')
        
        sd = aggregate(df3$fold_change, by = list(df3$temp), FUN = sd)
        colnames(sd) = c('temp','sd')
        df3 = merge(df3, sd, by = 'temp')
        
        lx = table(df3$temp) %>% as.data.frame()
        colnames(lx) = c('temp','freq')
        
        df3 = merge(df3, lx, by = 'temp')
        
        
        df3$se  = df3$sd/sqrt(df3$freq)
        df3 = df3[,-1]
        colnames(df3)[2] = '处理'
       
        
        # 统计检验
        if (length(unique(df3$处理)) > 2) {
          df3$处理 = as.factor(df3$处理)
          fit <- aov(fold_change ~ 处理, data = df3)
          pvalue <- summary(fit)[[1]][["Pr(>F)"]][1]
          
          # 判断是否进行多重校验
          if (pvalue < 0.05) {
            tuk <- glht(fit, linfct = mcp(处理 = 'Tukey'))
            sig <- cld(tuk, level = 0.95, decreasing = TRUE)[["mcletters"]][["Letters"]] %>%
              as.data.frame()
            colnames(sig) <- 'significance'
            sig$处理 <- rownames(sig)
            sig$pvalue <- pvalue
            
            df3 <- merge(df3, sig, by = '处理')
          }else{
            df3$pvalue <- pvalue
            df3$significance <- 'NS'
          }
        }else{
          fit <- t.test(fold_change ~ 处理, data = df3)
          df4 <- data.frame(pvalue = round(fit$p.value,4),
                            处理 = unique(df3$处理))
          df4$significance <- ifelse(df4$pvalue < 0.001,'***',
                                     ifelse(df4$pvalue > 0.001 & df4$pvalue < 0.01,'**',
                                            ifelse(df4$pvalue > 0.05,'NS','*')))
          
          df3 <- merge(df3, df4, by = '处理')
        }
        
        
        res = rbind(res, df3)
      }
      
    }
    
    if (input$color == 'FALSE') {
      if (length(unique(res$目的基因)) > 1) {
        if (input$plot_style == 'box') {
          p = ggplot(res, aes(处理,fold_change)) +
            geom_boxplot(width = 0.5) +
            geom_text(aes(处理, max(fold_change)*1.1, label = significance)) +
            facet_grid(.~ 目的基因) +
            theme_bw()
        }else{
          res$temp = paste(res$目的基因, res$处理, sep = '_')
          res = res[!duplicated(res$temp),]
          res = dplyr::select(res, -'temp')
          
          if (input$bar_style == 'sd') {
            p = ggplot(res,aes(处理, mean)) +
              geom_bar(stat = 'identity', width = 0.5) +
              facet_grid(. ~ 目的基因) +
              geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
              geom_text(aes(处理, max(mean + sd)*1.1, label = significance)) +
              geom_hline(yintercept = (max(res$mean) + max(res$sd))*1.2, color = 'white') +
              scale_y_continuous(expand = c(0,0)) +
              theme_bw()
          }else{
            p = ggplot(res,aes(处理, mean)) +
              geom_bar(stat = 'identity', width = 0.5) +
              facet_grid(. ~ 目的基因) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
              geom_text(aes(处理, max(mean + se)*1.1, label = significance)) +
              geom_hline(yintercept = (max(res$mean) + max(res$se))*1.2, color = 'white') +
              scale_y_continuous(expand = c(0,0)) +
              theme_bw()
          }
        }
        
      }else{
        if (input$plot_style == 'box') {
          p = ggplot(res, aes(处理,fold_change)) +
            geom_boxplot(width = 0.5) +
            geom_text(aes(处理, max(fold_change)*1.1, label = significance)) +
            theme_bw()
        }else{
          res$temp = paste(res$目的基因, res$处理, sep = '_')
          res = res[!duplicated(res$temp),]
          res = dplyr::select(res, -'temp')
          
          if (input$bar_style == 'sd') {
            p = ggplot(res,aes(处理, mean)) +
              geom_bar(stat = 'identity', width = 0.5) +
              geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
              geom_text(aes(处理, max(mean + sd)*1.1, label = significance)) +
              geom_hline(yintercept = (max(res$mean) + max(res$sd))*1.2, color = 'white') +
              scale_y_continuous(expand = c(0,0)) +
              theme_bw()
          }else{
            p = ggplot(res,aes(处理, mean)) +
              geom_bar(stat = 'identity', width = 0.5) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
              geom_text(aes(处理, max(mean + se)*1.1, label = significance)) +
              geom_hline(yintercept = (max(res$mean) + max(res$se))*1.2, color = 'white') +
              scale_y_continuous(expand = c(0,0)) +
              theme_bw()
          }
        }
      }
    }else{
      if (length(unique(res$目的基因)) > 1) {
        if (input$plot_style == 'box') {
          p = ggplot(res, aes(处理,fold_change, fill = 处理)) +
            geom_boxplot(width = 0.5) +
            geom_text(aes(处理, max(fold_change)*1.1, label = significance)) +
            facet_grid(. ~ 目的基因) +
            scale_fill_aaas() +
            theme_bw()
        }else{
          res$temp = paste(res$目的基因, res$处理, sep = '_')
          res = res[!duplicated(res$temp),]
          res = dplyr::select(res, -'temp')
          
          if (input$bar_style == 'sd') {
            p = ggplot(res,aes(处理, mean, fill = 处理)) +
              geom_bar(stat = 'identity', width = 0.5) +
              facet_grid(. ~ 目的基因) +
              geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
              geom_text(aes(处理, max(mean + sd)*1.1, label = significance)) +
              geom_hline(yintercept = (max(res$mean) + max(res$sd))*1.2, color = 'white') +
              scale_y_continuous(expand = c(0,0)) +
              scale_fill_aaas() +
              theme_bw()
          }else{
            p = ggplot(res,aes(处理, mean, fill = 处理)) +
              geom_bar(stat = 'identity', width = 0.5) +
              facet_grid(. ~ 目的基因) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
              geom_text(aes(处理, max(mean + se)*1.1, label = significance)) +
              geom_hline(yintercept = (max(res$mean) + max(res$se))*1.2, color = 'white') +
              scale_y_continuous(expand = c(0,0)) +
              scale_fill_aaas() +
              theme_bw()
        }
      }
      }else{
        
        if (input$plot_style == 'box') {
          p = ggplot(res, aes(处理,fold_change, fill = 处理)) +
            geom_boxplot(width = 0.5) +
            geom_text(aes(处理, max(fold_change)*1.1, label = significance)) +
            scale_fill_aaas() +
            theme_bw()
        }else{
          res$temp = paste(res$目的基因, res$处理, sep = '_')
          res = res[!duplicated(res$temp),]
          res = dplyr::select(res, -'temp')
          
          if (input$bar_style == 'sd') {
            p = ggplot(res,aes(处理, mean, fill = 处理)) +
              geom_bar(stat = 'identity', width = 0.5) +
              geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
              geom_text(aes(处理, max(mean + sd)*1.1, label = significance)) +
              geom_hline(yintercept = (max(res$mean) + max(res$sd))*1.2, color = 'white') +
              scale_y_continuous(expand = c(0,0)) +
              scale_fill_aaas() +
              theme_bw()
          }else{
            p = ggplot(res,aes(处理, mean, fill = 处理)) +
              geom_bar(stat = 'identity', width = 0.5) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
              geom_text(aes(处理, max(mean + se)*1.1, label = significance)) +
              geom_hline(yintercept = (max(res$mean) + max(res$se))*1.2, color = 'white') +
              scale_y_continuous(expand = c(0,0)) +
              scale_fill_aaas() +
              theme_bw()
          }
        }
    }}
    
    
    

    p = p + 
      labs(x = input$qpcr_fig_x_axis,
           y = input$qpcr_fig_y_axis,
           title = input$qpcr_fig_title) +
      theme_bw(base_family = "STKaiti") +
      theme(legend.position = 'none',
            legend.text = element_text(color = 'black',size = 10),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_text(color = 'black',size = 10),
            axis.title = element_text(color = 'black',size = 10),
            axis.ticks = element_line(color = 'black'),
            axis.ticks.length.x = unit(0,'cm'))
    
    p_qpcr <- p
    
  
    
    # 保存图片
    filename <- ifelse(input$qpcr_fig_res_filetype == '.pdf','res_fig.pdf',
                       ifelse(input$qpcr_fig_res_filetype == '.png','res_fig.png',
                              ifelse(input$qpcr_fig_res_filetype == '.jpg','res_fig.jpg',
                                     ifelse(input$qpcr_fig_res_filetype == '.tiff','res_fig.tiff','res_fig.eps'))))
    
    if (input$qpcr_fig_res_filetype == '.pdf') {
      ggsave(p_qpcr, 
             filename = paste('./results/', filename, sep = ''),
             width = input$qpcr_fig_wdith,
             height = input$qpcr_fig_height,
             device = cairo_pdf)
    }else{
      ggsave(p_qpcr, 
             filename = paste('./results/', filename, sep = ''),
             width = input$qpcr_fig_wdith,
             height = input$qpcr_fig_height)
    }
  }
  p_qpcr # 返回图
})
# 下载分析结果
# 运行正常20210111
output$table_download <- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_qpcr$name,
                           1,
                           (nchar(input$data_input_qpcr$name) - 4)),
          '_统计分析结果',input$qpcr_stat_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$qpcr_stat_res_filetype == '.csv') {
      file.copy('./results/rest_tab.csv',file)
    }else if (input$qpcr_stat_res_filetype == '.txt') {
      file.copy('./results/rest_tab.txt',file)
    }else{
      #return(NULL)
      file.copy('./results/rest_tab.xlsx',file)
    }
  }
)

# 下载图片
output$figure_download<- downloadHandler(
  filename <- function(){
    paste(stringr::str_sub(input$data_input_qpcr$name,
                           1,
                           (nchar(input$data_input_qpcr$name) - 4)),
          '_绘图结果',input$qpcr_fig_res_filetype,sep = ''
    )
  },
  content <- function(file){
    if (input$qpcr_fig_res_filetype == '.pdf') {
      file.copy('./results/res_fig.pdf',file)
    }else if (input$qpcr_fig_res_filetype == '.png') {
      file.copy('./results/res_fig.png',file)
    }else if (input$qpcr_fig_res_filetype == '.jpg') {
      file.copy('./results/res_fig.jpg',file)
    }else if (input$qpcr_fig_res_filetype == '.tiff') {
      file.copy('./results/res_fig.tiff',file)
    }else{
      file.copy('./results/res_fig.eps',file)
    }
  }
)