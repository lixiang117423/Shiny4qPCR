# 定义t-test选项卡开始----------------------------------------------------------
fluidRow(
  # 使用box后就不能再用column
  # 第一个box
  column(width = 2,
         box(width = NULL,
             height = 875,
             title = '数据上传与主要设置',
             status = "danger", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = "navy",
             #"随便打的文本", # 直接插入文本
             #br(), # 换行符
             # 上传数据
             fileInput("data_input_qpcr",
                       label = h4("上传数据"),
                       accept = ".csv",
                       buttonLabel = "浏览..."),
             # 下载示例数据
             downloadLink('download_demo_data_qpcr',
                          label = h4('下载示例数据')),
             
             # 内参基因名称
             textInput('reference_gene',
                       label = h4('内参基因名称'),
                       value = 'A'),
             # 生物学重复数量
             sliderInput('bio_num',
                         label = h4('生物学重复数量'),
                         min = 1, max = 10,
                         step = 1, value = 3),
             # 技术学重复数量
             sliderInput('tech_num',
                         label = h4('技术重复数量'),
                         min = 1, max = 10,
                         step = 1, value = 3),
             
             
             # 是否绘图
             selectInput('plot_style',
                         label = h4('绘图样式'),
                         choices = list('柱状图' = 'bar',
                                        '箱线图' = 'box'),
                         selected = 'bar'),
             # 是否绘图
             selectInput('bar_style',
                         label = h4('误差线'),
                         choices = list('标准差' = 'sd',
                                        '标准误' = 'se'),
                         selected = 'se'),
             # 点击提交
             actionButton('submit_qpcr',
                          label = h4('点击提交'),
                          width = 230,
                          icon = NULL),
             br(),
             br(),
             
             fluidRow(
               column(width = 2,
                      downloadButton('table_download',
                                     label = h4('下载结果'),
                                     width = 230)),
               
               column(width = 2,
                      offset = 4,# 偏移量
                      downloadButton('figure_download',
                                     label = h4('下载图片'),
                                     width = 230))
             )
         )),# 第一个box完结
  
  # 第二列
  column(width = 6,
         # 第二列上面的box展示统计分析结果
         box(width = NULL,
             height = 405,
             title = '分析结果预览',
             status = "warning", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = NULL,
             #"随便打的文本", # 直接插入文本
             br(),
             dataTableOutput('taboutput_qpcr_view')),
         
         # 第二列下面的box下载统计分析参数
         box(width = NULL,
             height = 450,
             title = '下载参数设置',
             status = "info", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = 'navy',
             #"随便打的文本", # 直接插入文本
             #br(),
             selectInput('qpcr_stat_res_filetype',
                         label = h4('选择数据下载格式'),
                         choices = list('Excel文件' = '.xlsx',
                                        'txt文件' = '.txt',
                                        'csv文件' = '.csv'),
                         selected = '.xlsx'),
             # 图片格式
             selectInput('qpcr_fig_res_filetype',
                         label = h4('选择图片下载格式'),
                         choices = list('PDF文件' = '.pdf',
                                        'PNG文件' = '.png',
                                        'JPG文件' = '.jpg',
                                        'TIFF文件' = '.tiff',
                                        'EPS文件' = '.eps'),
                         selected = '.pdf'),
             
             # 图片宽度
             sliderInput('qpcr_fig_wdith',
                         label = '图片宽度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5),
             
             # 图片高度
             sliderInput('qpcr_fig_height',
                         label = '图片高度（单位：cm）',
                         min = 1,max = 20,
                         value = 5,step = 0.5))
  ),
  
  # 第三列
  column(width = 4,
         # 第一个box用于绘图
         box(width = NULL,
             height = 405,
             title = '绘图结果展示',
             status = "success", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             #background = "navy",
             #"随便打的文本", # 直接插入文本
             br(),
             plotOutput('plot_qpcr')),
         
         # 第二个box用于下载图片
         box(width = NULL,
             height = 450,
             title = '绘图参数设置',
             status = "primary", 
             solidHeader = TRUE,
             collapsible = TRUE, 
             background = 'navy',
             selectInput('color',
                         label = h4('是否彩色'),
                         choices = list('是' = 'TRUE',
                                        '否' = 'FALSE'),
                         selected = 'TRUE'),
             textInput('qpcr_fig_x_axis',
                       label = h4('输入X轴标题')),
             textInput('qpcr_fig_y_axis',
                       label = h4('输入Y轴标题')),
             textInput('qpcr_fig_title',
                       label = h4('输入图片标题'))
  )
))
