# Colors

get_plot_clrs <- function() {
  plot_clrs <- list()
  
  plot_clrs$bg <- c('#0e3338')
  # other colours considered for background:
  # '#19292B','#1E3538'
  
  plot_clrs$rays <- c('#F0E7C9') 

  plot_clrs$margins <- c('#F0EFEB')
  
  return(plot_clrs)

}

get_polygon_clrs <- function() {
  clrs <- list()
  # education
  # clrs[['education']] <- c(
  #   '#392566',
  #            '#551f56',
  #            '#631e45',
  #            '#682536',
  #            '#662f2c'
  # )
  clrs[['education']] <- c(
    '#392566',
    '#5b215b',
    '#6f234e',
    '#7b2c41',
    '#803a37'
  )
  
  # environment
  clrs[['environment']] <- c(
    '#243f59',
             '#004a61',
             '#005460',
             '#005d55',
             '#126442'
  )
  
  # health
  clrs[['health']] <- c(
    '#312056',
    '#233569',
    '#0b4777',
    '#005980',
    '#0b6a85'
  )
  
  # infrastructure and sustainability
  clrs[['sustainability']] <- c(
    '#274519',
             '#3d4f1c',
             '#525820',
             '#696127',
             '#806930'
  )
  
  # rights
  clrs[['rights']] <- c(
    '#1b3659',
    '#333761',
    '#4b3665',
    '#623464',
    '#78315e')
    
  # voices
  clrs[['voices']] <- c(
    '#662f2c',
             '#713627',
             '#783f20',
             '#7c4b17',
             '#7d570b'
  )
  
  return(clrs)
}