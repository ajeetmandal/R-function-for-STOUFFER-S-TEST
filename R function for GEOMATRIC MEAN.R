#=================== GEOMATRIC MEAN function =========================================
geo.mean <- function(filtered_data){
  prod(filtered_data)^(1/length(filtered_data))
}
