library(htmltools)
library(yaml)

# carousel displays a list of items w/ nav buttons
carousel <- function(id, duration, items) {
  index <- -1
  items <- lapply(items, function(item) {
    index <<- index + 1
    carouselItem(item$caption, item$image, item$link, index, duration)
  })
  
  items_div <- div(class = "carousel-inner",
                   tagList(lapply(items, function(item) item$item))
  )
  
  # Only return the image carousel — no indicators, no nav buttons
  div(id = id, class = "carousel carousel-dark carousel-fade", `data-bs-ride` = "carousel",
      items_div
  )
}

# carousel item
carouselItem <- function(caption, image, link, index, interval) {
  item <- div(
    class = paste0("carousel-item", ifelse(index == 0, " active", "")),
    `data-bs-interval` = interval,
    img(src = image, class = "d-block mx-auto")
  )
  list(item = item)
}

