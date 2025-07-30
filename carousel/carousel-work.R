library(htmltools)
library(dplyr)

# Top-level carousel function
carousel <- function(id, duration, items) {
  index <- -1
  items <- lapply(items, function(item) {
    index <<- index + 1
    carouselItem(item$caption, item$image, item$link, index, duration, id)
  })
  
  # indicators go *after* items so they appear below the image
  inner_items <- div(class = "carousel-inner",
                     tagList(lapply(items, function(item) item$item))
  )
  
  indicators <- div(class = "carousel-indicators",
                    tagList(lapply(items, function(item) item$button))
  )
  
  # Final carousel container
  div(id = id, class = "carousel carousel-dark slide", `data-bs-ride` = "carousel",
      inner_items,
      indicators
  )
}

# Individual carousel item generator
carouselItem <- function(caption, image, link, index, interval, carousel_id) {
  # Button for indicator
  button <- tags$button(
    type = "button", 
    `data-bs-target` = paste0("#", carousel_id),
    `data-bs-slide-to` = index,
    `aria-label` = paste("Slide", index + 1)
  )
  
  if (index == 0) {
    button <- tagAppendAttributes(button,
                                  class = "active",
                                  `aria-current` = "true"
    )
  }
  
  # Carousel item content (non-clickable image + caption)
  item <- div(class = paste0("carousel-item", ifelse(index == 0, " active", "")),
              `data-bs-interval` = interval,
              img(src = image, class = "d-block mx-auto border"),
              div(class = "carousel-caption text-center",
                  tags$p(class = "fw-light", caption)
              )
  )
  
  list(
    button = button,
    item = item
  )
}
