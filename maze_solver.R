#Student Name
"Ahmed Abu Deiab"


### Libraries
library(magick)

### Read the image
d0 <- image_read(path = "images/wallpaper.png")

### Get only the relevant part
dims <- rev(dim(as.raster(d0)))
d1 <- image_crop(image = d0, geometry = "800x800+560+140")

### Quantizing
d2 <- as.raster(image_quantize(image_normalize(d1), max = 2))

### Changing to TRUE / FALSE matrix
### NOTE: TRUE = open/walkable space (background), FALSE = wall (the maze line)
d3 <- array(
  ifelse(as.vector(d2) == unique(as.vector(d2))[1], TRUE, FALSE),
  dim(d2)
)

### Saving
saveRDS(object = d3, file = "./maze.RDS")

### Reading in the maze
maze <- readRDS(file = "./maze.RDS")

### Plotting the raw maze
plot(as.raster(maze))


# ---------------------------------------------------------------------------
# Breadth-first search: find a path from the entrance to the Ubuntu logo
# ---------------------------------------------------------------------------
# maze[x, y] == TRUE  -> open space, can walk here
# maze[x, y] == FALSE -> wall (the printed maze line), cannot walk here
#
# The BFS is implemented with a preallocated integer queue (not a growing
# list) because the maze is 800x800 = 640,000 cells; a list-based queue
# that reallocates on every dequeue is far too slow at this scale.
# ---------------------------------------------------------------------------

findPath <- function(maze, startPoint, endRegion) {
  nr <- nrow(maze)
  nc <- ncol(maze)

  if (!maze[startPoint$x, startPoint$y]) {
    stop("Start point sits on a wall (FALSE) - not a valid entrance.")
  }

  visited   <- matrix(FALSE, nr, nc)
  parentIdx <- integer(nr * nc)   # 0 = no parent (used to reconstruct the path)

  toIdx <- function(x, y) (x - 1L) * nc + y

  queue <- integer(nr * nc)
  head  <- 1L
  tail  <- 1L

  startIdx <- toIdx(startPoint$x, startPoint$y)
  queue[tail] <- startIdx
  tail <- tail + 1L
  visited[startPoint$x, startPoint$y] <- TRUE

  dx <- c(-1L, 1L, 0L, 0L)   # up, down, left, right
  dy <- c(0L, 0L, -1L, 1L)

  foundIdx <- NA_integer_

  while (head < tail) {
    curIdx <- queue[head]
    head <- head + 1L

    cx <- ((curIdx - 1L) %/% nc) + 1L
    cy <- ((curIdx - 1L) %% nc) + 1L

    # Reached the target region (the logo)?
    if (cx >= endRegion$x[1] && cx <= endRegion$x[2] &&
        cy >= endRegion$y[1] && cy <= endRegion$y[2]) {
      foundIdx <- curIdx
      break
    }

    for (k in 1:4) {
      nx <- cx + dx[k]
      ny <- cy + dy[k]
      if (nx >= 1L && nx <= nr && ny >= 1L && ny <= nc &&
          maze[nx, ny] && !visited[nx, ny]) {
        visited[nx, ny] <- TRUE
        nIdx <- toIdx(nx, ny)
        parentIdx[nIdx] <- curIdx
        queue[tail] <- nIdx
        tail <- tail + 1L
      }
    }
  }

  if (is.na(foundIdx)) {
    return(list(found = FALSE, path = NULL, cellsVisited = tail - 1L))
  }

  # Walk parent pointers back from the target to the start to build the path
  path <- list()
  idx <- foundIdx
  repeat {
    x <- ((idx - 1L) %/% nc) + 1L
    y <- ((idx - 1L) %% nc) + 1L
    path[[length(path) + 1L]] <- c(x, y)
    if (idx == startIdx) break
    idx <- parentIdx[idx]
  }
  path <- rev(path)

  return(list(found = TRUE, path = path, cellsVisited = tail - 1L))
}


# ---- Run the search ----
startPoint <- list(x = 1, y = 1)
endRegion  <- list(x = c(387, 413), y = c(322, 348))

result <- findPath(maze, startPoint, endRegion)

if (result$found) {
  cat("A path from the outside to the logo was found.\n")
  cat("Path length (steps):", length(result$path), "\n")
  cat("Cells explored:", result$cellsVisited, "\n")
} else {
  cat("No path from the outside to the logo was found.\n")
  cat("Cells explored:", result$cellsVisited, "\n")
}


# ---- Visualize the path on top of the maze, and save it to file ----
if (result$found) {
  img <- array(1, dim = c(nrow(maze), ncol(maze), 3))  # white background
  wall_idx <- which(!maze, arr.ind = TRUE)
  for (ch in 1:3) img[cbind(wall_idx, ch)] <- 0         # black walls

  path_mat <- do.call(rbind, result$path)
  img[cbind(path_mat, 1)] <- 1   # red path: R=1
  img[cbind(path_mat, 2)] <- 0   #           G=0
  img[cbind(path_mat, 3)] <- 0   #           B=0

  plot(as.raster(img))

  png("images/maze_solution_path.png", width = 800, height = 800)
  plot(as.raster(img))
  dev.off()
}
