package com.timbarrass.mazes

import scala.reflect.ClassTag

object MazeTransform {
  private def transform(x: Int, scale: Int, step: Int): Int = {
    (scale + 1) * x + step
  }

  // initial value
  // pass x, y and return value
  // generic template for final returned 2d array

  def transformToGrid[T:ClassTag](m: Maze, scale: Int, wallValue: T, entranceValue: T, lookup: (Int, Int) => T): Array[Array[T]] = {

    val grid = Array.fill[T](transform(m.width, scale, 1), transform(m.height, scale, 1)) { wallValue } // true => wall

    for (
      y <- 0 until m.height;
      x <- 0 until m.width
    ) {

      // carve out the cell first
      for (
        yStep <- 1 until scale + 1;
        xStep <- 1 until scale + 1
      ) {
        grid(transform(x, scale, xStep))(transform(y, scale, yStep)) = lookup(x, y) // false => no wall
      }

      for (n <- m.routes(x)(y)) {
        // clear x-axis routes
        if (n.x == x) {
          val yWall = transform(math.max(n.y, y), scale, 0)
          val xWallStart = transform(n.x, scale, 1)

          for (xWall <- xWallStart until xWallStart + scale) {
            grid(xWall)(yWall) = lookup(x, y)
          }
        }

        // clear y-axis routes
        if (n.y == y) {
          val yWallStart = transform(n.y, scale, 1)
          val xWall = transform(math.max(n.x, x), scale, 0)

          for (yWall <- yWallStart until yWallStart + scale) {
            grid(xWall)(yWall) = lookup(x, y)
          }
        }
      }
    }

    // clear an entrance
    for (
      xStep <- 1 until scale + 1
    ) {
      grid(transform(m.entrance, scale, xStep))(transform(0, scale, 0)) = entranceValue // false => no wall
    }

    grid
  }
}
