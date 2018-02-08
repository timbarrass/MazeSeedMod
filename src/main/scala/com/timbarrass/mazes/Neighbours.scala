package com.timbarrass.mazes

case class Neighbours(width:Int, height:Int) {
  private val neighbours = Array.fill[Vector[Cell]](width, height) {
    Vector[Cell]()
  }

  for (
    x <- 0 until width;
    y <- 0 until height
  ) {
    if (x > 0) neighbours(x)(y) = neighbours(x)(y) :+ Cell(x - 1, y)
    if (x < width - 1) neighbours(x)(y) = neighbours(x)(y) :+ Cell(x + 1, y)
    if (y > 0) neighbours(x)(y) = neighbours(x)(y) :+ Cell(x, y - 1)
    if (y < height - 1) neighbours(x)(y) = neighbours(x)(y) :+ Cell(x, y + 1)
  }

  def apply(x:Int, y:Int): Vector[Cell] = {
    neighbours(x)(y)
  }
}

