package com.timbarrass.mazes


object MazePublish {
  def display[T](grid: Array[Array[T]], lookup: (T) => Char): Unit = {
    for (
      y <- grid(0).indices;
      x <- grid.indices;
      _ = if (x == grid.length - 1) { println }
    ) {
      print(lookup(grid(x)(y)))
    }
    println
  }
}


