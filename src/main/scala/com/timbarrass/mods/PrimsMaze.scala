package com.timbarrass.mods

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.Random

class PrimsMaze(w:Int, h:Int, scale: Int) {
  val r = new Random

  val width  = w
  val height = h

  val white = Array.fill[Boolean](width, height) { false }
  var grey = Set[Cell]()

  val neighbours = initNeighbours

  chooseFirstCell

  val tree = process(grey, Array.fill[mutable.Set[Cell]](width, height) { Set() })

  transformToFinalGrid



  def transformToFinalGrid: Unit = {
    val finalGrid = Array.fill[String](width * (scale + 1) + 1, height * (scale + 1) + 1) {
      "#"
    }

    for (
      y <- 0 until height;
      x <- 0 until width
    ) {
      for (
        yStep <- 1 until scale + 1;
        xStep <- 1 until scale + 1
      ) {
        finalGrid(transform(x, scale, xStep))(transform(y, scale, yStep)) = " "
      }

      for (n <- tree(x)(y)) {
        if (n.x == x) {
          val yWall = (scale + 1) * math.max(n.y, y)
          val xWallStart = (scale + 1) * n.x + 1

          for (xWall <- xWallStart until xWallStart + scale) {
            finalGrid(xWall)(yWall) = " "
          }
        }

        if (n.y == y) {
          val yWallStart = (scale + 1) * n.y + 1
          val xWall = (scale + 1) * math.max(n.x, x)

          for (yWall <- yWallStart until yWallStart + scale) {
            finalGrid(xWall)(yWall) = " "
          }
        }
      }
    }

    val entrance: Int = (scale + 1) * Random.nextInt(width) + 1
    val exit: Int = (scale + 1) * Random.nextInt(width) + 1
    for (x <- entrance until entrance + scale) {
      finalGrid(x)(0) = " "
    }
    for (x <- exit until exit + scale) {
      finalGrid(x)(height) = " "
    }
  }

  def transform(x: Int, scale: Int, step: Int): Int = {
    (scale + 1) * x + step
  }

  def process(grey: mutable.Set[Cell], tree: Array[Array[mutable.Set[Cell]]]): Array[Array[mutable.Set[Cell]]] =
  {
    if (grey.isEmpty) {
      tree
    } else {
      Random.shuffle(grey) // ha so efficient
      val c = grey.head // random?
      grey -= c

      white(c.x)(c.y) = true

      // Prim's algo proper would choose single closest node. In this modified algo
      // the closest nodes are the 4 cell neighbours, all 1 unit away -- so we
      // just randomly choose one unconsidered neighbour
      tree(c.x)(c.y) += neighbours(c.x)(c.y).filter(n => { white(n.x)(n.y) }).head

      process(grey union neighbours(c.x)(c.y).filterNot( n => white(n.x)(n.y) ), tree)
    }
  }

  def initNeighbours: Array[Array[mutable.Set[Cell]]] = {
    val neighbours = Array.fill[mutable.Set[Cell]](width, height) {
      Set()
    }

    for (
      x <- 0 until width;
      y <- 0 until height
    ) {
      if (x > 0) neighbours(x)(y) += Cell(x - 1, y)
      if (x < width - 1) neighbours(x)(y) += Cell(x + 1, y)
      if (y > 0) neighbours(x)(y) += Cell(x, y - 1)
      if (y < height - 1) neighbours(x)(y) += Cell(x, y + 1)
    }

    neighbours
  }


  private def chooseFirstCell: Unit = {

    val c = Cell(r.nextInt(width - 1), r.nextInt(height - 1))

    white(c.x)(c.y) = true

    for (n <- neighbours(c.x)(c.y)) {
      if (!grey.contains(n)) {
        grey += n
      }
    }
  }
}