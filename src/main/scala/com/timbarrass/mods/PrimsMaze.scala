package com.timbarrass.mods

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.Random

case class Cell(xIn:Int, yIn: Int) {
  val x = xIn
  val y = yIn
}

class PrimsMaze(w:Int, h:Int, scale: Int) {
  val r = new Random

  val width  = w
  val height = h

  val black = Array.fill[String](width, height) { "#" }

  val white = Array.fill[Boolean](width, height) { false }

  val grey = Set[Cell]()

  val neighbours = Array.fill[mutable.Set[Cell]](width, height) { Set() }
  val tree = Array.fill[mutable.Set[Cell]](width, height) { Set() }

  for ( x <- 0 to width - 1 ) {
    for ( y <- 0 to height - 1 ) {
      if (x > 0)          neighbours(x)(y) += Cell(x-1, y)
      if (x < width - 1)  neighbours(x)(y) += Cell(x+1, y)
      if (y > 0)          neighbours(x)(y) += Cell(x, y-1)
      if (y < height - 1) neighbours(x)(y) += Cell(x, y+1)
    }
  }

  chooseFirstCell

  while ( grey.size > 0 ) {
    // choose a random grey cell
    Random.shuffle(grey) // ha so efficient
    val c = grey.head // random?
    grey -= c

    // move cell into white set
    white(c.x)(c.y) = true
    black(c.x)(c.y) = " "

    // choose a neighbour of cell that's in white to go into tree
    // this is where Prim proper would use a shortest metric to choose
    // depending and random iteration through set here ...
    tree(c.x)(c.y) += neighbours(c.x)(c.y).filter(n => { if (white(n.x)(n.y)) { true } else { false }}).head

    // shift neighbours of new white cell into grey set
    for ( n <- neighbours(c.x)(c.y)) {
      if ( ! white(n.x)(n.y) && ! grey.contains(n) ) {
        grey += n
        black(n.x)(n.y) = "F"
      }
    }
  }

  def transform(x: Int, scale: Int, step: Int): Int = {
    (scale + 1) * x + step
  }

  val finalGrid = Array.fill[String](width * (scale + 1) + 1, height * (scale + 1) + 1) { "#" }

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

    for ( n <- tree(x)(y) ) {
      if ( n.x == x ) {
        val yWall = (scale + 1) * math.max(n.y, y)
        val xWallStart = (scale + 1) * n.x + 1

        for ( xWall <- xWallStart until xWallStart + scale ) {
          finalGrid(xWall)(yWall) = " "
        }
      }

      if( n.y == y ) {
        val yWallStart = (scale + 1) * n.y + 1
        val xWall = (scale + 1) * math.max(n.x, x)

        for ( yWall <- yWallStart until yWallStart + scale ) {
          finalGrid(xWall)(yWall) = " "
        }
      }
    }
  }

  val entrance:Int = (scale + 1) * Random.nextInt(width) + 1
  val exit:Int = (scale + 1) * Random.nextInt(width) + 1
  for ( x <- entrance until entrance + scale) {
    finalGrid(x)(0) = " "
  }
  for ( x <- exit until exit + scale) {
    finalGrid(x)(height) = " "
  }

  private def chooseFirstCell: Unit = {

    var c = Cell(r.nextInt(width - 1), r.nextInt(height - 1))

    black(c.x)(c.y) = " "

    white(c.x)(c.y) = true

    for (n <- neighbours(c.x)(c.y)) {
      if (!grey.contains(n)) {
        grey += n
        black(n.x)(n.y) = "F"
      }
    }
  }
}