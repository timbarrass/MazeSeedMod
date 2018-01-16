package com.timbarrass.mods

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.util.Random

class PrimsMaze(width:Int, height:Int, scale: Int) {
  private val r = new Random
  private val neighbours = initNeighbours // neighbours is basically the static config that makes this a maze solver

  val finalMaze = process(randomCell(width, height))

  val finalGrid = transformToFinalGrid(finalMaze)



  def fullFinalGrid: Array[Array[Boolean]] = {
    Array.fill[Boolean](width * (scale + 1) + 1, height * (scale + 1) + 1) { true } // true => wall
  }

  private def emptyTree: Array[Array[mutable.Set[Cell]]] = {
    Array.fill[mutable.Set[Cell]](width, height) { Set() }
  }

  def transformToFinalGrid(finalMaze: Array[Array[mutable.Set[Cell]]]): Array[Array[Boolean]] = {
    transformToFinalGrid(finalMaze, fullFinalGrid)
  }

  def transformToFinalGrid(finalMaze: Array[Array[mutable.Set[Cell]]], finalGrid: Array[Array[Boolean]]): Array[Array[Boolean]] = {

    for (
      y <- 0 until height;
      x <- 0 until width
    ) {

      // clear out the cell first
      for (
        yStep <- 1 until scale + 1;
        xStep <- 1 until scale + 1
      ) {
        finalGrid(transform(x, scale, xStep))(transform(y, scale, yStep)) = false // false => no wall
      }

      for (n <- finalMaze(x)(y)) {
        // clear x-axis routes
        if (n.x == x) {
          val yWall = (scale + 1) * math.max(n.y, y)
          val xWallStart = (scale + 1) * n.x + 1

          for (xWall <- xWallStart until xWallStart + scale) {
            finalGrid(xWall)(yWall) = false
          }
        }

        // clear y-axis routes
        if (n.y == y) {
          val yWallStart = (scale + 1) * n.y + 1
          val xWall = (scale + 1) * math.max(n.x, x)

          for (yWall <- yWallStart until yWallStart + scale) {
            finalGrid(xWall)(yWall) = false
          }
        }
      }
    }

    // clear an entrance
    val entrance: Int = (scale + 1) * Random.nextInt(width) + 1
    for (x <- entrance until entrance + scale) {
      finalGrid(x)(0) = false
    }

    finalGrid
  }

  private def transform(x: Int, scale: Int, step: Int): Int = {
    (scale + 1) * x + step
  }

  def process(c: Cell) : Array[Array[mutable.Set[Cell]]] = {
    process(initGreyListNeighbours(c), initWhiteList(c))
  }

  def process(grey: mutable.Set[Cell], white: Array[Array[Boolean]]): Array[Array[mutable.Set[Cell]]] = {
    process(grey, white, emptyTree)
  }

  def process(grey: mutable.Set[Cell], white: Array[Array[Boolean]], tree: Array[Array[mutable.Set[Cell]]]): Array[Array[mutable.Set[Cell]]] =
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

      process(grey union neighbours(c.x)(c.y).filterNot( n => white(n.x)(n.y) ), white, tree)
    }
  }

  private def initNeighbours: Array[Array[mutable.Set[Cell]]] = {
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

  private def randomCell(width:Int, height:Int): Cell = {
    Cell(r.nextInt(width - 1), r.nextInt(height - 1))
  }

  private def initWhiteList(c: Cell): Array[Array[Boolean]] = {
    val white = Array.fill[Boolean](width, height) { false }

    white(c.x)(c.y) = true

    white
  }

  private def initGreyListNeighbours(c: Cell): mutable.Set[Cell] = {
    val grey = Set[Cell]()

    for (n <- neighbours(c.x)(c.y)) {
      if (!grey.contains(n)) {
        grey += n
      }
    }

    grey
  }
}