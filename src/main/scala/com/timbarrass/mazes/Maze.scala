package com.timbarrass.mazes

import scala.collection.mutable
import scala.util.Random

object Maze {

  // Generate a square-cell maze of specified size
  def generateMaze(width:Int, height:Int): Maze = {
    val neighbours = Neighbours(width, height)

    val c = randomCell(width, height)

    val routes = process(
      initGreyListNeighbours(c, neighbours),
      initWhiteList(c, width, height),
      emptyTree(width, height),
      neighbours)

    Maze(routes)
  }

  // Use a modified Prim's algorithm to generate a maze with a route to every cell
  private def process(grey: Vector[Cell],
                      white: Array[Array[Boolean]],
                      tree: Array[Array[mutable.Set[Cell]]],
                      neighbours: Neighbours): Array[Array[mutable.Set[Cell]]] =
  {
    if (grey.isEmpty) {
      tree
    } else {
      val c = grey(r.nextInt(grey.size - 1))

      white(c.x)(c.y) = true

      // Prim's algo proper would choose single closest node. In this modified algo
      // the closest nodes are the 4 cell neighbours, all 1 unit away -- so we
      // just randomly choose one unconsidered neighbour
      val ns = neighbours(c.x, c.y).filter(n => { white(n.x)(n.y) })
      val n = if ( ns.size > 1 ) ns(r.nextInt(ns.size - 1)) else ns(0)
      tree(c.x)(c.y) += n
      tree(n.x)(n.y) += c

      process(grey.filter(t => {t != c}) union neighbours(c.x, c.y).filterNot( n => white(n.x)(n.y) ), white, tree, neighbours)
    }
  }

  private def randomCell(width:Int, height:Int): Cell = {
    Cell(r.nextInt(width - 1), r.nextInt(height - 1))
  }

  private def emptyTree(width: Int, height: Int): Array[Array[mutable.Set[Cell]]] = {
    Array.fill[mutable.Set[Cell]](width, height) { mutable.Set() }
  }

  private def initWhiteList(c: Cell, width: Int, height: Int): Array[Array[Boolean]] = {
    val white = Array.fill[Boolean](width, height) { false }

    white(c.x)(c.y) = true

    white
  }

  private def initGreyListNeighbours(c: Cell, neighbours: Neighbours): Vector[Cell] = {
    neighbours(c.x, c.y)
  }

  private val r = new Random
}

// TODO: identify regions.
// Could identify regions at instantiation and provide a method for asking about them -- property of Cell perhaps?
// Would have to update regions when links were made or broken
// Go for region identification first, then breaking a link, then making a link
// For making -- express as joinRegions, and have both make and break return detail of link (Cell Pair)
// Then provide a perturb method -- have this method return the made and broken pairs so no event side-effect
// Blog those thoughts and choices (e.g. an event is a way of causing side effects ...)
// Blog moving to scalatest and the changes that have come about while looking at the code!
case class Maze(routes: Array[Array[mutable.Set[Cell]]]) {
  val r = new Random()

  val width: Int = routes.length
  val height: Int  = routes(0).length

  private val neighbours = Neighbours(width, height)

  val entrance: Int = if (width > 1) Random.nextInt(width - 1) else 0 // although 1 by anything maze is going to be relatively easy

  // instead of a raw collection present regions as a readonly custom class that we can write to
  var Regions = identifyRegions

  def regions(x: Int, y: Int): Int = Regions(x)(y)

  def apply(x: Int, y:Int): mutable.Set[Cell] = {
    routes(x)(y)
  }

  def breakALink: (Cell, Cell) = {
    val x = r.nextInt(width - 1)
    val y = r.nextInt(height - 1)

    val targetCell = routes(x)(y).head

    routes(x)(y).remove(targetCell)
    routes(targetCell.x)(targetCell.y).remove(Cell(x, y))

    Regions = identifyRegions

    Cell(x, y) -> targetCell
  }

  def joinRegions: (Cell, Cell) = {

    var grey: Vector[(Cell, Cell)] = Vector();

    for (
      y <- 0 until height;
      x <- 0 until width
    ) {
      val r1 = regions(x, y)
      neighbours(x, y)
        .filterNot(n => {regions(n.x, n.y) == r1})
        .foreach(n => { grey = grey :+ Cell(x, y) -> Cell(n.x, n.y)})
    }

    val p = grey(if(grey.size > 1) r.nextInt(grey.size - 1) else 0)
    routes(p._1.x)(p._1.y) += Cell(p._2.x, p._2.y)
    routes(p._2.x)(p._2.y) += Cell(p._1.x, p._1.y)

    Regions = identifyRegions

    grey(if(grey.size > 1) r.nextInt(grey.size - 1) else 0)
  }

  def identifyRegions: Array[Array[Int]] = {
    val setId = 0
    val empty = Array.fill[Int](width, height) {
      0
    }

    val grey = Vector[Cell]()

    var black = Vector[Cell]()
    Array.tabulate[Unit](width, height) { (x, y) => black = black :+ Cell(x, y) }

    fillRegion(grey, black, routes, setId, empty)
  }

  // remember you're cycling through two states here, black and grey
  // shift grey to a vector, black to a vector as before
  // tag cells rather than returning 2d array? presupposes a certain use, go with tests
  private def fillRegion(grey: Vector[Cell], black: Vector[Cell], tree:Array[Array[mutable.Set[Cell]]], setId: Int, flooded: Array[Array[Int]]): Array[Array[Int]] = {
    if(black.isEmpty && grey.isEmpty) return flooded

    if(grey isEmpty) {
      val c = if (black.size > 1) black(r.nextInt(black.size - 1)) else black(0)
      fillRegion(grey :+ c, black.filterNot(b => { b == c }), routes, setId + 1, flooded)
    } else {
      val c = if (grey.size > 1) grey(r.nextInt(grey.size - 1)) else grey(0)

      flooded(c.x)(c.y) = setId

      fillRegion((grey union (tree(c.x)(c.y).toVector intersect black)).filterNot(b => b == c), black diff tree(c.x)(c.y).toVector, tree, setId, flooded)
    }
  }
}


