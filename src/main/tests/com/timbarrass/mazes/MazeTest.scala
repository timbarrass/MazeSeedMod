package com.timbarrass.mazes

import org.scalatest.{FlatSpec, MustMatchers}

import scala.collection.mutable

class MazeTest extends FlatSpec with MustMatchers {

  behavior of "Maze"

  "generateMaze" must "return a new Maze when requested" in {
    Maze.generateMaze(3, 4) mustBe an[Maze]
  }

  it must "return a Maze with specified width and height" in {
    val m = Maze.generateMaze(3, 4)
    m.width must equal(3)
    m.height must equal(4)
  }

  "maze" should "return a set of neighbour cells for a given cell" in {
    val m = Maze.generateMaze(4, 4)
    m(2, 3) mustBe an[mutable.Set[_]]
  }

  it must "have at least one route out of each cell" in {
    val m = Maze.generateMaze(4, 4)
    for (
      y <- 0 until m.height;
      x <- 0 until m.width
    ) {
      m(x, y).size must be > 0
    }
  }

  it must "have a matching route in for each route out of a cell" in {
    val m = Maze.generateMaze(4, 4)
    for (
      y <- 0 until m.height;
      x <- 0 until m.width
    ) {
      val me = Cell(x, y)
      m(x, y).foreach(n => {
        m(n.x, n.y) must contain(me)
      })
    }
  }

  it must "have an entrance" in {
    val routes = Array.fill[mutable.Set[Cell]](2, 2) { mutable.Set() }
    routes(0)(0) += Cell(1, 0)
    routes(1)(0) += Cell(0, 0)
    routes(0)(1) += Cell(1, 1)
    routes(1)(1) += Cell(0, 1)

    val m = new Maze(routes)

    if (m.entrance < 0 || m.entrance > 1) fail
  }


  // now I can't tell that it's actually created a maze ...
  // what's the definition of a maze? There's a route from every cell to every other cell?
  // the average distance from cell to cell is greater than the shorted right-angle route distance?

  "initNeighbours" must "return 4 neighbours for an internal cell" in {
    val n = Neighbours(6, 6)
    n(3, 4) must contain(Cell(3, 5))
    n(3, 4) must contain(Cell(2, 4))
    n(3, 4) must contain(Cell(4, 4))
    n(3, 4) must contain(Cell(3, 3))
  }

  it must "return 3 neighbours for a boundary cell" in {
    val n = Neighbours(6, 6)
    n(0, 3) must contain(Cell(0, 2))
    n(0, 3) must contain(Cell(0, 4))
    n(0, 3) must contain(Cell(1, 3))
    n(0, 3) must not contain(Cell(-1, 3))
  }

  "identifyRegions" must "correctly identify two regions in a network of incompletely joined cells" in {
    val routes = Array.fill[mutable.Set[Cell]](2, 2) { mutable.Set() }
    routes(0)(0) += Cell(1, 0)
    routes(1)(0) += Cell(0, 0)
    routes(0)(1) += Cell(1, 1)
    routes(1)(1) += Cell(0, 1)

    val m = new Maze(routes)

    val regions = m identifyRegions

    val upperRegion = regions(0)(0)
    val lowerRegion = regions(0)(1)
    upperRegion mustNot equal(lowerRegion)
    regions(1)(0) must equal(upperRegion)
    regions(1)(1) must equal(lowerRegion)
  }

  it must "correctly identify one region in a network of incompletely joined cells" in {
    val routes = Array.fill[mutable.Set[Cell]](3, 4) { mutable.Set() }
    routes(0)(0) += (Cell(1, 0), Cell(0,1))
    routes(1)(0) += (Cell(0, 0), Cell(1,1))
    routes(2)(0) += (Cell(2, 1))
    routes(0)(1) += (Cell(0,0), Cell(0, 2))
    routes(1)(1) += (Cell(1, 0), Cell(1, 2))
    routes(2)(1) += (Cell(2,0), Cell(2,3))
    routes(0)(2) += (Cell(0, 3), Cell(0, 1))
    routes(1)(2) += (Cell(1, 1), Cell(1, 3))
    routes(2)(2) += (Cell(2, 1), Cell(2, 3))
    routes(0)(3) += (Cell(0, 2))
    routes(1)(3) += (Cell(1, 2), Cell(2, 3))
    routes(2)(3) += (Cell(1, 3), Cell(2, 2))

    val m = new Maze(routes)

    val t = m.regions(0, 0)
    for (
      y <- 0 until m.height;
      x <- 0 until m.width
    ) {
      m.regions(x, y) must equal(t)
    }
  }

  it must "correctly identify three regions in a network of incompletely joined cells" in {
    val routes = Array.fill[mutable.Set[Cell]](2, 3) { mutable.Set() }
    routes(0)(0) += Cell(1, 0)
    routes(1)(0) += Cell(0, 0)
    routes(0)(1) += Cell(1, 1)
    routes(1)(1) += Cell(0, 1)
    routes(0)(2) += Cell(1, 2)
    routes(1)(2) += Cell(0, 2)

    val m = new Maze(routes)

    val regions = m identifyRegions

    val upperRegion = regions(0)(0)
    val midRegion = regions(0)(1)
    val lowerRegion = regions(0)(2)
    upperRegion mustNot equal(lowerRegion)
    upperRegion mustNot equal(midRegion)
    lowerRegion mustNot equal(midRegion)
    regions(1)(0) must equal(upperRegion)
    regions(1)(1) must equal(midRegion)
    regions(1)(2) must equal(lowerRegion)
  }

  "transform" must "transform a maze into a squared grid" in {
    val routes = Array.fill[mutable.Set[Cell]](3, 4) { mutable.Set() }
    routes(0)(0) += (Cell(1, 0), Cell(0,1))
    routes(1)(0) += (Cell(0, 0), Cell(1,1))
    routes(2)(0) += (Cell(2, 1))
    routes(0)(1) += (Cell(0,0), Cell(0, 2))
    routes(1)(1) += (Cell(1, 0), Cell(1, 2))
    routes(2)(1) += (Cell(2,0), Cell(2,3))
    routes(0)(2) += (Cell(0, 3), Cell(0, 1))
    routes(1)(2) += (Cell(1, 1), Cell(1, 3))
    routes(2)(2) += (Cell(2, 1), Cell(2, 3))
    routes(0)(3) += (Cell(0, 2))
    routes(1)(3) += (Cell(1, 2), Cell(2, 3))
    routes(2)(3) += (Cell(1, 3), Cell(2, 2))

    val m = new Maze(routes)

    val t = MazeTransform.transformToGrid[Int](m, 2, 99, 100, (x, y) => { m.regions(x, y) })

    t.size must equal(10)
    t(0).size must equal(13)

    // 1111111111
    // 1000001001
    // 1000001001
    // 1001001001
    // 1001001001
    // 1001001001
    // 1001001001
    // 1001000001
    // 1001000001
    // 1111111111
    for (
      y <- 0 until t(0).length;
      x <- 0 until t.length
      ) {
      if (x == 0 || x == 9 || (y == 0 && x != 3 * m.entrance + 1 && x != (3 * m.entrance) + 2) || y == 12) {
        t(x)(y) must equal(99)
      }
      if (x == 3 && y != 1 && y != 2) {
        t(x)(y) must equal(99)
      }
      if (x == 6 && y != 10 && y != 11) {
        t(x)(y) must equal(99)
      }
    }

    // todo: check that the entrance is carved out

    // 48 is 0
    // 32 is space
    //display[Int](t, (c: Int) => { if( c == 99 ) 'X' else if ( c == 100 ) ' ' else (c + 48).toChar})
  }

  "joinRegions" must "form a link and cause the region count to drop by one" in {
    val routes = Array.fill[mutable.Set[Cell]](3, 4) { mutable.Set() }
    routes(0)(0) += (Cell(1, 0), Cell(0,1))
    routes(1)(0) += (Cell(0, 0), Cell(1,1))
    routes(2)(0) += (Cell(2, 1))
    routes(0)(1) += (Cell(0,0), Cell(0, 2))
    routes(1)(1) += (Cell(1, 0)/*, Cell(1, 2)*/)
    routes(2)(1) += (Cell(2,0), Cell(2,3))
    routes(0)(2) += (Cell(0, 3), Cell(0, 1))
    routes(1)(2) += (/*Cell(1, 1),*/ Cell(1, 3))
    routes(2)(2) += (Cell(2, 1), Cell(2, 3))
    routes(0)(3) += (Cell(0, 2))
    routes(1)(3) += (Cell(1, 2), Cell(2, 3))
    routes(2)(3) += (Cell(1, 3), Cell(2, 2))

    val m = new Maze(routes)

    val mp =  new Maze(routes.map(_.map(_.clone))) // deep copy of routes

    val initialRegionCount = maxRegion(mp)

    val t = MazeTransform.transformToGrid[Int](mp, 2, 99, 100, (x, y) => { mp.regions(x, y) })
    MazePublish.display[Int](t, (c: Int) => { if( c == 99 ) 'X' else if ( c == 100 ) ' ' else (c + 48).toChar})

    mp joinRegions

    maxRegion(mp) must equal(initialRegionCount - 1)

    val tp = MazeTransform.transformToGrid[Int](mp, 2, 99, 100, (x, y) => { mp.regions(x, y) })
    MazePublish.display[Int](tp, (c: Int) => { if( c == 99 ) 'X' else if ( c == 100 ) ' ' else (c + 48).toChar})
  }

  "breakALink" must "break a link and cause regions to be redetermined" in {
    val routes = Array.fill[mutable.Set[Cell]](3, 4) { mutable.Set() }
    routes(0)(0) += (Cell(1, 0), Cell(0,1))
    routes(1)(0) += (Cell(0, 0), Cell(1,1))
    routes(2)(0) += (Cell(2, 1))
    routes(0)(1) += (Cell(0,0), Cell(0, 2))
    routes(1)(1) += (Cell(1, 0), Cell(1, 2))
    routes(2)(1) += (Cell(2,0), Cell(2,3))
    routes(0)(2) += (Cell(0, 3), Cell(0, 1))
    routes(1)(2) += (Cell(1, 1), Cell(1, 3))
    routes(2)(2) += (Cell(2, 1), Cell(2, 3))
    routes(0)(3) += (Cell(0, 2))
    routes(1)(3) += (Cell(1, 2), Cell(2, 3))
    routes(2)(3) += (Cell(1, 3), Cell(2, 2))

    val m = new Maze(routes)

    val mp =  new Maze(routes.map(_.map(_.clone))) // deep copy of routes

//    val t = MazeTransform.transformToGrid[Int](mp, 2, 99, 100, (x, y) => { mp.regions(x)(y) })
//    MazePublish.display[Int](t, (c: Int) => { if( c == 99 ) 'X' else if ( c == 100 ) ' ' else (c + 48).toChar})

    mp breakALink

//    val tp = MazeTransform.transformToGrid[Int](mp, 2, 99, 100, (x, y) => { mp.regions(x)(y) })
//    MazePublish.display[Int](tp, (c: Int) => { if( c == 99 ) 'X' else if ( c == 100 ) ' ' else (c + 48).toChar})

    val (blockedCount: Int, openedCount: Int) = countOpenedAndBlocked(m, mp)

    blockedCount must equal(2) // 2, for a route in each direction

    maxRegion(mp) must equal(2)



    // check regions formed (in this maze breaking any link should form two regions)
    // complement is joinRegions -- should be able to find all pairs at boundary between any two regions
    // and form a link between them
    // so every time you call joinRegions the number of regions should reduce by one
  }

  def maxRegion(m: Maze): Int = {
    var max = 0;
    for (
      y <- 0 until m.height;
      x <- 0 until m.width
    ) {
      if( m.regions(x, y) > max ) max = m.regions(x, y)
    }

    max
  }

  def countOpenedAndBlocked(m: Maze, mp: Maze): (Int, Int) = {
    var blockedCount = 0
    var openedCount = 0

    for {
      y <- 0 until m.height;
      x <- 0 until m.width
    } {
      if (mp(x, y).size > m(x, y).size) openedCount = openedCount + 1
      if (mp(x, y).size < m(x, y).size) blockedCount = blockedCount + 1
    }

    (blockedCount, openedCount)
  }
}
