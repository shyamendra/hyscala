import scala.io.StdIn

sealed trait Symbol
case object Cross  extends Symbol
case object Circle extends Symbol
case object Empty  extends Symbol

object TicTacToe {

  type Grid = List[List[Symbol]]
  type Row  = List[Symbol]

  def printGrid(grid: Grid): Unit = {
    println("Grid: ")
    grid.foreach(println)
  }

  val InitialValue = 0

  def isRowComplete(row: Row): Boolean = row.distinct.size == 1 && row.head != Empty

  def isAnyRowComplete(grid: Grid): Boolean = grid.exists(isRowComplete)
  
  def isAnyColumnComplete(grid: Grid): Boolean = isAnyRowComplete(grid.transpose)
  
  def isPrimaryDiagComplete(grid: Grid): Boolean = {
    val diagonalRow = (0 until grid.size).toList.map{ i => grid(i)(i) }
    isRowComplete(diagonalRow)
  }
  
  def isAnyDiagComplete(grid: Grid): Boolean = {
    isPrimaryDiagComplete(grid) || isPrimaryDiagComplete(grid.reverse)
  }
  
  def isComplete(grid: Grid): Boolean = {
    isAnyRowComplete(grid) || isAnyColumnComplete(grid) || isAnyDiagComplete(grid)
  }
  
  def update(grid: Grid, index: Int, symbol: Symbol): Grid = {
    val row = index / grid.size
    val col = index % grid.size
    val gridIndicesList = (0 until grid.size).toList
    gridIndicesList.map{ i =>
      gridIndicesList.map{ j =>
         if (i == row && j == col) symbol else grid(i)(j)
      }
    }
  }

  def createGrid(size: Int): Grid = List.fill(size)(List.fill(size)(Empty))

  def playerName(isFirst: Boolean) = if (isFirst) "First Player" else "Second Player"
  
  def startGame(size: Int): Unit = {
    val maxSteps = size * size
    val allIndices = (1 to maxSteps).toList

    var grid = createGrid(size)
    var firstPlayer = true
    var count = 0
    var usedIndices = List.empty[Int]

    while(!isComplete(grid) && count < maxSteps) {
      val availableIndices = (allIndices.toSet -- usedIndices.toSet).toList.sorted
      var index = InitialValue
      do { 
        println(s"\n${playerName(firstPlayer)} to play. Choose from: $availableIndices")
        index = StdIn.readInt
      }
      while(!availableIndices.contains(index))
      usedIndices = usedIndices :+ index
      grid = update(grid, index-1, if (firstPlayer) Cross else Circle)
      firstPlayer = !firstPlayer
      count += 1
      printGrid(grid)
    }

    println("\nTic Tac Toe: Complete")
    println(if (count == maxSteps) "Draw!"
            else s"${playerName(!firstPlayer)} Wins!")
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to Tic Tac Toe. Choose your grid size: ")
    val size = StdIn.readInt
    startGame(size)
  }
}
