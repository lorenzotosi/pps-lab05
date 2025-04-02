package polyglot.a01a

import util.Sequences.Sequence.*
import util.Sequences.*

import polyglot.a01a.Logics
import polyglot.a01a.Logics.Result

trait Logic:
  def hit(row: Int, col: Int): Result


/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logic:
  private val FAILURES = 5
  private val random = scala.util.Random(2)
  private var hit: Sequence[(Int, Int)] = Nil()
  private val boatRow = random.nextInt(size)
  private val boatLeftCol = random.nextInt(size-boat+1)
  private var failures = 0
  private var hitSize = 0

  def hit(row: Int, col: Int): Result =
    if
      row == boatRow && col >= boatLeftCol && col < boatLeftCol + boat
    then
      hit = Cons((row, col), hit)
      hitSize = hitSize + 1
      if hitSize == boat then Result.WON else Result.HIT
    else
      failures = failures + 1
      if failures == FAILURES then Result.LOST else Result.MISS
