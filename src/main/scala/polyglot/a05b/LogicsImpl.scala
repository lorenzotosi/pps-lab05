package polyglot.a05b

import polyglot.a05b.Logics

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private var tickCount = 0
  private val random = scala.util.Random(10)
  private val initial: (Int, Int) = (random.nextInt(size - 2) + 1, random.nextInt(size - 2) + 1)

  override def tick(): Unit = tickCount = tickCount + 1

  override def isOver: Boolean =
    (initial._2 - tickCount < 0) || (initial._2 + tickCount >= size) ||
      (initial._1 - tickCount < 0) || (initial._1 + tickCount >= size)

//    initial match
//    case (x, y) => (y - tickCount < 0) || (y + tickCount >= size) || (x - tickCount < 0) || (x + tickCount >= size)

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initial._1) && (Math.abs(y - initial._2) <= tickCount) ||
      (y == initial._2) && (Math.abs(x - initial._1) <= tickCount) ||
      (x - y == initial._1 - initial._2) && (Math.abs(x - initial._1) <= tickCount) ||
      (x + y == initial._1 + initial._2) && (Math.abs(x - initial._1) <= tickCount)

//    initial match
//    case (initialX, initialY) => (x == initialX) && (Math.abs(y - initialY) <= tickCount) ||
//      (y == initialY) && (Math.abs(x - initialX) <= tickCount) ||
//      (x - y == initialX - initialY) && (Math.abs(x - initialX) <= tickCount) ||
//      (x + y == initialX + initialY) && (Math.abs(x - initialX) <= tickCount)

