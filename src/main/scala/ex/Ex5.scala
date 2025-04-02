package ex

import util.Sequences.*
import util.Sequences.Sequence.*

import scala.annotation.tailrec


object Extractor:
  private type Course = (String, String)

  object Course:
    def apply(name: String, category: String): Course = (name, category)
    def name(c: Course): String = c._1
    def category(c: Course): String = c._2

  object SameCategory:
    def unapply(course: Sequence[Course]): scala.Option[String] =
      @tailrec
      def _checkCategory(course: Sequence[Course], cat: String): Boolean = course match
        case Cons(h, t) => Course.category(h) == cat && _checkCategory(t, cat)
        case _ => true

      course match
        case Cons(h, t) => if _checkCategory(t, h._2) then scala.Some(h._2) else scala.Option.empty
        case _ => scala.Option.empty

@main def testExtractor(): Unit =
  import Extractor.*

  val category = "math"

  val course1 = Course("Algebra", category)
  val course2 = Course("Geometry", category)
  val course3 = Course("Analisi", category)
  val courses = Cons(course1, Cons(course2, Cons(course3, Nil())))

  courses match
    case SameCategory(cat) => println(s"$courses have same category $cat")
    case _ => println(s"$courses have different categories")