package ex

import ex.SchoolModel.BasicSchool
import ex.SchoolModel.BasicSchool.*
import util.Sequences.*
import util.Sequences.Sequence.*

object SchoolModel:
  private type Teacher = String
  private type Course = String

  trait BasicSchool:
    def setTeacherToCourse(teacher: Teacher, course: Course): BasicSchool
    def courses: Sequence[Course]
    def teachers: Sequence[Teacher]
    def coursesOfATeacher(teacher: Teacher): Sequence[Course]
    def hasTeacher(name: String): Boolean
    def hasCourse(name: String): Boolean

  private class BasicSchoolImpl(private val school: Sequence[(Teacher, Course)] = Nil()) extends BasicSchool:

    def setTeacherToCourse(teacher: Teacher, course: Course): BasicSchoolImpl =
      BasicSchoolImpl(school.concat(Cons((teacher, course), Nil())))

    def courses: Sequence[Course] =
      school.map((_, c) => c)

    def teachers: Sequence[Teacher] =
      school.map((t, _) => t)

    def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
      school.filter((t, _) => t == teacher).map((_, c) => c)

    def hasTeacher(name: String): Boolean =
      school.filter((t, _) => t == name) != Nil()

    def hasCourse(name: String): Boolean =
      school.filter((_, c) => c == name) != Nil()

  object BasicSchool:
    def emptySchool: BasicSchool = new BasicSchoolImpl()

    def teacher(name: String): Teacher = name

    def course(name: String): Course = name

@main def examples(): Unit =
  val school = emptySchool
  println(school.teachers) // Nil()
  println(school.courses) // Nil()
  println(school.hasTeacher("John")) // false
  println(school.hasCourse("Math")) // false
  val john = teacher("John")
  val math = course("Math")
  val italian = course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // Cons("John", Nil())
  println(school2.courses) // Cons("Math", Nil())
  println(school2.hasTeacher("John")) // true
  println(school2.hasCourse("Math")) // true
  println(school2.hasCourse("Italian")) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers) // Cons("John", Nil())
  println(school3.courses) // Cons("Math", Cons("Italian", Nil()))
  println(school3.hasTeacher("John")) // true
  println(school3.hasCourse("Math")) // true
  println(school3.hasCourse("Italian")) // true
  println(school3.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))
