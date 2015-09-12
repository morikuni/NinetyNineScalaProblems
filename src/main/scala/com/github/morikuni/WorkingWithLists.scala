package com.github.morikuni

object WorkingWithLists {
  def last[A](l: List[A]): A = l.last

  def penultimate[A](l: List[A]): A = l.takeRight(2).head

  def nth[A](n: Int, l: List[A]): A = l(n)

  def length[A](l: List[A]): Int = l.length

  def reverse[A](l: List[A]): List[A] = l.reverse

  def isPalindrome[A](l: List[A]): Boolean = l == l.reverse

  def flatten(l: List[Any]): List[Any] = {
    @scala.annotation.tailrec
    def loop(l: List[Any], acc: List[Any]): List[Any] = {
      l match {
        case Nil => acc
        case (xs: List[_]) :: tail => loop(tail, acc ++ flatten(xs))
        case x :: tail => loop(tail, acc :+ x)
      }
    }
    loop(l, Nil)
  }
}
