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

  def compress[A](l: List[A]): List[A] = {
    l.foldRight(Nil: List[A]) { (a, acc) =>
      if(acc.headOption.exists(_ == a)) acc
      else a :: acc
    }
  }

  def pack[A](l: List[A]): List[List[A]] = {
    l.foldRight(Nil: List[List[A]]) { (a, acc) =>
      acc match {
        case (aList@(x :: aTail)) :: lTail if x == a => (a :: aList) :: lTail
        case tail => List(a) :: tail
      }
    }
  }

  def encode[A](l: List[A]): List[(Int, A)] = {
    l.foldRight(Nil: List[(Int, A)]) { (a, acc) =>
      acc match {
        case (c, x) :: tail if x == a => (c+1, x) :: tail
        case tail => (1, a) :: tail
      }
    }
  }
}