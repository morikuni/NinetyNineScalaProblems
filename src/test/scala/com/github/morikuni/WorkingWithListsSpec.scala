package com.github.morikuni

import org.specs2.mutable.Specification

class WorkingWithListsSpec extends Specification {
  import WorkingWithLists._

  "P01 last(List)" should {
    "returns the last element of a List" in {
      last(List(1, 1, 2, 3, 5, 8)) mustEqual 8
    }
  }

  "P02 penultimate(List)" should {
    "returns the last but one element of a List" in {
      penultimate(List(1, 1, 2, 3, 5, 8)) must_=== 5
    }
  }

  "P03 nth(n, List)" should {
    "returns the n-th element of a List" in {
      nth(2, List(1, 1, 2, 3, 5, 8)) must_=== 2
    }
  }

  "P04 length(List)" should {
    "returns the number of elements of a List" in {
      WorkingWithLists.length(List(1, 1, 2, 3, 5, 8)) must be equalTo 6
    }
  }

  "P05 reverse(List)" should {
    "reverse a List" in {
      reverse(List(1, 1, 2, 3, 5, 8)) must_=== List(8, 5, 3, 2, 1, 1)
    }
  }

  "P06 isPalindrome(List)" should {
    "returns true if a List is a palindrome" in {
      isPalindrome(List(1, 2, 3, 2, 1)) must_=== true
    }

    "returns false if a List is not a palindrome" in {
      isPalindrome(List(1, 2, 3, 2)) must_=== false
    }
  }

  "P07 flatten(List)" should {
    "flattens a nested List" in {
      flatten(List(List(1, 1), 2, List(3, List(5, 8)))) must_=== List(1, 1, 2, 3, 5, 8)
    }
  }

  "P08 compress(List)" should {
    "eliminates consecutive duplicates of list elements" in {
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== List('a, 'b, 'c, 'a, 'd, 'e)
    }
  }

  "P09 pack(List)" should {
    "packs consecutive duplicates of list elements into sublists" in {
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    }
  }

  "P10 encode(List)" should {
    "Run-length encoding of a List" in {
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) must_== List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    }
  }
}
