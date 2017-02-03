package ds

import java.math.max

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A], value: A) extends Tree[A]

object Tree {
  def size(tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right, _) => 1 + size(left) + size(right)
    }
  }

  def maximum(tree: Tree[A]): A = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right, value) => {
        val (lMax, rMax) = (maximum(left), maximum(right))
        Math.max(value, lMax, rMax)
      }
    }
  }

  def depth(tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right, _) => {
        val (lDep, rDep) = (depth(left), depth(right))
        Math.max(lDep, rDep)
      }
  }

  def map(tree: Tree[A], f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right, value) => Branch(map(left, f), map(right, f), f(value))
  }
}
