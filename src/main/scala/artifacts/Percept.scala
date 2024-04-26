package io.github.cakelier
package artifacts

sealed trait Percept[A] {
  val source: Artifact[A]

  val name: String

  val value: A
}

object Percept {
  final case class Property[A](source: Artifact[A], name: String, value: A) extends Percept[A]

  final case class Signal[A](source: Artifact[A], name: String, value: A) extends Percept[A]
}

export Percept.*
