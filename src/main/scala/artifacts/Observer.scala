package io.github.cakelier
package artifacts

@FunctionalInterface
trait Observer[A] {
  def onPerceptReceived(percept: Percept[? <: A]): Unit

  def onPropertyUpdated(property: Property[? <: A]): Unit = onPerceptReceived(property)

  def onPropertyCreated(property: Property[? <: A]): Unit = onPerceptReceived(property)

  def onPropertyDeleted(source: Artifact[? <: A], name: String): Unit = ()
}

object Observer {
  type Id = String
}

export Observer.Id as ObserverId
