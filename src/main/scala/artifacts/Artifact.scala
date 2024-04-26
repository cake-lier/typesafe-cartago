package io.github.cakelier
package artifacts

import java.util.UUID
import scala.util.{Failure, Success, Try}

case class Artifact[A]()(using Context) {
  private var properties: Map[String, ? <: A] = Map.empty
  private var observers: Map[ObserverId, Observer[A]] = Map.empty

  protected final def createObservableProperty[B <: A](name: String, init: B): Try[Unit] = 
    if (!properties.contains(name)) {
      properties += (name -> init)
      observers.foreach(_._2.onPropertyCreated(Property(this, name, init)))
      Success(())
    } else {
      Failure(IllegalArgumentException())
    }

  protected final def deleteObservableProperty(name: String): Try[Unit] = 
    if (properties.contains(name)) {
      properties -= name
      observers.foreach(_._2.onPropertyDeleted(this, name))
      Success(())
    } else {
      Failure(IllegalArgumentException())
    }

  protected final def updateObservableProperty[B <: A](name: String, value: B): Try[Unit] = 
    if (properties.contains(name)) {
      properties += (name -> value)
      observers.foreach(_._2.onPropertyUpdated(Property(this, name, value)))
      Success(())
    } else {
      Failure(IllegalArgumentException())
    }

  protected final def observableProperty(name: String): Try[A] = properties.get(name).toRight(IllegalStateException()).toTry

  protected final def signal[B <: A](name: String, value: B): Unit = 
    observers.foreach(_._2.onPerceptReceived(Signal(this, name, value)))

  protected[artifacts] final def addObserver(observer: Observer[A]): ObserverId = {
    val id = UUID.randomUUID().toString
    observers += (id -> observer)
    id
  }

  protected[artifacts] final def removeObserver(id: ObserverId): Unit = observers -= id

  protected[artifacts] final def observableProperties: Map[String, ? <: A] = properties
}
