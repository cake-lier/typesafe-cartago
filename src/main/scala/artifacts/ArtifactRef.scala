package io.github.cakelier
package artifacts

import scala.util.{Failure, Success, Try}

trait ArtifactRef[A <: Artifact[B], B] {
  val name: String
  
  val workspace: Workspace[A, B]
  
  def doAction[C](operation: A => C): Try[C]
  
  def focus(observer: Observer[B]): Try[ObserverId]

  def stopFocus(id: ObserverId): Try[Unit]
  
  protected[artifacts] def dispose(): Unit
}

object ArtifactRef {
  private case class ArtifactRefImpl[A <: Artifact[B], B](name: String, artifact: A, workspace: Workspace[A, B])
    extends ArtifactRef[A, B] {
    private var disposed = false
    
    override def focus(observer: Observer[B]): Try[ObserverId] = artifact.synchronized {
      if (!disposed) {
        val id = artifact.addObserver(observer)
        artifact.observableProperties.foreach((n, v) => observer.onPropertyUpdated(Property(artifact, n, v)))
        Success(id)
      } else {
        Failure(IllegalStateException())
      }
    }

    override def stopFocus(id: ObserverId): Try[Unit] = artifact.synchronized {
      if (!disposed)
        Success(artifact.removeObserver(id))
      else 
        Failure(IllegalStateException())
    }

    override def doAction[C](operation: A => C): Try[C] = artifact.synchronized {
      if (!disposed)
        Success(operation(artifact))
      else
        Failure(IllegalStateException())
    }

    override protected[artifacts] def dispose(): Unit = artifact.synchronized {
      disposed = true
    }
  }

  protected[artifacts] def apply[A <: Artifact[B], B](
    name: String, 
    artifact: A,
    workspace: Workspace[A, B]
  ): ArtifactRef[A, B] = 
    ArtifactRefImpl(name, artifact, workspace)
}
