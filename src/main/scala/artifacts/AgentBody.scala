package io.github.cakelier
package artifacts

import scala.concurrent.Future
import scala.util.{Failure, Try}

trait AgentBody[A <: Artifact[B], B] extends WorkspaceOps[A, B] {
  val workspace: WorkspaceRef[A, B]

  def leaveWorkspace(): Unit
}

object AgentBody {
  private case class AgentBodyImpl[A <: Artifact[B], B](innerWorkspace: Workspace[A, B]) extends AgentBody[A, B] {
    private var joined = true
    
    override val workspace: WorkspaceRef[A, B] = innerWorkspace

    override def createArtifact(name: String, constructor: Context ?=> A): Try[ArtifactRef[A, B]] = innerWorkspace.synchronized {
      if (joined)
        innerWorkspace.createArtifact(name, constructor)
      else
        Failure(IllegalStateException())
    }

    def retrieveArtifact(name: String): Try[ArtifactRef[A, B]] = innerWorkspace.synchronized {
      if (joined)
        innerWorkspace.retrieveArtifact(name)
      else
        Failure(IllegalStateException())
    }

    def retrieveArtifactWhenAvailable(name: String): Future[ArtifactRef[A, B]] = innerWorkspace.synchronized {
      if (joined)
        innerWorkspace.retrieveArtifactWhenAvailable(name) 
      else 
        Future.failed(IllegalStateException())
    }

    def disposeArtifact(name: String): Try[Unit] = innerWorkspace.synchronized {
      if (joined)
        innerWorkspace.disposeArtifact(name)
      else 
        Failure(IllegalStateException())
    }
    
    def leaveWorkspace(): Unit = innerWorkspace.synchronized {
      joined = false
    }
  }

  def apply[A <: Artifact[B], B](workspace: Workspace[A, B]): AgentBody[A, B] = AgentBodyImpl(workspace)
}
