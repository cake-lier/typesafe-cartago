package io.github.cakelier
package artifacts

import tree.Trie

import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

trait Platform[A <: Artifact[B], B] {
  val rootWorkspace: Workspace[A, B]

  def createWorkspace(name: String, parent: WorkspaceRef[A, B] = rootWorkspace): Try[Unit]

  def joinWorkspace(path: String): Try[AgentBody[A, B]]

  def joinWorkspaceWhenAvailable(path: String): Future[AgentBody[A, B]]
}

object Platform {
  private case class PlatformImpl[A <: Artifact[B], B](rootWorkspace: Workspace[A, B]) extends Platform[A, B] {
    private var workspaces: Trie[String, Workspace[A, B]] = Trie.leaf("", rootWorkspace)
    private var createdBodies: Trie[String, Seq[Promise[AgentBody[A, B]]]] = Trie.leaf("", Seq.empty)

    def createWorkspace(name: String, parent: WorkspaceRef[A, B] = rootWorkspace): Try[Unit] = synchronized {
      val path: String = parent.path + name
      refineV[NonEmpty](path.split('/').toSeq)
        .left
        .map(IllegalArgumentException(_))
        .toTry
        .flatMap(p => workspaces.get(p)
                                .map(_ => Failure[Unit](IllegalArgumentException()))
                                .getOrElse {
                                  val workspace = Workspace[A, B](name, path, Some(parent))
                                  workspaces += (p, workspace)
                                  parent.addChild(workspace)
                                  createdBodies.get(p).foreach(s => {
                                    s.foreach(_.success(AgentBody(workspace)))
                                    createdBodies -= p
                                  })
                                  Success(())
                                })
    }

    def joinWorkspace(path: String): Try[AgentBody[A, B]] = synchronized(
      refineV[NonEmpty](path.split('/').toSeq)
        .left
        .map(IllegalArgumentException(_))
        .toTry
        .flatMap(p => workspaces.get(p)
                                .map(AgentBody(_))
                                .toRight(IllegalArgumentException())
                                .toTry)
    )

    def joinWorkspaceWhenAvailable(path: String): Future[AgentBody[A, B]] = synchronized(
      refineV[NonEmpty](path.split('/').toSeq)
        .fold(
          e => Future.failed(IllegalArgumentException(e)),
          p => workspaces.get(p)
                         .map(w => Future.successful(AgentBody(w)))
                         .getOrElse {
                           val createdPromise = Promise[AgentBody[A, B]]()
                           createdBodies.updatedWith(p, {
                             case Some(s) => Some(s :+ createdPromise)
                             case None => Some(Seq(createdPromise))
                           })
                           createdPromise.future
                         }
        )
    )
  }

  def apply[A <: Artifact[B], B](): Platform[A, B] = PlatformImpl(Workspace("/", "/", None))
}
