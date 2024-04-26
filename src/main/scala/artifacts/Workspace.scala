package io.github.cakelier
package artifacts

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

trait Workspace[A <: Artifact[B], B] extends WorkspaceRef[A, B] with WorkspaceOps[A, B]

object Workspace {
  private case class WorkspaceImpl[A <: Artifact[B], B](
    override val name: String,
    override val path: String,
    override val parent: Option[WorkspaceRef[A, B]]
  ) extends AbstractWorkspaceRef[A, B](name, path, parent) with Workspace[A, B] {
    private var artifacts: Map[String, ArtifactRef[A, B]] = Map.empty
    private var createdArtifacts: Map[String, Promise[ArtifactRef[A, B]]] = Map.empty

    override def createArtifact(name: String, constructor: Context ?=> A): Try[ArtifactRef[A, B]] =
      artifacts.get(name)
               .map(_ => Failure[ArtifactRef[A, B]](IllegalArgumentException()))
               .getOrElse {
                 val ref = ArtifactRef(name, constructor, this)
                 artifacts += (name -> ref)
                 createdArtifacts.get(name).foreach(p => {
                   p.success(ref)
                   createdArtifacts -= name
                 })
                 Success(ref)
               }

    override def retrieveArtifact(name: String): Try[ArtifactRef[A, B]] =
      artifacts.get(name).toRight(IllegalArgumentException()).toTry

    override def retrieveArtifactWhenAvailable(name: String): Future[ArtifactRef[A, B]] =
      artifacts.get(name)
               .map(Future.successful)
               .getOrElse(createdArtifacts.get(name) match {
                 case Some(promise) => promise.future
                 case None =>
                   val createdPromise = Promise[ArtifactRef[A, B]]()
                   createdArtifacts += (name -> createdPromise)
                   createdPromise.future
               })

    override def disposeArtifact(name: String): Try[Unit] =
      artifacts.get(name)
               .map(a => {
                 artifacts -= name
                 a.dispose()
                 Success(())
               })
               .getOrElse(Failure(IllegalStateException()))
  }

  protected[artifacts] def apply[A <: Artifact[B], B](
    name: String,
    path: String,
    parent: Option[WorkspaceRef[A, B]]
  ): Workspace[A, B] =
    WorkspaceImpl(name, path, parent)
}
