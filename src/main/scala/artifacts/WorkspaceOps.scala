package io.github.cakelier
package artifacts

import scala.concurrent.Future
import scala.util.Try

trait WorkspaceOps[A <: Artifact[B], B] {
  def createArtifact(name: String, constructor: Context ?=> A): Try[ArtifactRef[A, B]]

  def retrieveArtifact(name: String): Try[ArtifactRef[A, B]]

  def retrieveArtifactWhenAvailable(name: String): Future[ArtifactRef[A, B]]

  def disposeArtifact(name: String): Try[Unit]
}
