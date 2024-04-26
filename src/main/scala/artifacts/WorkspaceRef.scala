package io.github.cakelier
package artifacts

import scala.concurrent.Future
import scala.util.{Failure, Try}

trait WorkspaceRef[A <: Artifact[B], B] {
  val name: String
  
  val path: String
  
  val parent: Option[WorkspaceRef[A, B]]
  
  def children: Set[WorkspaceRef[A, B]]
  
  protected[artifacts] def addChild(child: WorkspaceRef[A, B]): Unit
}

object WorkspaceRef {
  protected[artifacts] abstract class AbstractWorkspaceRef[A <: Artifact[B], B](
    override val name: String,
    override val path: String,
    override val parent: Option[WorkspaceRef[A, B]]
  ) extends WorkspaceRef[A, B] {
    private var innerChildren: Set[WorkspaceRef[A, B]] = Set.empty
   
    override def children: Set[WorkspaceRef[A, B]] = synchronized(innerChildren)

    override protected[artifacts] def addChild(child: WorkspaceRef[A, B]): Unit = synchronized {
      innerChildren += child
    }
  }
}

export WorkspaceRef.*
