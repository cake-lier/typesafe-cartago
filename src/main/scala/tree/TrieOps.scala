package io.github.cakelier
package tree

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.auto.autoUnwrap

import scala.annotation.{tailrec, targetName}

trait TrieOps[A, B, -CC <: Trie[A, B]] {
  @targetName("update")
  def +(t: CC, e: (Path[A], B)): Trie[A, B]

  def updated(t: CC, p: Path[A], v: B): Trie[A, B]

  def updatedWith(t: CC, p: Path[A], f: Option[B] => Option[B]): Trie[A, B]
  
  @targetName("remove")
  def -(t: CC, p: Path[A]): Trie[A, B]
  
  def removed(t: CC, p: Path[A]): Trie[A, B]
  
  def contains(t: CC, p: Path[A]): Boolean

  def get(t: CC, p: Path[A]): Option[B]

  def apply(t: CC, p: Path[A]): B
}

object TrieOps {
  type Path[A] = Seq[A] Refined NonEmpty

  given [A, B, CC <: Trie[A, B]]: TrieOps[A, B, CC] with {
    @targetName("update")
    override inline def +(t: CC, e: (Path[A], B)): Trie[A, B] = updated(t, e._1, e._2)

    override def updated(t: CC, p: Path[A], v: B): Trie[A, B] = {
      @tailrec
      def _updated(remainingTree: Trie[A, B], remainingPath: Seq[A]): Trie[A, B] = remainingPath match {
        case Seq(_) => remainingTree match {
          case TrieNode(k, _, c) => TrieNode(k, Some(v), c)
          case TrieLeaf(k, _) => TrieLeaf(k, v)
        }
        case Seq(_, es*) => remainingTree match {
          case TrieNode(k, e, cs) => cs.find(_.key == es.head) match {
            case Some(c) => _updated(c, es)
            case _ => TrieNode(k, e, cs :+ fromPath(es, v))
          }
          case TrieLeaf(k, e) => TrieNode(k, Some(e), Seq(fromPath(es, v)))
        }
      }
      if (t.key == p.head)
        _updated(t, p)
      else
        t
    }

    override def updatedWith(t: CC, p: Path[A], f: Option[B] => Option[B]): Trie[A, B] = {
      @tailrec
      def _updatedWith(remainingTree: Trie[A, B], remainingPath: Seq[A]): Trie[A, B] = remainingPath match {
        case Seq(_) => remainingTree match {
          case TrieNode(k, v, c) => TrieNode(k, f(v), c)
          case TrieLeaf(k, v) => f(Some(v)) match {
            case Some(nv) => TrieLeaf(k, nv)
            case _ => TrieNode(k, None, Seq.empty)
          }
        }
        case Seq(_, es*) => remainingTree match {
          case n @ TrieNode(k, v, cs) => cs.find(_.key == es.head) match {
            case Some(c) => _updatedWith(c, es)
            case _ => f(None) match {
              case Some(nv) => TrieNode(k, v, cs :+ fromPath(es, nv))
              case _ => n
            }
          }
          case l @ TrieLeaf(k, v) => f(None) match {
            case Some(nv) => TrieNode(k, Some(v), Seq(fromPath(es, nv)))
            case _ => l
          }
        }
      }
      if (t.key == p.head)
        _updatedWith(t, p)
      else
        t
    }
    
    @targetName("remove")
    override inline def -(t: CC, p: Path[A]): Trie[A, B] = removed(t, p)

    override def removed(t: CC, p: Path[A]): Trie[A, B] = {
      @tailrec
      def _removed(remainingTree: Trie[A, B], remainingPath: Seq[A]): Trie[A, B] = remainingPath match {
        case Seq(_) => remainingTree match {
          case TrieNode(k, _, c) => TrieNode(k, None, c)
          case TrieLeaf(k, _) => TrieNode(k, None, Seq.empty) 
        }
        case Seq(_, es*) => remainingTree match {
          case n @ TrieNode(_, _, cs) => cs.find(_.key == es.head) match {
            case Some(c) => _removed(c, es)
            case _ => n
          }
          case l => l
        }
      }

      if (t.key == p.head)
        _removed(t, p)
      else
        t
    }

    override def contains(t: CC, p: Path[A]): Boolean = {
      @tailrec
      def _contains(remainingTree: Trie[A, B], remainingPath: Seq[A]): Boolean = remainingPath match {
        case Seq(_) => remainingTree match {
          case TrieNode(_, v, _) => v.isDefined
          case _ => true
        }
        case Seq(_, es*) => remainingTree match {
          case TrieNode(_, _, cs) => cs.find(_.key == es.head) match {
            case Some(c) => _contains(c, es)
            case _ => false
          }
          case _ => false
        }
      }
      if (t.key == p.head)
        _contains(t, p)
      else
        false
    }

    override def get(t: CC, p: Path[A]): Option[B] = {
      @tailrec
      def _get(remainingTree: Trie[A, B], remainingPath: Seq[A]): Option[B] = remainingPath match {
        case Seq(_) => remainingTree match {
          case TrieNode(_, v, _) => v
          case TrieLeaf(_, v) => Some(v)
        }
        case Seq(_, es*) => remainingTree match {
          case TrieNode(_, _, cs) => cs.find(_.key == es.head) match {
            case Some(c) => _get(c, es)
            case _ => None
          }
          case _ => None
        }
      }
      if (t.key == p.head)
        _get(t, p)
      else
        None
    }

    override def apply(t: CC, p: Path[A]): B = get(t, p).get
  }
}

export TrieOps.*
