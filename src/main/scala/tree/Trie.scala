package io.github.cakelier
package tree

import scala.annotation.{tailrec, targetName}

sealed trait Trie[A, B] {
  val key: A
}

object Trie {
  final case class TrieNode[A, B](key: A, value: Option[B], children: Seq[Trie[A, B]]) extends Trie[A, B]

  final case class TrieLeaf[A, B](key: A, value: B) extends Trie[A, B]

  extension [A, B](t: Trie[A, B])(using TrieOps[A, B, Trie[A, B]]) {
    @targetName("update")
    inline def +(e: (Path[A], B)): Trie[A, B] = summon[TrieOps[A, B, Trie[A, B]]].+(t, e)

    inline def updated(p: Path[A], v: B): Trie[A, B] = summon[TrieOps[A, B, Trie[A, B]]].updated(t, p, v)

    inline def updatedWith(p: Path[A], f: Option[B] => Option[B]): Trie[A, B] = 
      summon[TrieOps[A, B, Trie[A, B]]].updatedWith(t, p, f)

    @targetName("remove")
    inline def -(p: Path[A]): Trie[A, B] = summon[TrieOps[A, B, Trie[A, B]]].-(t, p)

    inline def removed(p: Path[A]): Trie[A, B] = summon[TrieOps[A, B, Trie[A, B]]].removed(t, p)

    inline def contains(p: Path[A]): Boolean = summon[TrieOps[A, B, Trie[A, B]]].contains(t, p)

    inline def get(p: Path[A]): Option[B] = summon[TrieOps[A, B, Trie[A, B]]].get(t, p)

    inline def apply(p: Path[A]): B = summon[TrieOps[A, B, Trie[A, B]]].apply(t, p)
  }

  def leaf[A, B](k: A, v: B): Trie[A, B] = TrieLeaf(k, v)

  def node[A, B](k: A, v: Option[B], c: Seq[Trie[A, B]]): Trie[A, B] = TrieNode(k, v, c)

  def fromPath[A, B](p: Seq[A], v: B): Trie[A, B] = {
    @tailrec
    def _fromPath(remainingPath: Seq[A], accumulator: Trie[A, B] => Trie[A, B]): Trie[A, B] = remainingPath match {
      case Seq(k) => accumulator(TrieLeaf(k, v))
      case Seq(k, ks*) => _fromPath(ks, t => accumulator(TrieNode(k, None, Seq(t))))
    }
    _fromPath(p, identity)
  }
}

export Trie.*