package io.github.cakelier

import artifacts.{Artifact, Context, Observer, Platform}

import java.util.concurrent.CountDownLatch
import scala.concurrent.Future

@main
def main(): Unit = {
  class CounterArtifact(v: Int)(using Context) extends Artifact[Int] {
    createObservableProperty("count", v)

    def inc(): Unit = for {
      count <- observableProperty("count")
      _ <- updateObservableProperty("count", count + 1)
    } yield ()
  }

  class ExampleArtifact(using Context) extends Artifact[Double]

  import scala.concurrent.ExecutionContext.Implicits.global

  val platform = Platform[CounterArtifact, Int]()
  val latch = CountDownLatch(2)
  for {
    _ <- Future.fromTry(platform.createWorkspace("w1"))
    w1 <- Future.fromTry(platform.joinWorkspace("/w1"))
    a <- w1.retrieveArtifactWhenAvailable("a")
    observer: Observer[Int] = p => println(p.value)
    id <- Future.fromTry(a.focus(observer))
    _ = Thread.sleep(1_000)
    _ <- Future.fromTry(a.stopFocus(id))
    _ = latch.countDown()
  } yield ()

  for {
    w1 <- platform.joinWorkspaceWhenAvailable("/w1")
    a1 <- Future.fromTry(w1.createArtifact("a", CounterArtifact(5)))
    _ = Thread.sleep(1_000)
    _ <- Future.fromTry(a1.doAction(_.inc()))
    _ <- Future.fromTry(a1.doAction(_.inc()))
    _ = latch.countDown()
  } yield ()

  latch.await()
}
