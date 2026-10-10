package canopy.ingest

import cats.effect.IO
import fs2.Stream
import munit.CatsEffectSuite

class StreamOpsSuite extends CatsEffectSuite:
  private final case class Ev(id: String, ts: Long)

  private def reorder(events: List[Ev], lateness: Long): IO[List[Ordered[Ev]]] =
    Stream.emits(events).covary[IO].through(StreamOps.reorder(lateness)(_.ts)).compile.toList

  test("events within the lateness bound come out sorted") {
    val events = List(Ev("a", 10), Ev("c", 30), Ev("b", 20), Ev("e", 50), Ev("d", 40))
    reorder(events, 15).map { out =>
      assertEquals(out.collect { case Ordered.InOrder(e) => e.id }, List("a", "b", "c", "d", "e"))
    }
  }

  test("events later than already-released ones are flagged, not dropped") {
    val events = List(Ev("a", 100), Ev("b", 200), Ev("c", 300), Ev("old", 150))
    reorder(events, 50).map { out =>
      assert(out.contains(Ordered.Late(Ev("old", 150), 50)), out.toString)
      assertEquals(out.size, 4)
    }
  }

  test("ties keep arrival order and zero lateness passes events straight through") {
    val events = List(Ev("x", 5), Ev("y", 5), Ev("z", 5))
    reorder(events, 0).map(out => assertEquals(out.collect { case Ordered.InOrder(e) => e.id }, List("x", "y", "z")))
  }

  test("reordering works across chunk boundaries") {
    val events = (1 to 1000).map(i => Ev(s"$i", if i % 2 == 0 then i - 1L else i + 1L)).toList
    Stream.emits(events).covary[IO].chunkLimit(7).unchunks
      .through(StreamOps.reorder(5)(_.ts)).compile.toList
      .map(out => assertEquals(out.collect { case Ordered.InOrder(e) => e.ts }, events.map(_.ts).sorted))
  }

  test("dedupe drops repeats inside the window and forgets them after") {
    val events = List(Ev("a", 0), Ev("a", 5), Ev("b", 6), Ev("a", 20), Ev("a", 25))
    Stream.emits(events).covary[IO].through(StreamOps.dedupe(10)(_.id, _.ts)).compile.toList
      .map(out => assertEquals(out, List(Ev("a", 0), Ev("b", 6), Ev("a", 20))))
  }

  test("ndjson decoding keeps line numbers and skips blank lines") {
    val input = "1\n\nx\n3\n"
    Stream.emits(input.getBytes.toSeq).covary[IO]
      .through(StreamOps.ndjson[IO, Int](s => s.toIntOption.toRight(List(s"bad: $s"))))
      .compile.toList
      .map(out => assertEquals(out, List(Right(1), Left(IngestError(3, List("bad: x"))), Right(3))))
  }
