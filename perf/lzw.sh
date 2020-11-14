#!/usr/bin/env scala

if (args.isEmpty) {
  println("Which branches?")
  System.exit(1)
}
val branches = args

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream, PipedInputStream, PipedOutputStream}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration._
import scala.sys.process._

val Times = 20
val Repeat = 5
val Limit = 20_000_000
val Sleep = 1.minute

def die(message: String): Unit = {
  Console.err.println(message)
  System.exit(1)
}

def transferFully(in: InputStream)(outs: OutputStream*): Unit = {
  val buffer = new Array[Byte](1024)

  @tailrec
  def loop(): Unit = {
    val count = in.read(buffer)
    if (count != -1) {
      for (out <- outs) {
        out.write(buffer, 0, count)
      }
      loop()
    }
  }

  loop()
  in.close()
}

val measures = mutable.Map.empty[String, mutable.Buffer[Int]]

class ParserThread(branch: String) extends Thread {
  private val RE = raw"\d+ cases ÷ \d+s = (\d+) cases/s".r

  private val pos = new PipedOutputStream()
  private val reader = new BufferedReader(new InputStreamReader(new PipedInputStream(pos)))

  def outputStream: OutputStream = pos

  @tailrec
  override final def run(): Unit = {
    val line = reader.readLine()
    if (line != null) {
      line match {
        case RE(rate) =>
           if (!measures.contains(branch)) {
             measures(branch) = mutable.Buffer.empty
           }
           measures(branch).append(rate.toInt)
        case _ =>
      }
      run()
    } else {
      reader.close()
    }
  }
}


for (n <- 1 to Times) {
  println(s"$n/$Times")

  for (branch <- branches) {
    Thread.sleep(Sleep.toMillis)

    if (s"git switch $branch".! != 0) {
      die(s"Switch to branch $branch failed")
    }

    val t = new ParserThread(branch)
    t.start()

    val p = s"""sbt "Test/runMain lzw.core.GeneratedRoundTripTestsRunner --repeat $Repeat --limit $Limit""""
      .run(
        new ProcessIO(
          _.close(),
          outStream => {
            transferFully(outStream)(Console.out, t.outputStream)
            t.outputStream.close()
          },
          errStream => transferFully(errStream)(Console.err),
        )
      )

    if (p.exitValue() != 0) {
      die("run failed")
    }

    t.join()
  }
}

val averages =
  for {
    branch <- branches
    branchMeasures = measures(branch)
    _ = assert(branchMeasures.size == Times * Repeat)
    values = branchMeasures.sorted.drop(1).dropRight(1)
    avg = values.sum.toDouble / values.length
  } yield branch -> avg

println()
val referenceAverage = averages.head._2
for ((branch, avg) <- averages) {
  val score = 100 * (avg / referenceAverage - 1)
  val duration = Limit / avg
  println(
    Seq(
      "%.1f".format(avg),
      "%.1f%%".format(score),
      s"${duration}s",
      branch,
    ).mkString("\t")
  )
}
