import java.text.SimpleDateFormat
import java.util.Calendar

import fabricator.Fabricator
import org.apache.commons.math3.stat.inference.ChiSquareTest
import org.sameersingh.scalaplot.Style.{LineType, PointType}
import org.sameersingh.scalaplot._
import org.sameersingh.scalaplot.Implicits._

/**
  * Created by ilyas on 2017-02-18.
  */
object Config {
    val maxBranches: Int = 7
    val branchingSeed: Double = 0.1
    val specChars: String = ((0 to 47)++(58 to 64)++(91 to 96)++(123 to 255)).map(_.toChar).mkString("")//"[-~!@#$^%&*()_+={}\\\\[\\\\]|;:\\\"'`<,>.?/\\\\\\\\ '’‘ ̣ ̃]".r
    val uppercaseChars: Set[Char] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray.toSet
    val lowercaseChars: Set[Char] = "abcdefghijklmnopqrstuvwxyz".toCharArray.toSet
    val numbers: Set[Char] = "0123456789".toCharArray.toSet
    val inc : Int = 100
    val tts : Boolean = true
    val capturePct: Double = 0.8
    val acceptableGenerators: List[()=>String] = List(
        () => Fabricator.calendar().randomDate.asString,
        () => Fabricator.calendar().time24h,
        () => Fabricator.contact().firstName,
        () => Fabricator.contact().address,
        () => Fabricator.contact().eMail,
        () => Fabricator.contact().fullName(setPrefix = false, setSuffix = false),
        () => Fabricator.contact().weight(true),
        () => Fabricator.internet().UUID,
        () => Fabricator.internet().hashtag,
        () => Fabricator.internet().ipv6
    )

    def neededSampleSize(std: Double): Double = Math.pow(1.96*std/0.05, 2.0)
}

object Utils {
    // Returns the character class of a given character
    def getCharacterClass(c: Char): CharClass = c.toInt match {
        case x if 48 to 57 contains x => NUM
        case x if 65 to 90 contains x => UCC
        case x if 97 to 122 contains x => LCC
        case _ => SPC(c.toString)
    }

    // Performs Chi-Square test expecting a uniform distribution
    def significant(observed: Seq[Long]): Boolean = if (observed.length < 2) false else
        new ChiSquareTest().chiSquareTest(Array.fill(observed.length)(1.0), observed.toArray, 0.01)

    // Potentially clean up later?
    def asciiMap: Map[Char, Long] = (0 to 256).map(i => (i.toChar, 0: Long)).toMap

    // Percentiles calculator
    def calcPercentiles[T](arr: Seq[T], ps: Seq[Double])(implicit ev: T => Ordered[T]): Seq[T] =
        ps.map(p => arr.sorted[T].apply((arr.length * p).toInt))

    // Nanoseconds to seconds
    def nsToS(ns: Long): Double = ns/math.pow(10,9)

    // Formatting date for output graphs
    def formattedDate(): String = {
        val format = new SimpleDateFormat("yy_MM_dd_hh_mm_ss")
        format.format(Calendar.getInstance().getTime)
    }

    // Round-robin iterator
    def mergeStreams[T](streams: Seq[Stream[T]], next: Int = 0, done: Boolean = true): Stream[T] =
        if (streams.isEmpty) Stream.empty
        else (streams.length - next, streams(next), done) match {
            case (1, Stream.Empty, true) => Stream.empty
            case (1, Stream.Empty, false) => mergeStreams(streams, (next+1) % streams.length)
            case (_, Stream.Empty, _) => mergeStreams(streams, (next+1) % streams.length, done = done)
            case (_, s: Stream[T], _) => s.head #:: mergeStreams(streams.updated(next, s.tail), (next+1) % streams.length, done = false)
        }

    def plotSeries(serieses: Map[String, XYSeries], path: String, name: String, x: NumericAxis, y: NumericAxis): Unit = {
        val colors: List[Style.Color.Type] = List(Style.Color.RoyalBlue, Style.Color.Red, Style.Color.Green)
        val styledSeries: Seq[XYSeries] = serieses.toSeq.zipWithIndex.map(s => {
            val series = new MemXYSeries(s._1._2.points.toSeq, s._1._1)
            series.pointType = PointType.emptyO
            series.lineType = LineType.Solid
            series.lineWidth = 2.0
            series.color = colors(s._2)
            series.pointSize = 1.0
            series
        })
        val chart: XYChart = xyChart(
            styledSeries,
            x = x,
            y = y
        )
        chart.showLegend = serieses.size > 1
        output(GUI, chart)
    }

    // Calculate Precision and Recall
    def calcPR(scores: List[(Double,Boolean)],threshold: Double): (Double, Double) = {
        val tp = scores.filter(_._1 > threshold).count(_._2).toDouble
        val fp = scores.filter(_._1 > threshold).count(!_._2).toDouble
        val fn = scores.filter(_._1 <= threshold).count(_._2).toDouble
        (tp/(tp+fn), tp/(tp+fp))
    }
}