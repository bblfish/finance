
import com.github.tototoshi.csv.*
import scopt.OParser

import java.io.{File, InputStream}
import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.{Calendar, Date}
import scala.util.{Success, Try}

case class Config(
 csv: Option[File] = None,
 startingAmount: Float = 0.0,
 start: LocalDate = LocalDate.now(),
 months: Int = 6
)

def argsParser(): OParser[Unit, Config] =
   given scopt.Read[LocalDate] = scopt.Read.reads(str => LocalDate.parse(str, Account.df))
   
   given scopt.Read[Float] = scopt.Read.reads(str => java.lang.Float.parseFloat(str))
   
   import scopt.OParser
   val builder = OParser.builder[Config]
   import builder.*
   OParser.sequence(
      programName("accounts"),
      head("accounts", "0.1"),
      arg[File]("<file>")
       .required()
       .action((f, c) => c.copy(csv = Some(f)))
       .text("file containing csv data in required format"),
      opt[Float]('a', "amount")
       .action((amount, c) => c.copy(startingAmount = amount))
       .text("starting value in account"),
      opt[LocalDate]('s', "start")
       .action((date, c) => c.copy(start = date))
       .text("date in yyyy/mm/dd format to start from"),
      opt[Int]('m', "months")
       .action((i, c) => c.copy(months = i))
       .validate(i => if i < 0 then Left("number of months must be positive") else Right(()))
       .text("months from start date to display")
   )
end argsParser

@main
def run(args: String*): Unit =
   given CSVFormat = new TSVFormat {
      override val delimiter: Char = ':'
   }
   
   for config <- OParser.parse(argsParser(), args, Config()) yield
      val csvFile = CSVReader.open(config.csv.get)
      val data: Seq[Map[String, String]] = csvFile.toStreamWithHeaders
      val fintrys: Seq[Try[Account.Order]] = data.map(Account.interpret)
      fintrys.zipWithIndex.foreach { (dt: Try[Account.Order], i: Int) =>
         if dt.isFailure then println(s"failure on line $i is $dt")
      }
      val gdata: Seq[Account.Order] = fintrys.collect { case Success(v) => v }
      val stream: LazyList[Account.State] = Account.states(Account.State(config.start, config.startingAmount, List()), gdata)
      // we take months + 1 because we start at 0
      val coveredPeriod: List[Account.State] = stream.take(config.months + 1).toList
      import System.out
      import math.Fractional.Implicits.infixFractionalOps
      import math.Numeric.Implicits.infixNumericOps
      import math.Integral.Implicits.infixIntegralOps
      out.println(coveredPeriod.map(Account.pretty).mkString("\n"))
      out.println("""
         |
         |TOTAL
         |=====
         |""".stripMargin)
      val transactions: List[Float] = coveredPeriod.flatMap(_.activated.map(_.cost))
      val income = transactions.filter(_ >= 0f).fold(0f)(_+_)
      val outgoing = transactions.filter(_ < 0f).fold(0f)(_+_)
      out.println("income: "+ income + " average: "+ income/config.months +" over "+config.months + " months")
      out.println("expend: "+ outgoing + " average: "+ outgoing/config.months )
end run

object Account:
   val df = DateTimeFormatter.ofPattern("yyyy/MM/dd")
   
   enum Repeat:
      case Week, Month, Year
      case Once(date: LocalDate)
   
   enum Step:
      case Day, Week, Month
   
   case class Order(
    subj: String,
    cost: Float,
    repeat: Repeat,
    start: Option[LocalDate], end: Option[LocalDate],
    description: String
   )
   
   case class State(at: LocalDate, value: Float, activated: Seq[Order])
   
   def states(start: State, rules: Seq[Order], step: Step = Step.Month): LazyList[State] =
      LazyList.unfold(start) { (start: State) =>
         // nextCal is stateful
         val nextDt = step match
            case Step.Day => start.at.plusDays(1)
            case Step.Week => start.at.plusWeeks(1)
            case Step.Month => start.at.plusMonths(1)
         val active: Seq[Order] = rules.filter { (order: Order) =>
            order.end.fold(true)(end => nextDt.compareTo(end) <= 0)
             && order.start.fold(true)(start => nextDt.compareTo(start) > 0)
             && {
               order.repeat match
                  case Repeat.Once(d) => d.compareTo(start.at) > 0 && d.compareTo(nextDt) <= 0
                  case _ => true
            }
         }
         val newValue = active.foldLeft(start.value) { (v, fd) =>
            fd.repeat match
               case Repeat.Month => fd.cost + v
               case Repeat.Once(d) => fd.cost + v
               case Repeat.Week => v + (fd.cost * (start.at.until(nextDt).getDays / 7))
               case _ => v //todo: deal with year
         }
         Some((start, State(nextDt, newValue, active)))
      }
   
   object StrChar {
      def unapplySeq(str: String): Seq[Char] = str.toSeq
   }
   
   /** interpret a row in the table, */
   def interpret(row: Map[String, String]): Try[Order] = Try {
      Order(
         row("Topic"),
         row("Amount").toFloat,
         row("Repeat") match
            case StrChar('W', _*) => Repeat.Week
            case StrChar('M', _*) => Repeat.Month
            case StrChar('Y', _*) => Repeat.Year
            case x => Repeat.Once(LocalDate.parse(x, df))
         ,
         row("Start") match
            case "" => None
            case date => Some(LocalDate.parse(date, df))
         ,
         row("Ending") match
            case "" => None
            case date => Some(LocalDate.parse(date, df))
         ,
         row.get("Description").getOrElse("")
      )
   }
   
   def pretty(st: State): String =
      import st.*
      s"""$at: remaining: € $value
          |     out: € ${-st.activated.foldLeft(0.0) { (n, o) => if o.cost < 0 then n + o.cost else n }}
          |     in : € ${st.activated.foldLeft(0.0) { (n, o) => if o.cost > 0 then n + o.cost else n }}
          |""".stripMargin + st.activated.toList.map(o =>
         o.subj + ":\t € " + o.cost + s" ${o.repeat}\t${o.description}"
      ).mkString("   ", "\n   ", "")

end Account

