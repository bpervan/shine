//package exploration.util
//
//class CommandLineParser {
//  // dummy
//
//}


// move this to other location
//
//object CompilationGoal extends Enumeration {
//  type CompilationGoal = Value
//  val C, OpenMP, OpenCL = Value
//}
//
//object Executor extends Enumeration {
//  type Executor = Value
//  val C, OpenMP, OpenCL = Value
//}
//case class Config(
//                   foo: Int = -1,
//                   out: File = new File("."),
//                   xyz: Boolean = false,
//                   libName: String = "",
//                   maxCount: Int = -1,
//                   verbose: Boolean = false,
//                   debug: Boolean = false,
//                   mode: String = "",
//                   files: Seq[File] = Seq(),
//                   keepalive: Boolean = false,
//                   jars: Seq[File] = Seq(),
//                   kwargs: Map[String, String] = Map(),
//                   goal: CompilationGoal = CompilationGoal.C)
//
//
//
//object parser  {
//  import scopt.OParser
//  import CompilationGoal._
//
//  implicit val compilationGoalRead: scopt.Read[CompilationGoal.Value] =
//    scopt.Read.reads(CompilationGoal withName _)
//
//
//  val builder = OParser.builder[Config]
//  val parser1 = {
//    import builder._
//    OParser.sequence(
//      programName("shine"),
//      head("shine", "1.0"),
//      opt[File]('o', "output")
//        .valueName("<file>")
//        .action((x, c) => c.copy(out = x))
//        .text("output file"),
//      opt[CompilationGoal]('g', "goal")
//        .valueName("C|OpenMP|OpenCL")
//        .action((x, c) => c.copy(goal = x))
//        .text("compilation goal"),
//      opt[Unit]("verbose")
//        .action((_, c) => c.copy(verbose = true))
//        .text("verbose is a flag"),
//      help("help").text("prints this usage text"),
//      arg[File]("<input-file>")
//        .unbounded()
//        .required()
//        .action((x, c) => c.copy(files = c.files :+ x))
//        .text("input"),
//      note("some notes - information here " + sys.props("line.separator")),
//      cmd("explore")
//        .action((_, c) => c.copy(mode = "explore"))
//        .text("explore is a command.")
//        .children(
//          opt[Unit]("search heuristic")
//            .abbr("sh")
//            .action((_, c) => c.copy(keepalive = false))
//            .text("choose search heuristic"),
//          opt[File]('f', "file")
//            .valueName("<settings-file>")
//            .action((x, c) => c.copy(out = x))
//            .text("settings file for exploration"),
//          opt[Boolean]("verbose")
//            .action((x, c) => c.copy(xyz = x))
//            .text("print information during exploration"),
//          checkConfig(
//            c =>
//              if (c.keepalive && c.xyz) failure("xyz cannot keep alive - whatever?")
//              else success)
//        )
//    )
//  }
//
//}



