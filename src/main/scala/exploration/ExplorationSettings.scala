//package exploration
//
//import scala.collection.immutable.ListMap
//
//abstract class Config {
//  val defaultParameters : ListMap[String, Any]
//  def generateConfigString : String
//}
//
//case class ExplorationSettings(
//                                primary: String,
//                                secondary: String,
//                                executionIteration: Int,
//                                depth: Int,
//                                path: String
//                              ) extends Config {
//  override def toString: String =
//    s"""${ExplorationSettings.keyRiseExploration}:
//       |    ${ExplorationSettings.defaultConfiguration.mkString("\n")}
//      """.stripMargin
//
//  override val defaultParameters: ListMap[String, Any] = ExplorationSettings.defaultConfiguration
//  override def generateConfigString : String = ExplorationSettings.generateConfigString
//}
//
//
//object ExplorationSettings {
//
//  protected[exploration] val keyRiseExploration = "rise_exploration"
//  protected[exploration] val keyPrimary = "primary"
//  protected[exploration] val keySecondary = "secondary"
//  protected[exploration] val keyDepth = "depth"
//  protected[exploration] val keyExecutionIteration = "execution_iteration"
//  protected[exploration] val keyPath = "path"
//
//  protected[exploration] val defaultPrimary = "IterativeImprovement"
//  protected[exploration] val defaultSecondary = "Random"
//  protected[exploration] val defaultExecutionIteration = 3
//  protected[exploration] val defaultDepth = 5
//  protected[exploration] val defaultPath = "exploration/"
//
//  protected[exploration] val defaultConfiguration = ListMap(
//    keyPrimary -> defaultPrimary,
//    keySecondary -> defaultSecondary,
//    keyExecutionIteration -> defaultExecutionIteration,
//    keyDepth -> defaultDepth,
//    keyPath -> defaultPath
//  )
//
//  def generateConfigString: String = utils.generateConfigString(keyRiseExploration, defaultConfiguration)
//
//    def createDefault = createWithDefaults(None, None, None, None, None, None, None, None)
//
//    def createWithDefaults(
//                            configDepth: Option[Int],
//                            configExecutionIteration: Option[Int],
//                            configPrimary: Option[String],
//                            configSecondary: Option[String],
//                            configPath: Option[String]
//                          ) = ExplorationSettings(
//
//      // priority: 1) command-line args; 2) config-file; 3) default values
//      utils.getValue(keyPrimary, configPrimary, defaultPrimary),
//      utils.getValue(keySecondary, configSecondary, defaultSecondary),
//      utils.getValue(keyDepth, configDepth, defaultDepth),
//      utils.getValue(keyExecutionIteration, configExecutionIteration, defaultExecutionIteration),
//      utils.getValue(keyPath, configPath, defaultPath))
//  //      getValue(depthFilter, configDepthFilter, defaultDepthFilter),
////      getValue(distanceFilter, configDistanceFilter, defaultDistanceFilter),
////      getValue(ruleRepetition, configRuleRepetition, defaultRuleRepetition),
////      getValue(vectorWidth, configVectorWidth, defaultVectorWidth),
////      getValue(sequential, configSequential, defaultSequential),
////      getValue(onlyLower, configOnlyLower, defaultOnlyLower),
////      getValue(ruleCollection, configRuleCollection, defaultRuleCollection))
//
//
//
//}
//
//// util to generate json file from settings
//object utils {
//  def generateConfigString(key: String, parameters: ListMap[String, Any]): String = {
//    s"""{
//       |${generateInnerJson(key, parameters)}
//       |}
//     """.stripMargin
//  }
//
//
//  def generateInnerJson(key: String, parameters: ListMap[String, Any]): String = {
//    s"""
//       |  "$key" : {
//       |${
//      parameters.zipWithIndex.map { x =>
//        val name = x._1._1
//        val value = x._1._2
//        val i = x._2
//        s"""    "$name": ${
//          value match {
//            case s: String => s""""$s"""" // wrap string args in ""
//            case _ => value.toString()
//          }
//        }${
//          if (i < parameters.size - 1) // drop comma for last parameter
//          ","
//          else ""
//        }"""
//      }.mkString("\n")
//    }
//       |  }""".stripMargin
//  }
//
//
//  def getValue[T](option: Option[T], config: T): T = {
//    if (option.isDefined && config != option.get)
//      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")
//
//    option.getOrElse(config)
//  }
//
//  def getValue[T](option: SingleValueOption[T], config: Option[T], default: T): T =
//    getValue(option.value, config, default)
//
//  def getValue[T](option: SingleValueOption[T], config: T): T =
//    getValue(option.value, config)
//
//  def getValue[T](option: FlagOption[T], config: Option[T], default: T): T =
//    getValue(option.value, config, default)
//
//  def getValue[T](option: FlagOption[T], config: T): T =
//    getValue(option.value, config)
//
//  def getValue[T](option: Option[T], config: Option[T], default: T): T = {
//    if (option.isDefined && config.isDefined && config.get != option.get)
//      println("[ExplorationParameter] Warning: Command line arg overrides existing config file arg")
//
//    option.getOrElse(config.getOrElse(default))
//  }
//
//
//
//}


