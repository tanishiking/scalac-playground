import scala.reflect.io.File
import scala.tools.nsc.{Global, Phase, Settings, SubComponent}
import scala.tools.nsc.reporters.StoreReporter

package object playground {
  def newGlobal(
      options: String = "",
      extraPhases: Global => List[(SubComponent, String)] = _ => Nil
  ): Global = {
    // [error] scala.reflect.internal.MissingRequirementError: object scala in compiler mirror not found.
    val settings = new Settings()

    // https://stackoverflow.com/questions/27934282/object-scala-in-compiler-mirror-not-found-running-scala-compiler-programmatica
    settings.processArgumentString("-usejavacp " + options)

    // CompileError(ERROR) とか warning とか info とか出してくれるやつ
    // なんで settings も必要なの?
    // StoreReporter は FilteringReporter を継承していて、FilteringReporter は
    // settings.nowarn とか settings.maxerrs とかで表示する report を filtering してくれる。だから settings が必要
    // https://github.com/scala/scala/blob/c362e07035dcc70ef713cc9c26228670b055fb8c/src/compiler/scala/tools/nsc/reporters/Reporter.scala#L90-L97
    val reporter = new StoreReporter(settings)

    // Global は scalac compiler の object、あらゆる scalac のサブコンポーネントを mixin している巨大 object
    // http://slides.com/cb372/scalac-scalamatsuri-2016#/20
    //
    // SubComponent は scalac の一つの phase を表現するもので
    // Global#phasesSet に mutable.HashSet[SubComponent] が、その Global object が実行する phase 一覧
    // https://github.com/scala/scala/blob/c362e07035dcc70ef713cc9c26228670b055fb8c/src/compiler/scala/tools/nsc/Global.scala#L732
    val g = new Global(settings, reporter) {
      // ある SubComponent を phase の中に insert する
      def addToPhasesSet1(comp: SubComponent, desc: String) =
        addToPhasesSet(comp, desc)
    }
    // 引数で与えた extra phases を global object に設定する
    for ((comp, desc) <- extraPhases(g)) g.addToPhasesSet1(comp, desc)
    // Run を instantiate して何が起きてるの?
    new g.Run
    g
  }

  def compileUntilTyper(
      code: String,
      global: Global = newGlobal()
  ): CompileResult[global.type] = {

    val run = new global.Run
    global.reporter.reset()

    val unit = global.newCompilationUnit(code)
    run.compileUnits(List(unit), run.phaseNamed("terminal"))

    val packageobjectsPhase = run.phaseNamed("packageobjects")
    val basePhases = List(run.parserPhase, run.namerPhase, packageobjectsPhase)
    val phases =
      if (unit.isJava) basePhases
      else
        basePhases :+ run.typerPhase // can't run typer for Java units in 2.11

    global.reporter.reset()

    phases.foreach(phase => {
      global.phase = phase
      global.globalPhase = phase
      phase.asInstanceOf[global.GlobalPhase].apply(unit)
    })
    global.phase = run.phaseNamed("patmat")
    global.globalPhase = run.phaseNamed("patmat")

    val tree = unit.body
    val infos = global.reporter match {
      case sr: StoreReporter => sr.infos
      case _                 => Nil
    }

    new CompileResult[global.type](
      global,
      global.reporter.hasErrors,
      unit,
      tree,
      infos.toList
    )
  }

  def compile(
      code: String,
      global: Global = newGlobal()
  ): CompileResult[global.type] = {
    val run = new global.Run

    // reporter に保存されてた messages とかを消しておく。
    global.reporter.reset()

    // source file を作る
    val source = global.newSourceFile(code)

    // Source を CompilationUnit に変換して compile を実行する
    run.compileSources(source :: Nil)

    // どうせ 1ファイルしか compile しないので head をとってくる
    val unit = run.units.toList.head

    val tree = unit.body
    val infos = global.reporter match {
      case sr: StoreReporter => sr.infos
      case _                 => Nil
    }
    // なんでここで global Run を instantiate してるの????
    new global.Run

    new CompileResult[global.type](
      global,
      global.reporter.hasErrors,
      unit,
      tree,
      infos.toList
    )

  }

  case class CompileResult[G <: Global](
      global: G,
      error: Boolean,
      unit: G#CompilationUnit,
      tree: G#Tree,
      infos: List[StoreReporter.Info]
  ) {
    def assertNoErrors(): this.type = {
      assert(!error, infos)
      this
    }

  }
}
