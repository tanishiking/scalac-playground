package playground

object Main extends App {
  val global = newGlobal()
  import global._
  new global.Run

  val code =
    """|object root {
       |  object impl
       |  val f: impl.type => Unit = {
       |    case _: impl.type => ()
       |  }
       |  // f(impl)
       |}  
    """.stripMargin

  val result = compileUntilTyper(code, global)

  object Traverser extends global.Traverser {
    override def traverse(gtree: global.Tree): Unit = {
      gtree match {
        case tt: global.TypeTree if tt.original != null =>
          traverse(tt.original)
        case st: global.SingletonTypeTree =>
          println(st, st.ref.symbol, st.pos.start, st.pos.point, st.pos.end)
          super.traverse(gtree)
        case rt: global.RefTree =>
          println(rt, rt.symbol)
        case t =>
          super.traverse(gtree)
      }
    }
  }
  Traverser.traverse(result.tree)

}
