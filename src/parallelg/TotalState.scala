package parallelg

class TotalState(val global : GlobalState, val locals : List[LocalState]) {

  def eval : List[TotalState] =
    if (isFinal)
      List(this)
    else
      this :: this.steps.doAdmin.eval

  def steps : TotalState = {
    val newTasks = global.sparks.map(a => new LocalState(a))
    val tickedTasks = (locals ++ newTasks).map(_ tick)
    new TotalState(GlobalState(global.heap, global.globals, Nil, global.stats), tickedTasks).runStep
  }

  def runStep : TotalState = locals match {
    case Nil => this
    case x :: xs => {
      val (GlobalState(h, g, sp, st), x1) = step(GlobalState(global.heap, global.globals, global.sparks, global.stats), x)
      val acc2 = new TotalState(GlobalState(h, g, sp, st), xs).runStep
      new TotalState(acc2.global, x1 :: acc2.locals)
    }
  }

  def isFinal : Boolean = locals.isEmpty && global.sparks.isEmpty

  def step(g : GlobalState, l : LocalState) : (GlobalState, LocalState) =
    g.dispatch(l.code.head)(LocalState(l.code.tail, l.stack, l.dump, l.clock))

  def doAdmin : TotalState = {
    val (newLocals, newStats) = locals.foldRight((Nil : List[LocalState], global.stats))({
      case (local, (locals, stats)) =>
        if (local.code.isEmpty) (locals, local.clock :: stats) else (local :: locals, stats)
    })
    new TotalState(GlobalState(global.heap, global.globals, global.sparks, newStats), newLocals)
  }

}