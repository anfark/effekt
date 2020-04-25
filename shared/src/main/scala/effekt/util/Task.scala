package effekt.util

import effekt.context.Context
import effekt.util.messages.FatalPhaseError

import scala.collection.mutable

import org.bitbucket.inkytonik.kiama.util.{ Source, FileSource, StringSource }

trait Task[In, Out] { self =>

  /**
   * The name of this task
   */
  def taskName: String

  /**
   * The result indicates whether running this task was successful.
   *
   * Error messages are written to the context
   *
   * Can throw FatalPhaseError to abort execution of the task
   */
  def run(input: In)(implicit C: Context): Option[Out]

  /**
   * Apply this task
   */
  def apply(input: In)(implicit C: Context): Option[Out] = try {
    Task.need(this, input)
  } catch {
    case FatalPhaseError(msg) =>
      C.error(msg)
      None
  }

  def fingerprint(key: In): Long

  /**
   * Helper method to find the currently implicit context
   */
  def Context(implicit ctx: Context): Context = ctx

  /**
   * sequentially composes two tasks
   */
  def andThen[Out2](other: Task[Out, Out2]): Task[In, Out2] = new Task[In, Out2] {
    val taskName = s"${self.taskName} andThen ${other.taskName}"

    def run(input: In)(implicit C: Context): Option[Out2] =
      self.run(input).flatMap(other.run)

    def fingerprint(key: In): Long = self.fingerprint(key)
  }

  override def toString = taskName
}

abstract class SourceTask[Out](name: String) extends Task[Source, Out] {
  val taskName = name

  // On file sources, we use the timestamp to approximate content changes
  // We currently can't simply use a content hash, since
  // in kiama.FilesSource the content is only read once! and then stored in a `val`.
  // It will not update, if the external file changes.
  def fingerprint(source: Source) = paths.lastModified(source)

}

abstract class HashTask[In, Out](name: String) extends Task[In, Out] {
  val taskName = name
  def fingerprint(in: In) = in.hashCode
}

object Task { build =>

  private def log(msg: => String) = () // println(msg)

  /**
   * A concrete target / request to be build
   */
  case class Target[K, V](task: Task[K, V], key: K) {
    override def toString = s"${task}@${fingerprint}"
    def fingerprint: Long = task.fingerprint(key)
  }

  /**
   * A heterogenous store from Target to Trace
   */
  val db = mutable.HashMap.empty[Target[Any, Any], Trace[Any]]

  case class Info(target: Target[_, _], hash: Long) {
    def isValid: Boolean = target.fingerprint == hash
    override def toString =
      if (isValid) target.toString else s"${target}#${Console.RED_B}${Console.WHITE}${hash}${Console.RESET}"
  }

  // The datatype Trace is adapted from the paper "Build systems a la carte" (Mokhov et al. 2018)
  // currently the invariant that Info.target.V =:= V is not enforced
  case class Trace[V](current: Info, depends: List[Info], value: Option[V]) {
    def trace = current :: depends
    def isValid: Boolean = current.isValid && depends.forall { _.isValid }
    override def toString = {
      s"Trace($current) { ${depends.mkString("; ")} }"
    }
  }

  private def coerce[A, B](a: A): B = a.asInstanceOf[B]

  def get[K, V](target: Target[K, V]): Option[Trace[V]] =
    coerce(db.get(coerce(target)))

  def update[K, V](target: Target[K, V], trace: Trace[V]): Unit =
    db.update(coerce(target), coerce(trace))

  /**
   * A trace recording information about computed targets
   */
  private var trace: List[Info] = Nil

  def compute[K, V](target: Target[K, V])(implicit C: Context): Option[V] = {
    var before = trace
    log(s"computing for ${target}")

    // we start with an empty trace for this target
    trace = Nil
    val res = target.task.run(target.key)
    val tr = Trace(Info(target, target.fingerprint), trace, res)
    build.update(target, tr)

    // for the potential parent task, we append our trace to the existing one
    trace = before ++ tr.trace
    res
  }

  def reuse[V](tr: Trace[V]): Option[V] = {
    log(s"reusing ${tr.current.target}")
    // replay the trace
    trace ++= tr.trace
    tr.value
  }

  def need[K, V](task: Task[K, V], key: K)(implicit C: Context): Option[V] = need(Target(task, key))

  def need[K, V](target: Target[K, V])(implicit C: Context): Option[V] = get(target) match {
    case Some(trace) if !trace.isValid =>
      log(s"Something changed for ${target}")
      compute(target)
    case Some(trace) =>
      reuse(trace)
    case None =>
      compute(target)
  }

  def dump() = db.foreach { case (k, v) => println(k.toString + " -> " + v) }
}