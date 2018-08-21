package com.cibo.evilplot.plot.aesthetics

/** Either a value, or how that value can be accessed from a [[Theme]]. */
final case class ThemedValue[A](private val backing: Either[A, Theme => A]) {

  /** Whether this ThemedValue is backed immediately by a concrete value. */
  def hasValue: Boolean = backing.isLeft

  def map[B](f: A => B): ThemedValue[B] = backing match {
    case Left(a)          => f(a)
    case Right(themeFunc) => themeFunc andThen f
  }

  def flatMap[B](f: A => ThemedValue[B]): ThemedValue[B] = (t: Theme) => f(get(t)).get(t)
  def zip[B](that: ThemedValue[B]): ThemedValue[(A, B)] = (t: Theme) => get(t) -> that.get(t)

  def get(theme: Theme): A = backing match {
    case Left(a)   => a
    case Right(fn) => fn(theme)
  }
}
object ThemedValue {
  import scala.language.implicitConversions
  implicit def _themedValueFromPlainValue[A](a: A): ThemedValue[A] = ThemedValue(Left(a))
  implicit def _themedValueFromLookupFunction[A](lookup: Theme => A): ThemedValue[A] =
    ThemedValue(Right(lookup))
  implicit def _themedValueFromFunction1[In, A](f: In => A): In => ThemedValue[A] =
    f andThen _themedValueFromPlainValue

  implicit def _materializeThemedValue[A](tv: ThemedValue[A])(implicit theme: Theme): A = tv.get(theme)

  def sequence[A](seq: Seq[ThemedValue[A]]): ThemedValue[Seq[A]] = {
    seq.foldLeft(ThemedValue(Left(Seq.empty[A]))) { (acc, next) =>
      acc.flatMap(s => (t: Theme) => s :+ next.get(t))
    }
  }
}
