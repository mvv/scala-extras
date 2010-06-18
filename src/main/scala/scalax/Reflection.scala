package scalax

import java.lang.reflect.Method
import scalax.reflect.Property
import scala.util.control.Exception._

object Reflection {
  object implicits {
    implicit def toCastable(x: Any) = new AnyRef {
      def as[A]()(implicit m: ClassManifest[A]) = Reflection.as(x)(m)
    }

    implicit def toRichClass(clazz: Class[_]) = new AnyRef {
      def getter(name: String) = Reflection.getter(clazz, name)
      def setters(name: String) = Reflection.setters(clazz, name)
      def setter(name: String, propertyType: Class[_]) =
        Reflection.setter(clazz, name, propertyType)
      def property[V](name: String): Option[Property[V]] =
        Reflection.property(clazz, name)
    }
  }

  def as[A](x: Any)(implicit m: ClassManifest[A]): Option[A] = {
    System.out.println(Data.getClass(x) + " ||| " + m.erasure)
    if (m.erasure.isAssignableFrom(Data.getClass(x)))
      Some(x.asInstanceOf[A])
    else
      None
  }

  def getterName(name: String) = "get" + name.capitalize
  def booleanGetterName(name: String) = "is" + name.capitalize
  def setterName(name: String) = "set" + name.capitalize

  def getter(clazz: Class[_], name: String): Option[Method] =
    Stream.cons(getterName(name), Stream(booleanGetterName(name))).
      map(n => Errors.ignore(classOf[NoSuchMethodException]) { clazz.getMethod(n) }).
        find(_.isDefined).flatMap(identity)
  def setters(clazz: Class[_], name: String): Iterator[Method] = {
    val sn = setterName(name)
    clazz.getMethods.view.filter(
      m => m.getParameterTypes.size == 1 && m.getName == sn).toIterator
  }
  def setter(clazz: Class[_], name: String, propertyType: Class[_]): Option[Method] =
    Errors.ignore(classOf[NoSuchMethodException]) {
      clazz.getMethod(setterName(name), propertyType)
    }
  def property[V](clazz: Class[_], name: String): Option[Property[V]] =
    getter(clazz, name) match {
      case Some(g) =>
        Some(new Property[V](Some(g), setter(clazz, name, g.getReturnType)))
      case None =>
        setters(clazz, name).take(2).toSeq match {
          case Seq(s) => Some(new Property(None, Some(s)))
          case _ => None
        }
    }
}
