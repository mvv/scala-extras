/*
 * Copyright (C) 2010 Mikhail Vorozhtsov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scalax

import java.lang.reflect.Method
import scalax.reflect.Property

object Reflection {
  object implicits {
    implicit def toCastable(x: Any) = new {
      def as[A]()(implicit m: ClassManifest[A]) = Reflection.as(x)(m)
    }

    implicit def toRichClass(clazz: Class[_]) = new {
      def getter(name: String) = Reflection.getter(clazz, name)
      def setters(name: String) = Reflection.setters(clazz, name)
      def setter(name: String, propertyType: Class[_]) =
        Reflection.setter(clazz, name, propertyType)
      def property[V](name: String): Option[Property[V]] =
        Reflection.property(clazz, name)
      def readOnlyProperties: Iterator[Property[_]] =
        Reflection.readOnlyProperties(clazz)
    }
  }

  def as[A](x: Any)(implicit m: ClassManifest[A]): Option[A] =
    if (m.erasure.isAssignableFrom(Data.getClass(x)))
      Some(x.asInstanceOf[A])
    else
      None

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
  def readOnlyProperties(clazz: Class[_]): Iterator[Property[_]] =
    Iterator.empty ++
    clazz.getMethods.iterator.filter(m =>
      m.getParameterTypes.size == 0 &&
      m.getReturnType != classOf[Unit] && {
        val name = m.getName
        (name.startsWith("get") && name.size >= 4 && name(3).isUpper) ||
        (name.startsWith("is") && name.size >= 3 && name(2).isUpper &&
          (m.getReturnType == classOf[Boolean] ||
           m.getReturnType == classOf[java.lang.Boolean]))
      }).map(m => new Property(Some(m), None))
}
