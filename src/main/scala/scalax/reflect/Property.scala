package scalax.reflect

import java.lang.reflect.Method
import scalax.Data

class Property[V] private[scalax](val getter: Option[Method],
                                  val setter: Option[Method]) {
  require(getter.isDefined || setter.isDefined)
  val propertyType = getter.map(_.getReturnType).
                       getOrElse(setter.get.getParameterTypes()(0)).
                         asInstanceOf[Class[V]]
  def isReadable = getter.isDefined
  def isWritable = setter.isDefined
  def isReadOnly = setter.isEmpty
  def isWriteOnly = getter.isEmpty
  def isReadWrite = getter.isDefined && setter.isDefined
  def readOnly = if (setter.isDefined) new Property[V](None, setter)
                 else this
  def writeOnly = if (getter.isDefined) new Property[V](getter, None)
                  else this
  def get(obj: AnyRef): V = getter.get.invoke(obj).asInstanceOf[V]
  def set(obj: AnyRef, x: V): Unit = setter.get.invoke(obj, Data.box(x))
}

