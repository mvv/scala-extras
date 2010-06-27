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

package scalax.reflect

import java.lang.reflect.Method
import scalax.Data
import scalax.Data.implicits._

class Property[V] private[scalax](val getter: Option[Method],
                                  val setter: Option[Method]) {
  type Value = V
  require(getter.isDefined || setter.isDefined)
  val (propertyType, name) =
    getter.map(m => (m.getReturnType, m.getName.substring(3).decapitalize)).
      getOrElse({
        val m = setter.get
        (m.getParameterTypes()(0), m.getName.substring(3).decapitalize)
      }).asInstanceOf[(Class[Value], String)]
  def isReadable = getter.isDefined
  def isWritable = setter.isDefined
  def isReadOnly = setter.isEmpty
  def isWriteOnly = getter.isEmpty
  def isReadWrite = getter.isDefined && setter.isDefined
  def readOnly = if (setter.isDefined) new Property[V](None, setter)
                 else this
  def writeOnly = if (getter.isDefined) new Property[V](getter, None)
                  else this
  def get(obj: AnyRef): Value = Data.fromRef(getter.get.invoke(obj), propertyType)
  def set(obj: AnyRef, x: Value): Unit = setter.get.invoke(obj, Data.box(x))
}

