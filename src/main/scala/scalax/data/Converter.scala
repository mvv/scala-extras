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

package scalax.data

import scalax.Data
import scalax.Data.implicits._
import scalax.Errors

class ConversionException(val value: Any, val toType: Class[_],
                          msg: String, cause: Throwable) extends RuntimeException(cause) {
  def this(value: Any, toType: Class[_], msg: String) = this(value, toType, msg, null)
  def this(value: Any, toType: Class[_], cause: Throwable) = this(value, toType, null, cause)
  def this(value: Any, toType: Class[_]) = this(value, toType, null, null)

  override def getMessage =
    "[While converting '" + value + "' (" + Data.getClass(value) + ") " +
    "to type " + toType + "]" + (if (msg == null) "" else (" " + msg))
}

class OverflowException(value: Any, toType: Class[_]) extends ConversionException(value, toType)

trait Converter { self =>
  def convert[T](value: Any, toType: Class[T]): Option[T]
  def ignoreErrors: Converter = new Converter {
    def convert[T](value: Any, toType: Class[T]): Option[T] =
      Errors.ignore { self.convert(value, toType) } flatMap(identity)
  }
  def :>>(next: Converter) = new Converter {
    def convert[T](value: Any, toType: Class[T]): Option[T] = {
      self.convert(value, toType).orElse(next.convert(value, toType))
    }
  }
  def autobox(next: Converter) = new Converter {
    def convert[T](value: Any, toType: Class[T]): Option[T] = {
      self.convert(value, toType).orElse {
        val toBoxed = Data.isBoxedType(toType)
        next.convert(Data.unbox(value), Data.unboxedType(toType)).map { v =>
          if (toBoxed)
            Data.box(v)
          else
            v
        }.asInstanceOf[Option[T]]
      }
    }
  }
}

object AnyToUnitConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] = toType match {
    case c if c == classOf[Unit] => Some(()).asInstanceOf[Option[T]]
    case _ => None
  }
}

object IdentityConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] = toType match {
    case c if value == null && classOf[AnyRef].isAssignableFrom(c) =>
      Some(null.asInstanceOf[T])
    case c if value != null && toType.isAssignableFrom(Data.getClass(value)) =>
      Some(value.asInstanceOf[T])
    case _ => None
  }
}

object BoxingConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] = ((value, toType) match {
    case (x: Boolean, c) if c == classOf[java.lang.Boolean] =>
      if (x) Some(java.lang.Boolean.TRUE) else Some(java.lang.Boolean.FALSE)
    case (x: Char, c) if c == classOf[java.lang.Character] =>
      Some(new java.lang.Character(x))
    case (x: Byte, c) if c == classOf[java.lang.Byte] =>
      Some(new java.lang.Byte(x))
    case (x: Short, c) if c == classOf[java.lang.Short] =>
      Some(new java.lang.Short(x))
    case (x: Int, c) if c == classOf[java.lang.Integer] =>
      Some(new java.lang.Integer(x))
    case (x: Long, c) if c == classOf[java.lang.Long] =>
      Some(new java.lang.Long(x))
    case (x: Float, c) if c == classOf[java.lang.Float] =>
      Some(new java.lang.Float(x))
    case (x: Double, c) if c == classOf[java.lang.Double] =>
      Some(new java.lang.Double(x))
    case _ => None
  }).asInstanceOf[Option[T]]
}

object UnboxingConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] = ((value, toType) match {
    case (x: java.lang.Boolean, c) if c == classOf[Boolean] =>
      Some(x.booleanValue)
    case (x: java.lang.Character, c) if c == classOf[Char] =>
      Some(x.charValue)
    case (x: java.lang.Byte, c) if c == classOf[Byte] =>
      Some(x.byteValue)
    case (x: java.lang.Short, c) if c == classOf[Short] =>
      Some(x.shortValue)
    case (x: java.lang.Integer, c) if c == classOf[Int] =>
      Some(x.intValue)
    case (x: java.lang.Long, c) if c == classOf[Long] =>
      Some(x.longValue)
    case (x: java.lang.Float, c) if c == classOf[Float] =>
      Some(x.floatValue)
    case (x: java.lang.Double, c) if c == classOf[Double] =>
      Some(x.doubleValue)
    case _ => None
  }).asInstanceOf[Option[T]]
}

object IdentityAutoboxingConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] =
    (IdentityConverter :>> BoxingConverter :>> UnboxingConverter).
      convert(value, toType)
}

object StrictIntegralConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] = (value match {
    case x: Byte => toType match {
      case c if c == classOf[Byte] => Some(x)
      case c if c == classOf[Short] => Some(x.shortValue)
      case c if c == classOf[Int] => Some(x.intValue)
      case c if c == classOf[Long] => Some(x.longValue)
      case c if c == classOf[Float] => Some(x.floatValue)
      case c if c == classOf[Double] => Some(x.doubleValue)
      case c if c == classOf[java.math.BigInteger] =>
        Some(java.math.BigInteger.valueOf(x.longValue))
      case c if c == classOf[java.math.BigDecimal] =>
        Some(java.math.BigDecimal.valueOf(x.longValue))
      case _ => None
    }
    case x: Short => toType match {
      case c if c == classOf[Byte] =>
        Some(x.byteValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Short] => Some(x.shortValue)
      case c if c == classOf[Int] => Some(x.intValue)
      case c if c == classOf[Long] => Some(x.longValue)
      case c if c == classOf[Float] => Some(x.floatValue)
      case c if c == classOf[Double] => Some(x.doubleValue)
      case c if c == classOf[java.math.BigInteger] =>
        Some(java.math.BigInteger.valueOf(x.longValue))
      case c if c == classOf[java.math.BigDecimal] =>
        Some(java.math.BigDecimal.valueOf(x.longValue))
      case _ => None
    }
    case x: Int => toType match {
      case c if c == classOf[Byte] =>
        Some(x.byteValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Short] =>
        Some(x.shortValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Int] => Some(x.intValue)
      case c if c == classOf[Long] => Some(x.longValue)
      case c if c == classOf[Float] => Some(x.floatValue)
      case c if c == classOf[Double] => Some(x.doubleValue)
      case c if c == classOf[java.math.BigInteger] =>
        Some(java.math.BigInteger.valueOf(x.longValue))
      case c if c == classOf[java.math.BigDecimal] =>
        Some(java.math.BigDecimal.valueOf(x.longValue))
      case _ => None
    }
    case x: Long => toType match {
      case c if c == classOf[Byte] =>
        Some(x.byteValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Short] =>
        Some(x.shortValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Int] => Some(x.intValue)
        Some(x.intValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Long] => Some(x.longValue)
      case c if c == classOf[Float] => Some(x.floatValue)
      case c if c == classOf[Double] => Some(x.doubleValue)
      case c if c == classOf[java.math.BigInteger] =>
        Some(java.math.BigInteger.valueOf(x.longValue))
      case c if c == classOf[java.math.BigDecimal] =>
        Some(java.math.BigDecimal.valueOf(x.longValue))
      case _ => None
    }
    case x: java.math.BigInteger => toType match {
       case c if c == classOf[Byte] =>
        Some(x.byteValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Short] =>
        Some(x.shortValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Int] => Some(x.intValue)
        Some(x.intValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Long] => Some(x.longValue)
        Some(x.longValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Float] => Some(x.floatValue)
      case c if c == classOf[Double] => Some(x.doubleValue)
      case c if c == classOf[java.math.BigInteger] => Some(x)
      case c if c == classOf[java.math.BigDecimal] =>
        Some(new java.math.BigDecimal(x))
      case _ => None
    }
    case _ => None
  }).asInstanceOf[Option[T]]
}

object FloatingPointConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] = (value match {
    case x: Float => toType match {
      case c if c == classOf[Float] => Some(x)
      case c if c == classOf[Double] => Some(x.doubleValue)
      case c if c == classOf[java.math.BigDecimal] =>
        if (x.isNaN || x.isInfinity)
          throw new ConversionException(x, toType)
        Some(new java.math.BigDecimal(x.toString))
      case _ => None
    }
    case x: Double => toType match {
      case c if c == classOf[Float] =>
        Some(x.floatValueOption.getOrElse { new OverflowException(x, toType) })
      case c if c == classOf[Double] => Some(x.doubleValue)
      case c if c == classOf[java.math.BigDecimal] =>
        if (x.isNaN || x.isInfinity)
          throw new ConversionException(x, toType)
        Some(new java.math.BigDecimal(x.toString))
      case _ => None
    }
    case x: java.math.BigDecimal => toType match {
      case c if c == classOf[Float] => x.floatValue
      case c if c == classOf[Double] => x.doubleValue
      case c if c == classOf[java.math.BigDecimal] => Some(x)
    }
    case _ => None
  }).asInstanceOf[Option[T]]
}

object StringToBooleanConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] = ((value, toType) match {
    case (x: String, c) if c == classOf[Boolean] => 
      if (Seq("true", "yes", "on").find(x.equalsIgnoreCase(_)).isDefined)
        Some(true)
      else if (Seq("false", "no", "off").find(x.equalsIgnoreCase(_)).isDefined)
        Some(false)
      else
        throw new ConversionException(x, toType)
    case _ => None
  }).asInstanceOf[Option[T]]
}

object StringToNumericConverter extends Converter {
  def convert[T](value: Any, toType: Class[T]): Option[T] =
    if (value.isInstanceOf[String]) {
      val str = value.asInstanceOf[String]
      try {
        (toType match {
          case c if c == classOf[Byte] => Some(java.lang.Byte.parseByte(str))
          case c if c == classOf[Short] => Some(java.lang.Short.parseShort(str))
          case c if c == classOf[Int] => Some(java.lang.Integer.parseInt(str))
          case c if c == classOf[Long] => Some(java.lang.Long.parseLong(str))
          case c if c == classOf[Float] => Some(java.lang.Float.parseFloat(str))
          case c if c == classOf[Double] => Some(java.lang.Double.parseDouble(str))
          case c if c == classOf[java.math.BigInteger] =>
            Some(new java.math.BigInteger(str))
          case c if c == classOf[java.math.BigDecimal] =>
            Some(new java.math.BigDecimal(str))
          case _ => None
        }).asInstanceOf[Option[T]]
      } catch {
        case e: NumberFormatException =>
          throw new ConversionException(value, toType, e)
      }
    } else
      None
}
