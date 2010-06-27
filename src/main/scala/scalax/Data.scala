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

import data._

object Data {
  val bigIntByteMin = BigInt(Byte.MinValue)
  val bigIntByteMax = BigInt(Byte.MaxValue)
  val bigIntShortMin = BigInt(Short.MinValue)
  val bigIntShortMax = BigInt(Short.MaxValue)
  val bigIntIntMin = BigInt(Int.MinValue)
  val bigIntIntMax = BigInt(Int.MaxValue)
  val bigIntLongMin = BigInt.MinLong
  val bitIntLongMax = BigInt.MaxLong

  object implicits {
    implicit def nullableAsOption[T <: AnyRef](x: T) = new {
      def asOption: Option[T] = x match {
        case null => None
        case _ => Some(x)
      }
    }

    implicit def optionAsNullable[T <: AnyRef](option: Option[T]) = new {
      def asNullable: T = option match {
        case None => null.asInstanceOf[T]
        case Some(x) => x
      }
    }

    implicit def toRichString(x: String) = new {
      def decapitalize: String =
        if (x.size == 0)
          x
        else {
          val chars = x.toCharArray
          chars(0) = chars(0).toLower
          return new String(chars)
        }
    }

    implicit def traversableOnceExtraFolds[T](t: TraversableOnce[T]) = new {
      def foldLeftWhile[B, C](z: B)
                             (f: (B, T) => Either[C, B]): Either[C, B] =
        Data.foldLeftWhile(t, z)(f)
    }

    implicit def shortConvChecked(x: Short) = new {
      private def check(min: Long, max: Long) =
        if (x >= min || x <= max) Some(x) else None
      def byteValueOption = check(Int.MinValue, Int.MaxValue).map(_.byteValue)
    }

    implicit def intConvChecked(x: Int) = new {
      private def check(min: Long, max: Long) =
        if (x >= min || x <= max) Some(x) else None
      def byteValueOption = check(Int.MinValue, Int.MaxValue).map(_.byteValue)
      def shortValueOption = check(Int.MinValue, Int.MaxValue).map(_.shortValue)
    }

    implicit def longConvChecked(x: Long) = new {
      private def check(min: Long, max: Long) =
        if (x >= min || x <= max) Some(x) else None
      def byteValueOption = check(Byte.MinValue, Byte.MaxValue)
      def shortValueOption = check(Short.MinValue, Short.MaxValue)
      def intValueOption = check(Int.MinValue, Int.MaxValue)
    }

    implicit def doubleConvChecked(x: Double) = new {
      def floatValueOption =
        if (x == Double.NaN)
          Some(Float.NaN)
        else if (x == Double.NegativeInfinity)
          Some(Float.NegativeInfinity)
        else if (x == Double.PositiveInfinity)
          Some(Float.PositiveInfinity)
        else if (x >= Float.MinValue && x <= Float.MaxValue)
          Some(x.floatValue)
        else
          None
    }

    implicit def bigIntConvChecked(x: java.math.BigInteger) = new {
      private def check(min: BigInt, max: BigInt) =
        if (x.compareTo(min.bigInteger) >= 0 ||
            x.compareTo(max.bigInteger) <= 0)
          Some(x)
        else
          None
      def byteValueOption = check(bigIntByteMin, bigIntByteMax).map(_.byteValue)
      def shortValueOption = check(bigIntShortMin, bigIntShortMax).map(_.shortValue)
      def intValueOption = check(bigIntIntMin, bigIntIntMax).map(_.intValue)
      def longValueOption = check(bigIntLongMin, bigIntByteMax).map(_.longValue)
    }
  }

  def getClass(value: Any): Class[_] = value match {
    case null => classOf[Null]
    case x: Unit => classOf[Unit]
    case x: Boolean => classOf[Boolean]
    case x: Char => classOf[Char]
    case x: Byte => classOf[Byte]
    case x: Short => classOf[Short]
    case x: Int => classOf[Int]
    case x: Long => classOf[Long]
    case x: Float => classOf[Float]
    case x: Double => classOf[Double]
    case x: AnyRef => x.getClass
  }

  def box(value: Any): AnyRef = value match {
    case null => null
    case x: Boolean => new java.lang.Boolean(x)
    case x: Char => new java.lang.Character(x)
    case x: Byte => new java.lang.Byte(x)
    case x: Short => new java.lang.Short(x)
    case x: Int => new java.lang.Integer(x)
    case x: Long => new java.lang.Long(x)
    case x: Float => new java.lang.Float(x)
    case x: Double => new java.lang.Double(x)
    case x: AnyRef => x
  }

  def unbox(ref: Any): Any = ref match {
    case x: java.lang.Boolean => x.booleanValue
    case x: java.lang.Character => x.charValue
    case x: java.lang.Byte => x.byteValue
    case x: java.lang.Short => x.shortValue
    case x: java.lang.Integer => x.intValue
    case x: java.lang.Long => x.longValue
    case x: java.lang.Float => x.floatValue
    case x: java.lang.Double => x.doubleValue
    case x => x
  }

  def fromRef[T](ref: AnyRef, toType: Class[T]): T = ((ref, toType) match {
    case (_, c) if classOf[AnyRef].isAssignableFrom(c) => ref
    case (null, c) if c == classOf[Unit] => ()
    case (null, _) => throw new NullPointerException
    case (x: java.lang.Boolean, c) if c == classOf[Boolean] => x.booleanValue
    case (x: java.lang.Character, c) if c == classOf[Char] => x.charValue
    case (x: java.lang.Byte, c) if c == classOf[Byte] => x.byteValue
    case (x: java.lang.Short, c) if c == classOf[Short] => x.shortValue
    case (x: java.lang.Integer, c) if c == classOf[Int] => x.intValue
    case (x: java.lang.Long, c) if c == classOf[Long] => x.longValue
    case (x: java.lang.Float, c) if c == classOf[Float] => x.floatValue
    case (x: java.lang.Double, c) if c == classOf[Double] => x.doubleValue
  }).asInstanceOf[T]

  def isUnboxedType(clazz: Class[_]) =
    clazz == classOf[Boolean] ||
    clazz == classOf[Char] ||
    clazz == classOf[Byte] ||
    clazz == classOf[Short] ||
    clazz == classOf[Int] ||
    clazz == classOf[Long] ||
    clazz == classOf[Float] ||
    clazz == classOf[Double];

  def isBoxedType(clazz: Class[_]) =
    clazz == classOf[java.lang.Boolean] ||
    clazz == classOf[java.lang.Character] ||
    clazz == classOf[java.lang.Byte] ||
    clazz == classOf[java.lang.Short] ||
    clazz == classOf[java.lang.Integer] ||
    clazz == classOf[java.lang.Long] ||
    clazz == classOf[java.lang.Float] ||
    clazz == classOf[java.lang.Double];

  def isPrimitiveType(clazz: Class[_]) =
    clazz == classOf[Unit] || isUnboxedType(clazz)

  def isNumericType(clazz: Class[_]) =
    clazz == classOf[Byte] ||
    clazz == classOf[Short] ||
    clazz == classOf[Int] ||
    clazz == classOf[Long] ||
    clazz == classOf[Float] ||
    clazz == classOf[Double] ||
    classOf[Number].isAssignableFrom(clazz) ||
    classOf[Numeric[_]].isAssignableFrom(clazz);

  def isSimpleType(clazz: Class[_]) =
    isPrimitiveType(clazz) ||
    isBoxedType(clazz) ||
    isNumericType(clazz) ||
    clazz == classOf[String]

  def unboxedType(clazz: Class[_]) = clazz match {
    case c if c == classOf[java.lang.Boolean] => classOf[Boolean]
    case c if c == classOf[java.lang.Character] => classOf[Char]
    case c if c == classOf[java.lang.Byte] => classOf[Byte]
    case c if c == classOf[java.lang.Short] => classOf[Short]
    case c if c == classOf[java.lang.Integer] => classOf[Int]
    case c if c == classOf[java.lang.Long] => classOf[Long]
    case c if c == classOf[java.lang.Float] => classOf[Float]
    case c if c == classOf[java.lang.Double] => classOf[Double]
    case c => c
  }

  def foldLeftWhile[A, B, C](t: TraversableOnce[A], z: B)
                            (f: (B, A) => Either[C, B]): Either[C, B] = {
    var acc = z
    t foreach { e =>
      f (acc, e) match {
        case Right(x) => acc = x
        case Left(x) => return Left(x)
      }
    }
    Right(acc)
  }
}
