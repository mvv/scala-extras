package scalax.data

import scala.collection.JavaConversions._
import scalax.reflect.Property
import scalax.Reflection
import scalax.Data

object JSON {
  object implicits {
    implicit def boolFieldToJs(p: (String, Boolean)) = p._1 -> JsBool(p._2)
    implicit def strFieldToJs(p: (String, String)) = p._1 -> JsStr(p._2)
    implicit def byteFieldToJs(p: (String, Byte)) = p._1 -> JsNum(p._2)
    implicit def shortFieldToJs(p: (String, Short)) = p._1 -> JsNum(p._2)
    implicit def intFieldToJs(p: (String, Int)) = p._1 -> JsNum(p._2)
    implicit def longFieldToJs(p: (String, Long)) = p._1 -> JsNum(p._2)
    implicit def floatFieldToJs(p: (String, Float)) = p._1 -> JsNum(p._2)
    implicit def doubleFieldToJs(p: (String, Double)) = p._1 -> JsNum(p._2)
    implicit def bigIntegerToJs(p: (String, java.math.BigInteger)) =
      p._1 -> JsNum(p._2)
    implicit def bigIntToJs(p: (String, BigInt)) = p._1 -> JsNum(p._2)
    implicit def bigDecimalToJs(p: (String, java.math.BigDecimal)) =
      p._1 -> JsNum(p._2)
    implicit def bigDecToJs(p: (String, BigDecimal)) = p._1 -> JsNum(p._2)
 
    implicit def lazyJsValuePair(x: JsValue) = new {
      def |#(next: => JsValue): Iterator[JsValue] =
        Iterator.single(x) ++ Iterator.single(next)
    }
    implicit def lazyItJsValuePair(it: Iterator[JsValue]) = new {
      def |#(next: => JsValue): Iterator[JsValue] =
        it ++ Iterator.single(next)
    }
    implicit def lazyBoolPair(x: Boolean) = new {
      def |#(next: => JsValue): Iterator[JsValue] =
        Iterator.single(JsBool(x)) ++ Iterator.single(next)
    }
    implicit def lazyStrPair(x: String) = new {
      def |#(next: => JsValue): Iterator[JsValue] =
        Iterator.single(JsStr(x)) ++ Iterator.single(next)
    }
    implicit def lazyBytePair(x: Byte) = new {
      def |#(next: => JsValue): Iterator[JsValue] =
        Iterator.single(JsNum(x)) ++ Iterator.single(next)
    }
    implicit def lazyShortPair(x: Short) = new {
      def |#(next: => JsValue): Iterator[JsValue] =
        Iterator.single(JsNum(x)) ++ Iterator.single(next)
    }
    implicit def lazyIntPair(x: Int) = new {
      def |#(next: => JsValue): Iterator[JsValue] =
        Iterator.single(JsNum(x)) ++ Iterator.single(next)
    }
    implicit def lazyLongPair(x: Long) = new {
      def |#(next: => JsValue): Iterator[JsValue] =
        Iterator.single(JsNum(x)) ++ Iterator.single(next)
    }
    implicit def lazyNameJsValuePair(x: (String, JsValue)) = new {
      def |#(next: => (String, JsValue)): Iterator[(String, JsValue)] =
        Iterator.single(x) ++ Iterator.single(next)
    }
    implicit def lazyItNameJsValuePair(it: Iterator[(String, JsValue)]) = new {
      def |#(next: => (String, JsValue)): Iterator[(String, JsValue)] =
        it ++ Iterator.single(next)
    }
    implicit def lazyNameBoolPair(x: (String, Boolean)) = new {
      def |#(next: => (String, JsValue)): Iterator[(String, JsValue)] =
        Iterator.single((x._1, JsBool(x._2))) ++ Iterator.single(next)
    }
    implicit def lazyNameStrPair(x: (String, String)) = new {
      def |#(next: => (String, JsValue)): Iterator[(String, JsValue)] =
        Iterator.single((x._1, JsStr(x._2))) ++ Iterator.single(next)
    }
    implicit def lazyNameBytePair(x: (String, Byte)) = new {
      def |#(next: => (String, JsValue)): Iterator[(String, JsValue)] =
        Iterator.single((x._1, JsNum(x._2))) ++ Iterator.single(next)
    }
    implicit def lazyNameShortPair(x: (String, Short)) = new {
      def |#(next: => (String, JsValue)): Iterator[(String, JsValue)] =
        Iterator.single((x._1, JsNum(x._2))) ++ Iterator.single(next)
    }
    implicit def lazyNameIntPair(x: (String, Int)) = new {
      def |#(next: => (String, JsValue)): Iterator[(String, JsValue)] =
        Iterator.single((x._1, JsNum(x._2))) ++ Iterator.single(next)
    }
    implicit def lazyNameLongPair(x: (String, Long)) = new {
      def |#(next: => (String, JsValue)): Iterator[(String, JsValue)] =
        Iterator.single((x._1, JsNum(x._2))) ++ Iterator.single(next)
    }
  }

  sealed trait JsValue extends Iterator[Char]

  object JsValue {
    val numberRegexp = "\\-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][\\-\\+]?[0-9]+)?".r

    implicit def booleanToJs(x: Boolean) = JsBool(x)
    implicit def stringToJs(x: String) = JsStr(x)
    implicit def byteToJs(x: Byte) = JsNum(x)
    implicit def shortToJs(x: Short) = JsNum(x)
    implicit def intToJs(x: Int) = JsNum(x)
    implicit def longToJs(x: Long) = JsNum(x)
    implicit def floatToJs(x: Float) = JsNum(x)
    implicit def doubleToJs(x: Double) = JsNum(x)
    implicit def bigIntegerToJs(x: java.math.BigInteger) = JsNum(x)
    implicit def bigIntToJs(x: BigInt) = JsNum(x)
    implicit def bigDecimalToJs(x: java.math.BigDecimal) = JsNum(x)
    implicit def bigDecToJs(x: BigDecimal) = JsNum(x)
  }

  private def wrap(it: Iterator[Char]): JsValue =
    new WrappedIterator(it) with JsValue

  def JsNull = wrap { "null".toIterator }
  def JsBool(value: Boolean) = wrap {
    if (value) "true".toIterator else "false".toIterator
  }
  def JsNum(x: Byte) = wrap { x.toString.toIterator }
  def JsNum(x: Short) = wrap { x.toString.toIterator }
  def JsNum(x: Int) = wrap { x.toString.toIterator }
  def JsNum(x: Long) = wrap { x.toString.toIterator }
  def JsNum(x: Float) =
    if (x.isNaN)
      JsStr("NaN")
    else if (x.isNegInfinity)
      JsStr("-Infinity")
    else if (x.isPosInfinity)
      JsStr("Infinity")
    else
      wrap { x.toString.toIterator }
  def JsNum(x: Double) =
    if (x.isNaN)
      JsStr("NaN")
    else if (x.isNegInfinity)
      JsStr("-Infinity")
    else if (x.isPosInfinity)
      JsStr("Infinity")
    else
      wrap { x.toString.toIterator }
  def JsNum(x: java.math.BigInteger) = wrap { x.toString.toIterator }
  def JsNum(x: BigInt) = wrap { x.toString.toIterator }
  def JsNum(x: java.math.BigDecimal) = wrap { x.toString.toIterator }
  def JsNum(x: BigDecimal) = wrap { x.toString.toIterator }
  def JsNum(x: String) = wrap {
    if (!JsValue.numberRegexp.pattern.matcher(x).matches)
      throw new IllegalArgumentException()
    x.toIterator
  }
  def JsStr(chars: Iterator[Char]): JsValue = wrap {
    Iterator.single('"') ++
    (chars.map {
       case c if c == '"' || c == '\\' || c <= 0x1F =>
         ("\\u%04X" format c.intValue).toIterator
       case c => Iterator.single(c)
     } flatten) ++ Iterator.single('"')
  }
  def JsStr(str: String): JsValue = JsStr(str.toIterator)
  def JsArray(elements: Iterator[JsValue]): JsValue = wrap {
    Iterator.single('[') ++
    (elements.zipWithIndex.map {
       case (element, i) =>
         (if (i == 0) Iterator.empty else Iterator.single(',')) ++ element
     } flatten) ++ Iterator.single(']')
  }
  def JsArray(element: => JsValue): JsValue =
    JsArray(Iterator.continually(element).take(1))
  def JsArray(): JsValue = JsArray(Iterator.empty)
  def JsObj(members: Iterator[(String, JsValue)]): JsValue = wrap {
    Iterator.single('{') ++
    (members.zipWithIndex.map {
       case ((name, value), i) =>
         (if (i == 0) Iterator.empty else Iterator.single(',')) ++
         JsStr(name) ++ Iterator.single(':') ++ value
     } flatten) ++ Iterator.single('}')
  }
  def JsObj(member: => (String, JsValue)): JsValue =
    JsObj(Iterator.continually(member).take(1))
  def JsObj(): JsValue = JsObj(Iterator.empty)

  object BeanSerializer {
    type Path = Seq[Path.Element[_]]

    object Path {
      sealed trait Element[V] {
        def elementType: Class[V]
      }
      final case class Member[V](name: String, property: Property[V]) extends Element[V] {
        def elementType = property.propertyType
      }
      final case class Elem[V](index: Int, elementType: Class[V]) extends Element[V]
    }

    object PathSuffix {
      def unapply(path: Path): Option[(Path, Path.Element[_])] =
        if (path.isEmpty)
          None
        else
          Some(path.init, path.last)
    }

    sealed trait Hint
    object DefaultHint extends Hint
    object OmitHint extends Hint
    case class ValueHint(value: JsValue) extends Hint
    case class ArrayHint[T](clazz: Class[T], element: Iterator[T]) extends Hint
  }

  trait BeanSerializer[T <: AnyRef] {
    import BeanSerializer._

    val beanType: Class[T]

    protected def hint(path: Path, value: Any): Hint = DefaultHint

    private def serializeSimpleValue(value: Any): Option[JsValue] = Data.unbox(value) match {
      case null => Some(JsNull)
      case x: Boolean => Some(JsBool(x))
      case x: Char => Some(JsStr(x.toString))
      case x: Byte => Some(JsNum(x))
      case x: Short => Some(JsNum(x))
      case x: Int => Some(JsNum(x))
      case x: Long => Some(JsNum(x))
      case x: Float => Some(JsNum(x))
      case x: Double => Some(JsNum(x))
      case x: java.math.BigInteger => Some(JsNum(x))
      case x: BigInt => Some(JsNum(x))
      case x: java.math.BigDecimal => Some(JsNum(x))
      case x: BigDecimal => Some(JsNum(x))
      case x: java.lang.Number => Some(JsNum(x.toString))
      case x: Numeric[_] => Some(JsNum(x.toString))
      case x: String => Some(JsStr(x))
      case _ => None
    }

    private def serializeArray[A](path: Path, clazz: Class[A],
                                  elements: Iterator[A]): JsValue = JsArray {
      elements.zipWithIndex.map { case (e, i) =>
        val elementPath: Path = path :+ Path.Elem(i, clazz)
        hint(elementPath, e) match {
          case OmitHint => Iterator.empty
          case ValueHint(value) => Iterator.single(value)
          case ArrayHint(_, _) => throw new IllegalStateException
          case DefaultHint =>
            Iterator.single {
              serializeSimpleValue(e).getOrElse {
                val (obj, objClass) = (e, clazz).asInstanceOf[(AnyRef, Class[AnyRef])]
                serializeObject(elementPath, objClass, obj)
              }
            }
        }
      } flatten
    }

    private def serializeObject[A <: AnyRef](
                  path: Path, clazz: Class[A], obj: A): JsValue = JsObj {
      Reflection.readOnlyProperties(clazz).map { p =>
        val memberPath: Path = path :+ Path.Member(p.name, p)
        val value = p.get(obj)
        hint(memberPath, value) match {
          case OmitHint => Iterator.empty
          case ValueHint(value) => Iterator.single(p.name -> value)
          case ArrayHint(et, elems) =>
            Iterator.single(p.name -> serializeArray(memberPath, et, elems))
          case DefaultHint =>
            Iterator.single { p.name ->
              serializeSimpleValue(value).getOrElse {
                val prop = p.asInstanceOf[Property[V] forSome { type V <: AnyRef }]
                serializeObject(memberPath, prop.propertyType,
                                value.asInstanceOf[prop.type#Value])
              }
            }
        }
      } flatten
    }

    def serialize(obj: T): JsValue = {
      if (obj == null)
        JsNull
      else
        serializeObject(FingerSeq.empty, beanType, obj)
    }

    def serialize(obj1: T, objs: T*): JsValue = JsArray {
      Iterator.single(serialize(obj1)) ++ objs.toIterator.map(serialize(_))
    }

    def serialize(objs: Iterator[T]): JsValue = JsArray {
      objs.map(serialize(_))
    }
  }
}
