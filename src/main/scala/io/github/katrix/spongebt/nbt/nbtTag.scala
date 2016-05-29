/*
 * This file is part of SpongeBT, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2016 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package io.github.katrix.spongebt.nbt

import java.util.{Optional, UUID}

import scala.annotation.varargs
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import org.spongepowered.api.data.{DataContainer, DataSerializable}

import io.github.katrix.spongebt.sponge.NBTTranslator

sealed abstract class NBTTag {

	def copyTag: NBTTag

	def getType: NBTType

	def toMojangson: String

	def toIndentedMojangson: String = toMojangsonIndent(0)

	protected[spongebt] def toMojangsonIndent(indention: Int): String = toMojangson

	protected def indent(b: StringBuilder, indention: Int): StringBuilder = {
		b.append('\n')

		for(i <- 0 to indention) {
			b.append('\t')
		}

		b
	}
}

final case class NBTByte(value: Byte) extends NBTTag {

	override def copyTag: NBTTag = new NBTInt(value)

	override def getType: NBTType = NBTType.TAG_BYTE

	override def toMojangson: String = s"${value}b"
}

final case class NBTShort(value: Short) extends NBTTag {

	override def copyTag: NBTTag = new NBTShort(value)

	override def getType: NBTType = NBTType.TAG_SHORT

	override def toMojangson: String = s"${value}s"
}

final case class NBTInt(value: Int) extends NBTTag {

	override def copyTag: NBTTag = new NBTInt(value)

	override def getType: NBTType = NBTType.TAG_INT

	override def toMojangson: String = s"$value"
}

final case class NBTLong(value: Long) extends NBTTag {

	override def copyTag: NBTTag = new NBTLong(value)

	override def getType: NBTType = NBTType.TAG_LONG

	override def toMojangson: String = s"${value}L"
}

final case class NBTFloat(value: Float) extends NBTTag {

	override def copyTag: NBTTag = new NBTFloat(value)

	override def getType: NBTType = NBTType.TAG_FLOAT

	override def toMojangson: String = s"${value}f"
}

final case class NBTDouble(value: Double) extends NBTTag {

	override def copyTag: NBTTag = new NBTDouble(value)

	override def getType: NBTType = NBTType.TAG_DOUBLE

	override def toMojangson: String = s"${value}d"
}

final case class NBTByteArray(values: Array[Byte]) extends NBTTag {

	override def copyTag: NBTTag = {
		val bytes: Array[Byte] = new Array[Byte](values.length)
		System.arraycopy(values, 0, bytes, 0, values.length)
		new NBTByteArray(bytes)
	}

	override def getType: NBTType = NBTType.TAG_BYTE_ARRAY

	override def toMojangson: String = s"[${values.length} bytes]"
}

final case class NBTString(value: String) extends NBTTag {
	require(value != null, "NBT created with null string. String cannot be null")

	override def copyTag: NBTTag = new NBTString(value)

	override def getType: NBTType = NBTType.TAG_STRING

	override def toMojangson: String = s""""$value""""
}

final class NBTList(val nbtType: NBTType) extends NBTTag {

	private val values = new ArrayBuffer[NBTTag]()

	def get(i: Int): NBTTag = values(i)

	def getAll: List[NBTTag] = values.toList

	def getAllJava: java.util.List[NBTTag] = values.toList.asJava

	def add(value: NBTTag): Unit = {
		require(value.getType == nbtType, "Tried to add wrong type to NBT list")
		values += value
	}

	@varargs
	def addAll(value: NBTTag*): Unit = {
		value.foreach(add(_))
	}

	def set(value: NBTTag, i: Int): Unit = {
		require(value.getType == nbtType, "Tried to add wrong type to NBT list")
		values.update(i, value)
	}

	def remove(i: Int): Unit = values.remove(i)

	def size: Int = values.size

	def getListType: NBTType = nbtType

	override def getType: NBTType = NBTType.TAG_LIST

	override def copyTag: NBTTag = {
		val list = new NBTList(nbtType)

		for(tag <- values) {
			val newTag = tag.copyTag
			list.add(newTag)
		}

		list
	}

	override def toMojangson: String = {
		val b: StringBuilder = new StringBuilder("[")

		for(i <- 0 to values.size) {
			if(i != 0) {
				b.append(',')
			}

			b.append(s"$i:${values(i)}")
		}

		b.append(']').toString
	}

	override def toMojangsonIndent(indention: Int): String = {
		val b = new StringBuilder("[")

		for(i <- values.indices) {
			if(i != 0) {
				b.append(',')
			}
			indent(b, indention)
			val subTag = values(i).toMojangsonIndent(indention + 1)
			b.append(s"$i:$subTag")
		}

		indent(b, indention - 1)
		b.append(']').toString
	}

	override def equals(other: Any): Boolean = other match {
		case that: NBTList => values == that.values && nbtType == that.nbtType
		case _ => false
	}

	override def hashCode(): Int = {
		val state = Seq(values, nbtType)
		state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
	}

	override def toString = s"NBTList($nbtType, $values)"
}

final case class NBTCompound(values: mutable.Map[String, NBTTag]) extends NBTTag with DataSerializable {

	def this() {
		this(new mutable.LinkedHashMap[String, NBTTag]())
	}

	def size: Int = values.size

	def setTag(key: String, tag: NBTTag): Unit = values.put(key, tag)

	def setByte(key: String, value: Byte): Unit = setTag(key, new NBTByte(value))

	def setShort(key: String, value: Short): Unit = setTag(key, new NBTShort(value))

	def setInt(key: String, value: Int): Unit = setTag(key, new NBTInt(value))

	def setLong(key: String, value: Long): Unit = setTag(key, new NBTLong(value))

	def setFloat(key: String, value: Float): Unit = setTag(key, new NBTFloat(value))

	def setDouble(key: String, value: Double): Unit = setTag(key, new NBTDouble(value))

	def setString(key: String, value: String): Unit = setTag(key, new NBTString(value))

	def setByteArray(key: String, value: Array[Byte]): Unit = setTag(key, new NBTByteArray(value))

	def setIntArray(key: String, value: Array[Int]): Unit = setTag(key, new NBTIntArray(value))

	def setUUID(key: String, value: UUID): Unit = {
		setLong(key + "Most", value.getMostSignificantBits)
		setLong(key + "Least", value.getLeastSignificantBits)
	}

	def setBoolean(key: String, value: Boolean): Unit = setByte(key, (if(value) 1 else 0).toByte)

	def get(key: String): Option[NBTTag] = values.get(key)

	def getJava(key: String): Optional[NBTTag] = values.get(key) match {
		case Some(tag) => Optional.of(tag)
		case None => Optional.empty();
	}

	def getOrCreate(key: String, tag: NBTTag): NBTTag = values.get(key) match {
		case Some(foundTag) => foundTag
		case None =>
			setTag(key, tag)
			tag
	}

	def getUUID(key: String): Option[UUID] = {
		val optMost = get(s"${key}Most")
		val optLeast = get(s"${key}Least")
		for {
			m <- optMost
			l <- optLeast
		} {
			m match {
				case NBTLong(most) => l match {
					case NBTLong(least) => return Some(new UUID(most, least))
					case _ => return None
				}
				case _ => return None
			}
		}

		None
	}

	def getUUIDJava(key: String): Optional[UUID] = {
		getUUID(key) match {
			case Some(tag) => Optional.of(tag)
			case None => Optional.empty();
		}
	}

	def remove(key: String): Unit = values.remove(key)

	def hasKey(key: String): Boolean = values.contains(key)

	def valuesJava: java.util.Map[String, NBTTag] = values.asJava

	def merge(other: NBTCompound) {
		for(entry <- other.values.seq) {
			val tag = entry._2
			val key = entry._1

			tag match {
				case compound: NBTCompound =>
					val optChildCompound = get(key)
					if(optChildCompound.isDefined) {
						optChildCompound.get match {
							case childCompound: NBTCompound => childCompound.merge(compound)
							case _ => setTag(key, compound.copyTag)
						}
					}
					else {
						setTag(key, compound.copyTag)
					}
				case _ =>
					setTag(key, tag.copyTag)
			}
		}
	}

	override def copyTag: NBTTag = {
		val tag = new NBTCompound

		for(entry <- values.seq) {
			tag.setTag(entry._1, entry._2.copyTag)
		}

		tag
	}

	override def getType: NBTType = NBTType.TAG_COMPOUND

	override def toMojangson: String = {
		val b = new StringBuilder("{")

		for(entry <- values.seq) {
			if(b.length != 1) {
				b.append(',')
			}
			b.append(s"${entry._1}:${entry._2}")
		}

		b.append('}').toString
	}

	override def toMojangsonIndent(indention: Int): String = {
		val b = new StringBuilder("{")
		var first = true

		for(entry <- values.seq) {
			if(!first) {
				b.append(',')
			}
			first = false
			indent(b, indention)
			val subTag = entry._2.toMojangsonIndent(indention + 1)
			b.append(s"${entry._1}:$subTag")
		}

		if(values.nonEmpty) {
			indent(b, indention - 1)
		}

		b.append('}').toString
	}

	override def toContainer: DataContainer = NBTTranslator.translateFrom(this)

	override def getContentVersion: Int = 1
}

final case class NBTIntArray(values: Array[Int]) extends NBTTag {

	override def copyTag: NBTTag = {
		val ints: Array[Int] = new Array[Int](values.length)
		System.arraycopy(values, 0, ints, 0, values.length)
		new NBTIntArray(ints)
	}

	override def getType: NBTType = NBTType.TAG_INT_ARRAY

	override def toMojangson: String = {
		val b: StringBuilder = new StringBuilder("[")
		for(i <- values) {
			b.append(i)
			b.append(',')
		}
		b.append(']')

		b.toString
	}
}