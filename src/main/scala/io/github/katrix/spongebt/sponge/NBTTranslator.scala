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
package io.github.katrix.spongebt.sponge

import java.util
import java.lang

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable

import org.spongepowered.api.data.DataQuery._
import org.spongepowered.api.data.translator.DataTranslator
import org.spongepowered.api.data.{DataContainer, DataQuery, DataSerializable, DataView, MemoryDataContainer}

import com.google.common.collect.Lists

import io.github.katrix.spongebt.nbt.{NBTByte, NBTByteArray, NBTCompound, NBTDouble, NBTFloat, NBTInt, NBTIntArray, NBTList, NBTLong, NBTShort, NBTString, NBTTag}
import io.github.katrix.spongebt.nbt.NBTType._

//Most code gotten from SpongeCommon
object NBTTranslator extends DataTranslator[NBTCompound] {

	//Force Java boxed values so nothing gets mixed up
	type Boolean = lang.Boolean
	type Byte = lang.Byte
	type Short = lang.Short
	type Int = lang.Integer
	type Long = lang.Long
	type Float = lang.Float
	type Double = lang.Double

	private final val boolIdent = "$Boolean" //Stupid boolean

	override def translateContainerToData(node: NBTCompound, container: DataView): Unit = containerToCompound(container).merge(node)

	override def translateData(container: DataView): NBTCompound = containerToCompound(container)

	override def translateFrom(node: NBTCompound): DataContainer = compoundToContainer(node)

	private def containerToCompound(container: DataView): NBTCompound = {
		val compound = new NBTCompound

		val tags = for(elem <- container.getValues(false).entrySet().asScala) yield {
			val value = elem.getValue
			val key = keyWithBoolean(elem.getKey, value)
			value match {
				case view: DataView => (key, containerToCompound(view))
				case _ => (key, getTagForObject(value))
			}
		}

		tags.foreach(elem => compound.setTag(elem._1, elem._2))

		compound
	}

	private def compoundToContainer(compound: NBTCompound): DataContainer = {
		val container = new MemoryDataContainer
		compound.values.foreach(elem => tagToView(elem._2, container, elem._1))
		container
	}

	private def tagToView(tag: NBTTag, view: DataView, key: String): Unit = {
		val query = DataQuery.of('.', key)
		def setView(any: Any) = view.set(query, any)

		tag match {
			case NBTByte(byte) =>
				if(key.contains(boolIdent)) {
					view.set(DataQuery.of('.', key.replace(boolIdent, "")), byte == 1)
				}
				else {
					setView(byte)
				}
			case NBTCompound(values) =>
				val internalView = view.createView(query)
				values.foreach(elem => tagToView(elem._2, internalView, elem._1))
			case _ => setView(tagToObject(tag))
		}
	}

	private def tagToObject(tag: NBTTag): AnyRef = tag match {
		case NBTByte(byte) => Byte.box(byte)
		case NBTShort(short) => Short.box(short)
		case NBTInt(int) => Int.box(int)
		case NBTLong(long) => Long.box(long)
		case NBTFloat(float) => Float.box(float)
		case NBTDouble(double) => Double.box(double)
		case NBTByteArray(array) => array
		case NBTIntArray(array) => array
		case NBTString(string) => string
		case list: NBTList => list.getAll.map(elem => tagToObject(elem)).asJava
		case compund: NBTCompound => compoundToContainer(compund)
	}

	private def getTagForObject(anyRef: AnyRef): NBTTag = anyRef match {
		case bool: Boolean => nbtBoolean(bool)
		case byte: Byte => NBTByte(byte)
		case short: Short => NBTShort(short)
		case int: Int => NBTInt(int)
		case long: Long => NBTLong(long)
		case float: Float => NBTFloat(float)
		case double: Double => NBTDouble(double)
		case string: String => NBTString(string)
		case byteArray: Array[scala.Byte] =>
			val newArray = util.Arrays.copyOf(byteArray, byteArray.length)
			NBTByteArray(newArray)
		case intArray: Array[scala.Int] =>
			val newArray = util.Arrays.copyOf(intArray, intArray.length)
			NBTIntArray(newArray)
		case byteArray: Array[Byte] =>
			val newArray = util.Arrays.copyOf(byteArray, byteArray.length)
			NBTByteArray(newArray.map(_.byteValue()))
		case intArray: Array[Int] =>
			val newArray = util.Arrays.copyOf(intArray, intArray.length)
			NBTIntArray(newArray.map(_.intValue()))
		case list: util.List[AnyRef @unchecked] =>
			val scalaList = list.asScala.toList
			val nbtType = if(list.isEmpty) TAG_BYTE else getTagForObject(scalaList.head).getType
			val nbtList = new NBTList(nbtType)
			val listContent = scalaList.map(getTagForObject(_))
			nbtList.addAll(listContent: _*)
			nbtList
		case map: util.Map[AnyRef @unchecked, AnyRef @unchecked] =>
			val scalaMap = map.asScala
			val compound = new NBTCompound
			val tags = scalaMap.map(elem => (keyWithBoolean(elem._1, elem._2), getTagForObject(elem._2)))
			tags.foreach(elem => compound.setTag(elem._1, elem._2))
			compound
		case serializable: DataSerializable => containerToCompound(serializable.toContainer)
		case view: DataView => containerToCompound(view)
		case _ => throw new IllegalArgumentException(s"Unable to translate object to NBTTag: $anyRef")
	}

	private def nbtBoolean(boolean: Boolean): NBTByte = NBTByte(if(boolean) 1 else 0)

	private def keyWithBoolean(key: AnyRef, obj: AnyRef): String = {
		val baseString = key.toString

		obj match {
			case bool: Boolean => baseString + boolIdent
			case _ => baseString
		}
	}
}
