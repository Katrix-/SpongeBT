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
package io.github.katrix.spongebt

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.annotation.{switch, tailrec}

import io.github.katrix.spongebt.nbt.NBTType._
import io.github.katrix.spongebt.nbt.{NBTByte, NBTByteArray, NBTCompound, NBTDouble, NBTFloat, NBTInt, NBTIntArray, NBTList, NBTLong, NBTShort, NBTString, NBTTag, NBTType}

object NBTStreamTools {

	final val UTF8 = StandardCharsets.UTF_8

	@throws[IOException]
	def writeTo(stream: OutputStream, nbt: NBTCompound, rootName: String, compressed: Boolean): Unit = {
		val newStream = new DataOutputStream(if(compressed) new BufferedOutputStream(new GZIPOutputStream(stream)) else stream)
		try {
			writeType(newStream, nbt.getType)
			writeString(newStream, rootName)
			writeCompound(newStream, nbt)
		}
		finally {
			newStream.close()
		}
	}

	@throws[IOException]
	def readFrom(stream: InputStream, compressed: Boolean): (String, NBTCompound) = {
		val newStream = new DataInputStream(if(compressed) new BufferedInputStream(new GZIPInputStream(stream)) else stream)
		val ret = try {
			val nbtType = readType(newStream)
			if(nbtType == TAG_COMPOUND) {
				val name = readString(newStream)
				val tag = readCompound(newStream, new NBTCompound())
				(name, tag)
			}
			else throw new IOException()
		}
		finally {
			newStream.close()
		}

		ret
	}

	@throws[IOException]
	private def writeCompound(stream: DataOutputStream, nbt: NBTCompound): Unit = {
		for(elem <- nbt.values) {
			writeType(stream, elem._2.getType)
			writeString(stream, elem._1)
			writeTag(stream, elem._2)
		}

		stream.writeByte(0)
	}

	@throws[IOException]
	private def writeString(stream: DataOutputStream, string: String): Unit = {
		stream.writeShort(string.length)
		stream.write(string.getBytes(UTF8))
	}

	@throws[IOException]
	private def writeList(stream: DataOutputStream, list: NBTList): Unit = {
		writeType(stream, list.getListType)
		stream.writeInt(list.size)
		list.getAll.foreach(writeTag(stream, _))
	}

	@throws[IOException]
	private def writeByteArray(stream: DataOutputStream, array: Array[Byte]): Unit = {
		stream.writeInt(array.length)
		stream.write(array)
	}

	@throws[IOException]
	private def writeIntArray(stream: DataOutputStream, array: Array[Int]): Unit = {
		stream.writeInt(array.length)
		array.foreach(stream.writeInt)
	}

	@throws[IOException]
	private def writeType(stream: DataOutputStream, nbtType: NBTType): Unit = stream.writeByte(nbtType.id)

	@throws[IOException]
	private def writeTag(stream: DataOutputStream, nbt: NBTTag): Unit = {
		nbt match {
			case NBTByte(byte) => stream.writeByte(byte)
			case NBTShort(short) => stream.writeShort(short)
			case NBTInt(int) => stream.writeInt(int)
			case NBTLong(long) => stream.writeLong(long)
			case NBTFloat(float) => stream.writeFloat(float)
			case NBTDouble(double) => stream.writeDouble(double)
			case NBTByteArray(byteArray) => writeByteArray(stream, byteArray)
			case NBTString(string) => writeString(stream, string)
			case list: NBTList => writeList(stream, list)
			case compound: NBTCompound => writeCompound(stream, compound)
			case NBTIntArray(intArray) => writeIntArray(stream, intArray)
		}
	}

	@tailrec
	@throws[IOException]
	private def readCompound(stream: DataInputStream, compound: NBTCompound): NBTCompound = {
		val nbtType = readType(stream)
		if(nbtType == TAG_END) return compound

		val name = readString(stream)
		val tag = readTag(stream, nbtType)
		compound.setTag(name, tag)
		readCompound(stream, compound)
	}

	@throws[IOException]
	private def readString(stream: DataInputStream): String = {
		val length = stream.readShort()
		val characters = new Array[Byte](length)
		stream.readFully(characters)

		new String(characters, UTF8)
	}

	@throws[IOException]
	private def readList(stream: DataInputStream): NBTList = {
		val nbtType = readType(stream)
		val length = stream.readInt()
		val list = new NBTList(nbtType)

		for(i <- 0 until length) {
			val listElement = readTag(stream, nbtType)
			list.add(listElement)
		}

		list
	}

	@throws[IOException]
	private def readByteArray(stream: DataInputStream): Array[Byte] = {
		val length = stream.readInt()
		val bytes = new Array[Byte](length)
		stream.readFully(bytes)

		bytes
	}

	@throws[IOException]
	private def readIntArray(stream: DataInputStream): Array[Int] = {
		val length = stream.readInt()
		val intArray = new Array[Int](length)


		for(i <- 0 until length) {
			intArray(i) = stream.readInt()
		}

		intArray
	}

	@throws[IOException]
	private def readType(stream: DataInputStream): NBTType = {
		val nbtType = stream.readByte()
		Option(NBTType.idToType(nbtType)) match {
			case Some(ret) => ret
			case None => throw new IOException(s"Unrecognized NBT type $nbtType")
		}
	}

	@throws[IOException]
	private def readTag(stream: DataInputStream, nbtType: NBTType): NBTTag = nbtType match {
		case TAG_BYTE => NBTByte(stream.readByte())
		case TAG_SHORT => NBTShort(stream.readShort())
		case TAG_INT => NBTInt(stream.readInt())
		case TAG_LONG => NBTLong(stream.readLong())
		case TAG_FLOAT => NBTFloat(stream.readFloat())
		case TAG_DOUBLE => NBTDouble(stream.readDouble())
		case TAG_BYTE_ARRAY => NBTByteArray(readByteArray(stream))
		case TAG_STRING => NBTString(readString(stream))
		case TAG_LIST => readList(stream)
		case TAG_COMPOUND => readCompound(stream, new NBTCompound())
		case TAG_INT_ARRAY => NBTIntArray(readIntArray(stream))
		case TAG_END => throw new IOException("Unexpected end tag")
	}
}