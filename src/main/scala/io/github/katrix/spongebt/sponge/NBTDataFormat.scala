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

import java.io.{InputStream, OutputStream}

import org.spongepowered.api.data.persistence.DataFormat
import org.spongepowered.api.data.{DataContainer, DataView}

import io.github.katrix.spongebt.NBTStreamTools

/**
	* A [[DataFormat]] for NBT to make it easier to read and write [[DataView]]s to a stream
	*/
object NBTDataFormat extends DataFormat {

	/**
		* Write a [[DataView]] to an [[OutputStream]] as NBT
		*
		* The data is being written with as compressed with an empty root name
		* @param output The stream to write to
		* @param data The Data to write
		*/
	override def writeTo(output: OutputStream, data: DataView): Unit = {
		val tag = NBTTranslator.translateData(data)
		NBTStreamTools.writeTo(output, tag, "", compressed = true)
	}

	/**
		* Reads a [[DataContainer]] from a [[InputStream]] containing NBT
		*
		* The data is assumed to be compressed
		* @param input The stream to read from
		* @return A [[DataContainer]] created from the [[InputStream]]
		*/
	override def readFrom(input: InputStream): DataContainer = {
		val tag = NBTStreamTools.readFrom(input, compressed = true)
		NBTTranslator.translateFrom(tag._2)
	}

	override def getName: String = getId

	override def getId: String = "spongynbt:nbt"
}
