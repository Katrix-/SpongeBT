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
package io.github.katrix.spongebt.nbt.parser

import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import io.github.katrix.spongebt.nbt.{NBTByte, NBTCompound, NBTDouble, NBTFloat, NBTInt, NBTList, NBTLong, NBTShort, NBTString, NBTTag}
import io.github.katrix.spongebt.nbt.parser.NBTTokenType._

object NBTParser {

	@throws[NBTParseException]
	def parse(string: String): NBTCompound = {
		val trimmed = string.trim
		val tokens = new Lexer(trimmed).lex.iterator

		//First name, not used
		testHasNest(tokens)
		val token = tokens.next
		if(token.tokenType != NBTTokenType.COMPOUND_START) throw new NBTParseException("NBT did not start with curly bracket", token)

		getCompound(tokens)
	}

	@throws[NBTParseException]
	private def getCompound(tokens: Iterator[Token]): NBTCompound = {
		val compound = new NBTCompound
		testHasNest(tokens)
		val token = tokens.next

		if(token.tokenType == NBTTokenType.COMPOUND_END) return compound

		getCompoundRec(tokens, token, compound)

		/*
		while(tokens.hasNext) {
			if(token.tokenType != NBTTokenType.TAG_NAME) throw new NBTParseException(s"Expected name, got ${token.tokenType}", token)

			val name = token.value

			testHasNest(tokens)
			token = tokens.next
			if(token.tokenType != NBTTokenType.COLON) throw new NBTParseException("Expected colon after name", token)

			compound.setTag(name, tagFromToken(tokens))

			testHasNest(tokens)
			token = tokens.next

			if(token.tokenType != NBTTokenType.COMMA) {
				if(token.tokenType == NBTTokenType.COMPOUND_END) {
					return compound
				}
				else {
					throw new NBTParseException("Unbalanced curly brackets. Last token was ", token)
				}
			}

			testHasNest(tokens)
			token = tokens.next
		}

		compound
		*/
	}

	//TEST
	@tailrec
	@throws[NBTParseException]
	private def getCompoundRec(tokens: Iterator[Token], lastToken: Token, compound: NBTCompound): NBTCompound = {

		var token = lastToken
		if(token.tokenType != NBTTokenType.TAG_NAME) throw new NBTParseException(s"Expected name, got ${token.tokenType}", token)

		val name = token.value

		testHasNest(tokens)
		token = tokens.next
		if(token.tokenType != NBTTokenType.COLON) throw new NBTParseException("Expected colon after name", token)

		compound.setTag(name, tagFromToken(tokens))

		testHasNest(tokens)
		token = tokens.next

		if(token.tokenType != NBTTokenType.COMMA) {
			if(token.tokenType == NBTTokenType.COMPOUND_END) {
				return compound
			}
			else {
				throw new NBTParseException("Unbalanced curly brackets. Last token was ", token)
			}
		}

		getCompoundRec(tokens, token, compound)
	}

	@throws[NBTParseException]
	private def getList(tokens: Iterator[Token]): NBTList = {
		var list: NBTList = null //Ugly
		testHasNest(tokens)
		var token = tokens.next

		var i = 0
		while(tokens.hasNext) {

			val stringIndex = token.value
			if(token.tokenType != NBTTokenType.NBT_INT) throw new NBTParseException(s"Expected index, got ${token.tokenType} $stringIndex", token)

			testHasNest(tokens)
			token = tokens.next
			if(token.tokenType != NBTTokenType.COLON) throw new NBTParseException("Expected colon after index", token)

			val index = stringIndex.toInt
			if(index != i) throw new NBTParseException("Index did not follow sequential order", token)

			val nbtTag = tagFromToken(tokens)
			if(list == null) {
				list = new NBTList(nbtTag.getType)
			}
			list.add(nbtTag)

			testHasNest(tokens)
			token = tokens.next

			if(token.tokenType != NBTTokenType.COMMA) {
				if(token.tokenType == NBTTokenType.LIST_END) {
					return list
				}
				else {
					throw new NBTParseException("Unbalanced [] brackets. Last token was ", token)
				}
			}

			testHasNest(tokens)
			token = tokens.next
			i += 1
		}

		list
	}

	private def tagFromToken(tokens: Iterator[Token]): NBTTag = {
		testHasNest(tokens)
		val token = tokens.next
		val value = token.value
		val primitive = value.substring(0, value.length - 1)

		token.tokenType match {
			case NBT_BYTE => new NBTByte(primitive.toByte)
			case NBT_SHORT => new NBTShort(primitive.toShort)
			case NBT_LONG => new NBTLong(primitive.toLong)
			case NBT_FLOAT => new NBTFloat(primitive.toFloat)
			case NBT_DOUBLE => new NBTDouble(primitive.toDouble)
			case NBT_INT => new NBTInt(value.toInt)
			case NBT_STRING => new NBTString(value)
			case LIST_START => getList(tokens)
			case COMPOUND_START => getCompound(tokens)
			case _ => throw new NBTParseException(s"Unexpected token ${token.tokenType}", token)
		}
	}

	@throws[NBTParseException]
	private def testHasNest(tokens: Iterator[Token]): Unit = if(!tokens.hasNext) throw new NBTParseException("Unexpected end of string")
}

case class Token(
	tokenType: NBTTokenType.TokenType,
	value: String,
	col: Int,
	line: Int)

private class Lexer(val input: String) {

	val patternBuilder = new StringBuilder

	NBTTokenType.values.foreach(token => patternBuilder.append(String.format(s"|(?<${token.group}>${token.regex.pattern})")))

	private var pos   = 0
	private val regex = Pattern.compile(patternBuilder.toString.substring(1))

	@throws[NBTParseException]
	protected[parser] def lex: List[Token] = {

		@throws[NBTParseException]
		@tailrec
		def lexToken(list: ListBuffer[Token]): ListBuffer[Token] = {
			if(!hasNext) list
			else {
				val token = next

				if(token.tokenType != NBTTokenType.WHITESPACE) {
					list += token
				}

				lexToken(list)
			}
		}

		lexToken(new ListBuffer[Token]).toList
	}

	private def hasNext: Boolean = pos < input.length

	@throws[NBTParseException]
	private def next: Token = {
		val line = getLine
		val col = getCol
		val matcher = regex.matcher(input.substring(pos))
		if(matcher.find) {

			for(tokenType <- NBTTokenType.values) {
				val result = matcher.group(tokenType.group)
				if(result != null) {
					val length = matcher.end - matcher.start
					val token = new Token(tokenType, result, col, line)
					pos += length
					return token
				}
			}

			throw new NBTParseException("No patterns matched", col, line)
		}
		else throw new NBTParseException("Unrecognized character", col, line)
	}

	private def getLine: Int = {
		val current = input.substring(0, pos)
		val newLine = "\n".r
		val res = newLine findAllIn current
		res.length + 1
	}

	private def getCol: Int = {
		val current = input.substring(0, pos)
		val lines = current.split("\n")

		var col = pos + 1 //Columns begin at 1

		val stringIterator = lines.iterator
		while(stringIterator.hasNext) {

			val line = stringIterator.next
			if(stringIterator.hasNext) {
				col -= line.length
				col -= 1
			}
			else if(current.endsWith("\n")) {
				col = 1 //\n doesn't produce it's own line so we need to it add manually.
			}
		}

		col
	}
}