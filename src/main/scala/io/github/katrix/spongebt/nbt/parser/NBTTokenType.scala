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

import scala.util.matching.Regex

/**
	* The many different token types that a mojangson string can be broken down into.
	*/
object NBTTokenType {

	sealed abstract class TokenType(val regex: Regex, val group: String) {

		def this(string: String, group: String) {
			this(s"^($string)".r(group), group)
		}

		override def toString = s"$group"
	}

	object COLON extends TokenType("[:]", "colon")

	object COMMA extends TokenType("[,]", "comma")

	object COMPOUND_START extends TokenType("""[\{]""", "compoundStart")

	object COMPOUND_END extends TokenType("""[\}]""", "compoundEnd")

	object LIST_START extends TokenType("""[\[]""", "listStart")

	object LIST_END extends TokenType("""[\]]""", "listEnd")

	object NBT_BYTE extends TokenType("""-?\d+b""", "nbtByte")

	object NBT_SHORT extends TokenType("""-?\d+s""", "nbtShort")

	object NBT_LONG extends TokenType("""-?\d+L""", "nbtLong")

	object NBT_FLOAT extends TokenType("""[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?f""", "nbtFloat")

	object NBT_DOUBLE extends TokenType("""[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?d""", "nbtDouble")

	object NBT_INT extends TokenType("""-?\d+""", "nbtInt")

	object WHITESPACE extends TokenType("""\s+""", "whitespace")

	object NBT_STRING extends TokenType("""\".+?\"""".r("nbtString"), "nbtString")

	//Doesn't support nested quotes. Problem?
	object TAG_NAME extends TokenType("^([^:]+)", "tagName")

	def values: Seq[TokenType] = Seq(
		COLON,
		COMMA,
		COMPOUND_START,
		COMPOUND_END,
		LIST_START,
		LIST_END,
		NBT_BYTE,
		NBT_SHORT,
		NBT_LONG,
		NBT_FLOAT,
		NBT_DOUBLE,
		NBT_INT,
		WHITESPACE,
		NBT_STRING,
		TAG_NAME)
}