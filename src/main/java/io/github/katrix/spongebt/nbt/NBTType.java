/*
 * This file is part of SpongyNBT, licensed under the MIT License (MIT).
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
package io.github.katrix.spongebt.nbt;

import javax.annotation.Nullable;

/**
 * The different NBT types you can find. If you check that an NBTTag is of a specific byte, you can safely cast to that type.
 */
public enum NBTType {

	TAG_END(0),
	TAG_BYTE(1),
	TAG_SHORT(2),
	TAG_INT(3),
	TAG_LONG(4),
	TAG_FLOAT(5),
	TAG_DOUBLE(6),
	TAG_BYTE_ARRAY(7),
	TAG_STRING(8),
	TAG_LIST(9),
	TAG_COMPOUND(10),
	TAG_INT_ARRAY(11);

	public final byte id;

	NBTType(int id) {
		this.id = (byte)id;
	}

	@Nullable
	public static NBTType idToType(byte id) {
		switch(id) {
			case 0: return TAG_END;
			case 1: return TAG_BYTE;
			case 2: return TAG_SHORT;
			case 3: return TAG_INT;
			case 4: return TAG_LONG;
			case 5: return TAG_FLOAT;
			case 6: return TAG_DOUBLE;
			case 7: return TAG_BYTE_ARRAY;
			case 8: return TAG_STRING;
			case 9: return TAG_LIST;
			case 10: return TAG_COMPOUND;
			case 11: return TAG_INT_ARRAY;
		}

		return null;
	}
}
