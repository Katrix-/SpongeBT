#SpongyNBT
SpongyNBT is a lightweight NBT library made to work especially well with Sponge, although you don't need sponge to use SpongyNBT.

Although SpongyNBT is written in scala, it should work find with java. It even has a few helper methods to convert the NBT data to Java lists and maps.

##Features
* Convert between Sponge DataView and NBT
* Write NBT to disk, both compressed and raw
* Can read Minecraft NBT files
* Parse Mojangson to NBT
* Convert NBT to Mojangson

##Example

```java
public class Example {

	public static void main(String[] args) {
		NBTList list = new NBTList(NBTType.TAG_DOUBLE);
		list.addAll(new NBTDouble(Math.PI), new NBTDouble(Math.E));

		NBTCompound root = new NBTCompound();
		root.setByte("aByteTag", (byte)5);
		root.setString("stringTag", "this is the string");
		root.setTag("listTag", list);

		File someFile = new File(testFile.nbt);
		try(OutputStream stream = new FileOutputStream(someFile)) {
			NBTStreamTools.writeTo(stream, root, "", true);
		}
		catch(IOException e) {
			e.printStackTrace();
		}
	}
}
```