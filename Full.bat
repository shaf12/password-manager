BIN\tasm /zi DES.asm
BIN\tasm /zi Files.asm
BIN\tasm /zi Base32En.asm
BIN\tasm /zi UNIXts.asm
BIN\tasm /zi SHA1.asm
BIN\tasm /zi Main.asm
BIN\tlink /v Main.obj SHA1.obj UNIXts.obj Base32En.obj Files.obj DES.obj
