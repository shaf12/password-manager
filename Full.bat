tasm /zi Base32En.asm
tasm /zi UNIXts.asm
tasm /zi SHA1.asm
tasm /zi Main.asm
tlink /v Main.obj SHA1.obj UNIXts.obj Base32En.obj
