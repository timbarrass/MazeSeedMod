package com.timbarrass.mods.mazeseedmod

import net.minecraft.init.Blocks
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.Mod.EventHandler
import net.minecraftforge.fml.common.event.FMLInitializationEvent


@Mod(modid = MazeSeedMod.MODID, version = MazeSeedMod.VERSION, modLanguage = "scala")
object MazeSeedMod {
  final val MODID = "MazeSeedMod"
  final val VERSION = "1.0"

  @EventHandler
  def init(event: FMLInitializationEvent): Unit = {
    println(s"DIRT BLOCK >> ${Blocks.dirt.getUnlocalizedName}")
  }
}