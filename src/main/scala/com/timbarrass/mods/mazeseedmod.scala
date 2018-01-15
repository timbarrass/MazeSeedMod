package com.timbarrass.mods

import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.fml.common.Mod.EventHandler
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent, FMLServerStartingEvent}
import net.minecraftforge.fml.common.{FMLCommonHandler, Mod}


@Mod(modid = MazeSeedMod.MODID, version = MazeSeedMod.VERSION, modLanguage = "scala")
object MazeSeedMod {
  final val MODID = "MazeSeedMod"
  final val VERSION = "1.0"

  @EventHandler
  def init(event: FMLInitializationEvent): Unit = {
    MazeSeedItemRender.registerItemRender

    val handler = new PlayerJoinEventHandler()
    MinecraftForge.EVENT_BUS.register(handler)
    FMLCommonHandler.instance().bus().register(handler);
  }

  @EventHandler
  def postInit(event: FMLPostInitializationEvent): Unit = {
  }

  @EventHandler
  def serverLoad(event: FMLServerStartingEvent): Unit = {
    event.registerServerCommand(new SummonMazeCommand());
  }

  @EventHandler
  def preInit(event: FMLPreInitializationEvent): Unit = {

    MazeSeedItems.init

  }
}
