package com.timbarrass.mods

import net.minecraftforge.fml.common.registry.GameRegistry

object MazeSeedItems {

  var mazeSeed: MazeSeedItem = new MazeSeedItem("")

  def init = {

    mazeSeed = new MazeSeedItem("mazeSeed")

    GameRegistry.registerItem(mazeSeed, "mazeSeed")
  }

}
