package com.timbarrass.mods

import net.minecraft.client.Minecraft
import net.minecraft.client.resources.model.ModelResourceLocation

object MazeSeedItemRender {

  def registerItemRender = {

    regItem(MazeSeedItems.mazeSeed)

  }

  def regItem(item: MazeSeedItem) = {

    Minecraft.getMinecraft.getRenderItem.getItemModelMesher.register(item, 0, new ModelResourceLocation(MazeSeedMod.MODID + ":" + item.getUnlocalizedName.substring(5), "inventory"))

  }

}
