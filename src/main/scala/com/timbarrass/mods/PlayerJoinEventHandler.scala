package com.timbarrass.mods

import net.minecraft.entity.player.EntityPlayer
import net.minecraft.util.{ChatComponentText, EnumChatFormatting}
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.fml.common.gameevent.PlayerEvent.PlayerLoggedInEvent


class PlayerJoinEventHandler {
  @SubscribeEvent
  def onPlayerLoggedIn(event: PlayerLoggedInEvent) = {

    event.player.addChatComponentMessage(new ChatComponentText(EnumChatFormatting.YELLOW + "-----------------------------------------------------"))
    event.player.addChatComponentMessage(new ChatComponentText(EnumChatFormatting.YELLOW + "Thanks for loading the Maze Seed mod"))
    event.player.addChatComponentMessage(new ChatComponentText(EnumChatFormatting.YELLOW + "Place an object, create a maze by using a Maze Seed -- then go find the diamond!"))
    event.player.addChatComponentMessage(new ChatComponentText(EnumChatFormatting.YELLOW + "Or use /summonmaze to just summon a simple maze nearby."))
    event.player.addChatComponentMessage(new ChatComponentText(EnumChatFormatting.YELLOW + "-----------------------------------------------------"))
  }
}
