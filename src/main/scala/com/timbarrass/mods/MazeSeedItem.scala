package com.timbarrass.mods

import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.{BlockPos, EnumFacing}
import net.minecraft.world.World

class MazeSeedItem(name: String) extends Item {

  setUnlocalizedName(name)
  setCreativeTab(CreativeTabs.tabMisc)

  override def onItemUse(stack: ItemStack, playerIn: EntityPlayer, worldIn: World, pos: BlockPos, side: EnumFacing, hitX: Float, hitY: Float, hitZ: Float): Boolean = {
    println("used maze seed")

    val world: World = playerIn.getEntityWorld

    // check type of block at blockpos?
    // create maze wit corner at blockpos (transform as viewpoint, offset in summonmaze command)
    // use blocks of same type as hit block
    val block = world.getBlockState(pos)
    val blockType = block.getBlock()
    val baseBlockType = world.getBlockState(pos.add(0, -1, 0)).getBlock()

    var dir = playerIn.rotationYaw % 360
    if (dir < 0) dir += 360

    val width = 10
    val height = 10
    val scale = 2

    val m = new PrimsMaze(width, height, scale)

    var xOffset: Int = 0
    var yOffset: Int = 0
    if (dir < 45 || dir >= 320) {
      xOffset = -width * (scale + 1)
    }
    if (dir >= 45 && dir < 135) {
      yOffset = -height * (scale + 1)
      xOffset = -width * (scale + 1)
    }
    if (dir >= 135 && dir < 225) {
      yOffset = -height * (scale + 1)
    }
    if (dir >= 225 && dir < 320) {
    }

    SummonMazeCommand.BuildMaze(pos, world, width, height, m, xOffset, yOffset, block.getBlock(), baseBlockType, scale)

    return true
  }
}
