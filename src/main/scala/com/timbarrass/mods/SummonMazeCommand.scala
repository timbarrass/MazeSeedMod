package com.timbarrass.mods

import java.util

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.timbarrass.mazes.{Cell, Maze, MazePublish, MazeTransform}
import com.typesafe.config.{Config, ConfigFactory}
import net.minecraft.block.Block
import net.minecraft.block.state.IBlockState
import net.minecraft.command._
import net.minecraft.init.{Blocks, Items}
import net.minecraft.inventory.IInventory
import net.minecraft.item.ItemStack
import net.minecraft.tileentity.{TileEntity, TileEntityChest}
import net.minecraft.util.{BlockPos, ChatComponentText, EnumFacing, EnumParticleTypes}
import net.minecraft.world.World
import net.minecraft.world.chunk.Chunk
import net.minecraftforge.items.CapabilityItemHandler

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class SummonMazeCommand extends CommandBase {

  override def getCommandName: String = {
    return "summonmaze"
  }

  override def canCommandSenderUseCommand(sender: ICommandSender): Boolean = {
    return true;
  }

  val aliases: util.ArrayList[String] = new util.ArrayList[String]()
  aliases.add("sm")

  override def getCommandAliases: util.List[String] = {
    return aliases
  }

  override def isUsernameIndex(args: Array[String], index: Int): Boolean = {
    return true
  }

  override def getCommandUsage(sender: ICommandSender): String = {
    return "/summonmaze"
  }

  val tabOptions: util.ArrayList[String] = new util.ArrayList[String]()

  override def addTabCompletionOptions(sender: ICommandSender, args: Array[String], pos: BlockPos): util.List[String] = {
    return tabOptions
  }

  override def processCommand(sender: ICommandSender, args: Array[String]): Unit = {
    if(args.length != 1) {
      sender.addChatMessage(new ChatComponentText("Try /summonmaze [make] [step]"))

      return
    }

    if(args(0) == "step") {
      println("Nudging actors ..")

      SummonMazeCommand.nudgedActors.foreach((actor) => {actor ! "Nudge"})
    }
    else if(args(0) == "switchMode") {
      println("Switching mode ..");

      SummonMazeCommand.nudgedActors.foreach((actor) => {actor ! "SwitchMode"})
    }
    else {
      println("summon maze called")
      sender.addChatMessage(new ChatComponentText("maze summoned"))

      // mazes are currently spawned to the south east
      // so adding 1, 1 to player position moves it south, east
      // south is rotation yaw 0/360
      // east 270, north 180, west 90 (assuming sun sets in the west)
      // from point of view of someone looking down on at the ground ...
      // so, depending on broad direction we can keep the build loop the same
      // but rotate the initial position around the player so that a corner
      // or other desired point) is next to the player after spawn
      val dir = sender.getCommandSenderEntity.rotationYaw
      println("yaw" + sender.getCommandSenderEntity.rotationYaw)


      var blockPos: BlockPos = sender.getPosition()
      val world: World = sender.getEntityWorld
      val height = 4
      val width = 4
      val scale = 2

      val m = Maze.generateMaze(width, height)
      var xOffset: Int = 0
      var yOffset: Int = 0
      if (dir < 45 || dir >= 320) {
        xOffset = -width / 2
        yOffset = 1
      }
      if (dir >= 45 && dir < 135) {
        xOffset = 1 + width
        yOffset = height / 2
      }
      if (dir >= 135 && dir < 225) {
        xOffset = width / 2
        yOffset = 1 + height
      }
      if (dir >= 225 && dir < 320) {
        xOffset = 1
        yOffset = -height / 2
      }

      SummonMazeCommand.BuildMaze(blockPos, world, height, width, m, xOffset + 1, yOffset + 1, Blocks.stone, Blocks.sand, scale)
    }
  }

  override def compareTo(o: ICommand): Int = {
    return 0
  }
}

class NudgedActor(m: Maze, world: World, marker: BlockPos, xOffset: Int, yOffset: Int, scale: Int) extends Actor {

  private var stepping: Boolean = false

  override def preStart(): Unit = {
    self ! "Nudge"
  }

  def receive = {
    case "Nudge" =>
      doWork
      if (! stepping) context.system.scheduler.scheduleOnce(0.5 seconds, self, "Nudge")
    case "SwitchMode" =>
      stepping = ! stepping
      self ! "Nudge"
    case _ =>
      println("Don't know what to do with message ")
  }

  def doWork: Unit = {

    if (!world.isRemote) {

      println("Before:")
      val t = MazeTransform.transformToGrid[Int](m, 2, 999, 1000, (x, y) => { m.regions(x, y) })
      MazePublish.display[Int](t, (c: Int) => { if( c == 999 ) 'X' else if ( c == 1000 ) ' ' else (c + 48).toChar})

      // debug
      val md =  new Maze(m.routes.map(_.map(_.clone))) // deep copy of routes

      val broken = m.breakALink
      val made = m.joinRegions

      // don't do anything if we've made and broken the same link!
      if(broken == made || broken == Cell(made._2.x, made._2.y) -> Cell(made._1.x, made._1.y)) {
        println("made == broken, nothing to do")
        return
      }

      MazeTransform.clearWall(
        scale,
        (xWall, yWall, x, y) => {

          val xp = xWall + xOffset + marker.getX()
          val zp = yWall + yOffset + marker.getZ()
          val pos: BlockPos = new BlockPos(xp, marker.getY() + 10, zp)
          val lowerPos: BlockPos = new BlockPos(xp, marker.getY() + 9, zp)

          SummonMazeCommand.SetBlock(world, pos, Blocks.gravel)
          SummonMazeCommand.SetBlock(world, lowerPos, Blocks.gravel)
        },
        broken._1.y,
        broken._1.x,
        broken._2)

      MazeTransform.clearWall(
        scale,
        (xWall, yWall, x, y) => {
          val xp = xWall + xOffset + marker.getX()
          val zp = yWall + yOffset + marker.getZ()
          val pos: BlockPos = new BlockPos(xp, marker.getY() - 1, zp)
          val lowerPos: BlockPos = new BlockPos(xp, marker.getY() - 2, zp)

          SummonMazeCommand.SetBlock(world, pos, Blocks.air)
          SummonMazeCommand.SetBlock(world, lowerPos, Blocks.air)
        },
        made._1.y,
        made._1.x,
        made._2)
    }
  }
}

object SummonMazeCommand {

  // temp hack, not a great idea to expose this
  var nudgedActors: Vector[ActorRef] = Vector[ActorRef]()

  // Handle periodic automatic changes to the maze
  // Interesting thing is that these mazes aren't persisted as mazes .. so once you've
  // quit and restarted their dynamic state will be lost, and they'll be frozen
  private val system = ActorSystem("MySystem")

  def CompareClientAndServer(blockPos: BlockPos, world: World, height: Int, width: Int, xOffset: Int, yOffset: Int, scale: Int): Unit = {
    for (
      z <- 0 until (scale + 1) * height + 1;
      x <- 0 until (scale + 1) * width + 1
    ) {
      val xp = x + xOffset + blockPos.getX()
      val zp = z + yOffset + blockPos.getZ()
      val pos: BlockPos = new BlockPos(xp, blockPos.getY(), zp)
      val lowerPos: BlockPos = new BlockPos(xp, blockPos.getY() - 1, zp)

      if(! world.isRemote) {
        println("server " + (x,z) + " " + world.getBlockState(pos))
      }
      else {
        println("client " + (x,z) + " " + world.getBlockState(pos))
      }
    }
    }

  def SetBlock(world: World, blockpos: BlockPos, block: Block) = {
    // IMPORTANT
    // By default this gets issued on the client and server. If you create blocks
    // too quickly the two seem to get out of sync, leading to weird invisible block
    // problems where the player jiggles when trying to move through a clear space.
    // If you create only on the client the new blocks don't get saved. Solution
    // is to create on the server and mark the blocks as dirty on the client so they
    // get refreshed.
    if (!world.isRemote) {
      val tileentity1: TileEntity = world.getTileEntity(blockpos)

      if (tileentity1 != null) {
        if (tileentity1.isInstanceOf[IInventory]) tileentity1.asInstanceOf[IInventory].clear()
        world.setBlockState(blockpos, Blocks.air.getDefaultState, if (block eq Blocks.air) 2
        else 4)
      }

      val iblockstate: IBlockState = block.getDefaultState()

      if (!world.setBlockState(blockpos, iblockstate, 2)) {
//        throw new CommandException("commands.setblock.noChange", new Array[AnyRef](0))
      }
      else {
        world.notifyNeighborsRespectDebug(blockpos, iblockstate.getBlock)
      }
    }
  }

  def BuildMaze(blockPos: BlockPos, world: World, height: Int, width: Int, m: Maze, xOffset: Int, yOffset: Int, wallBlock: Block, baseBlock: Block, scale: Int): Unit = {
    if (!world.isRemote) {

      // lookup and values here are just for doing something special with regions
      val finalGrid = MazeTransform.transformToGrid(m, 2, 1, 0, (x, y) => {
        0
      })

      // first clear the ground
      for (
        z <- 0 until (scale + 1) * height + 1;
        x <- 0 until (scale + 1) * width + 1
      ) {
        val xp = x + xOffset + blockPos.getX()
        val zp = z + yOffset + blockPos.getZ()
        val pos: BlockPos = new BlockPos(xp, blockPos.getY(), zp)
        val lowerPos: BlockPos = new BlockPos(xp, blockPos.getY() - 1, zp)

        for ( y <- -1 until 2) {
          val pos: BlockPos = new BlockPos(xp, blockPos.getY() + y, zp)

          world.destroyBlock(pos, false)
        }
      }

      // lay the base
      for (
        z <- 0 until (scale + 1) * height + 1;
        x <- 0 until (scale + 1) * width + 1
      ) {
        val xp = x + xOffset + blockPos.getX()
        val zp = z + yOffset + blockPos.getZ()
        val lowerPos: BlockPos = new BlockPos(xp, blockPos.getY() - 1, zp)

        SetBlock(world, lowerPos, baseBlock)
      }

      for (
        z <- 0 until (scale + 1) * height + 1;
        x <- 0 until (scale + 1) * width + 1
      ) {
        val xp = x + xOffset + blockPos.getX()
        val zp = z + yOffset + blockPos.getZ()

        for ( y <- 0 until 2) {
          val pos: BlockPos = new BlockPos(xp, blockPos.getY() + y + 10, zp)

          if (finalGrid(x)(z) == 1) {
            SetBlock(world, pos, Blocks.gravel)
          }
        }
      }

      val chunkMap: mutable.HashMap[Chunk, BlockPos] = mutable.HashMap()

      for (
        z <- 0 until (scale + 1) * height + 1;
        x <- 0 until (scale + 1) * width + 1
      ) {
        val xp = x + xOffset + blockPos.getX()
        val zp = z + yOffset + blockPos.getZ()
        val pos: BlockPos = new BlockPos(xp, blockPos.getY(), zp)

        chunkMap.put(world.getChunkFromBlockCoords(pos), pos)
      }

      // place a chest
      val chestX = Random.nextInt(width) * (scale + 1) + 1
      val chestY = Random.nextInt(height) * (scale + 1) + 1
      val cXp = chestX + xOffset + blockPos.getX()
      val cZp = chestY + yOffset + blockPos.getZ()

      val pos: BlockPos = new BlockPos(cXp, blockPos.getY(), cZp)
      SetBlock(world, pos, Blocks.chest)

      val te: TileEntity = world.getTileEntity(pos)
      val itemhandler = te.asInstanceOf[TileEntityChest].getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, EnumFacing.NORTH)
      val stack: ItemStack = new ItemStack(Items.diamond)
      itemhandler.insertItem(1, stack, false)

      for (
        (k, v) <- chunkMap
      ) {
        world.markChunkDirty(v, null)
      }


      // gah thread safety!
      val actor = system.actorOf(Props(new NudgedActor(m, world, blockPos, xOffset, yOffset, scale)), "NudgedActor-" + this.nudgedActors.size)
      nudgedActors = nudgedActors :+ actor
    }
  }
}
