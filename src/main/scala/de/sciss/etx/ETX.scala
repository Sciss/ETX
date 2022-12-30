/*
 *  ETX.scala
 *  (ETX)
 *
 *  Copyright (c) 2014-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.etx

import de.sciss.equal.Implicits._
import de.sciss.file._
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.synth.{InMemory, Server, Txn}
import de.sciss.lucre.{Cursor, Folder, Source}
import de.sciss.nuages
import de.sciss.nuages.Nuages.Surface
import de.sciss.nuages.{NamedBusConfig, Nuages, ScissProcs, Wolkenpumpe, WolkenpumpeMain}
import de.sciss.proc.{Durable, FScape, Timeline, Universe, Workspace}
import de.sciss.submin.Submin
import jpen.event.{PenAdapter, PenManagerListener}
import jpen.owner.multiAwt.AwtPenToolkit
import jpen.{PLevel, PLevelEvent, PenDevice, PenProvider}

import java.text.SimpleDateFormat
import java.util.Locale
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.swing.Swing
import scala.util.control.NonFatal

object ETX {
  def mkDatabase(parent: File): File = {
    require (parent.isDirectory)
    val recFormat = new SimpleDateFormat("'session_'yyMMdd'_'HHmmss'.mllt'", Locale.US)
    parent / recFormat.format(new java.util.Date)
  }

  def version     : String = buildInfString("version")
  def homepage    : String = buildInfString("homepage")
  def builtAt     : String = buildInfString("builtAtString")
  def fullVersion : String = s"v$version, built $builtAt"

  private def buildInfString(key: String): String = try {
    val clazz = Class.forName("de.sciss.anemone.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(_) => "?"
  }

  final case class Config(
                           masterChannels    : Range,
                           masterGroups      : Vec[NamedBusConfig] = Vector.empty,
                           soloChannels      : Range,
                           micInputs         : Vec[NamedBusConfig],
                           lineInputs        : Vec[NamedBusConfig],
                           lineOutputs       : Vec[NamedBusConfig],
                           genNumChannels    : Int                 = 0,
                           device            : Option[String]      = None,
                           database          : Option[File]        = None,
                           timeline          : Boolean             = true,
                           tablet            : Boolean             = true
  )

  lazy val ExpLab: Config = Config(
    masterChannels    = 0 until 2,
    soloChannels      = 0 until 0,
    genNumChannels    = 2,
    micInputs         = Vector(
      NamedBusConfig("m-mic", 0 until 2),
      NamedBusConfig("m-vig", 2 until 4),
      NamedBusConfig("m-luc", 4 until 6)
    ),
    lineInputs      = Vector.empty,
    lineOutputs     = Vector.empty,
    device    = Some("Wolkenpumpe"),
    database  = None, // Some(mkDatabase(userHome/"Documents"/"projects"/"Anemone"/"sessions")),
    timeline  = true
  )

  private val config: Config = ExpLab

  def mkSurface[T <: Txn[T]](config: Config)(implicit tx: T): Surface[T] =
    if (config.timeline) {
      val tl = Timeline[T]()
      Surface.Timeline(tl)
    } else {
      val f = Folder[T]()
      Surface.Folder(f)
    }

  private def createWorkspace(folder: File): Workspace.Durable =
    {
      val config          = BerkeleyDB.Config()
      config.allowCreate  = true
      val ds              = BerkeleyDB.factory(folder, config)
      // config.lockTimeout  = Duration(Prefs.dbLockTimeout.getOrElse(Prefs.defaultDbLockTimeout), TimeUnit.MILLISECONDS)
      val meta = Map(
        (Workspace.KeySoundProcessesVersion, de.sciss.proc.BuildInfo.version),
      )

      Workspace.Durable.empty(folder.toURI, ds, meta = meta)
    }

  def main(args: Array[String]): Unit = {
    println(s"ETX $fullVersion")
    nuages.showLog = false
    // de.sciss.nuages. DSL.useScanFixed = true
    // defer(WebLookAndFeel.install())
    Submin.install(true)
    Wolkenpumpe .init()
    FScape      .init()

    config.database match {
      case Some(f) =>
        type S = Durable
        type T = Durable.Txn
        val ws = createWorkspace(f)
        implicit val system: S = Durable(BerkeleyDB.factory(f))
        val anemone = new ETX[T](config)
        val nuagesH = system.step { implicit tx =>
          val n = Nuages[T](mkSurface[T](config))
          ws.root.addLast(n)
          tx.newHandle(n)
        }
        anemone.run(nuagesH)

      case None =>
        type S = InMemory
        type T = InMemory.Txn
        implicit val system: S = InMemory()
        val anemone = new ETX[T](config)
        val nuagesH = system.step { implicit tx =>
          val n = Nuages[T](mkSurface(config))
          tx.newHandle(n)
        }
        anemone.run(nuagesH)
    }
  }
}
class ETX[T <: Txn[T]](config: ETX.Config) extends WolkenpumpeMain[T] {

  override protected def configure(sCfg: ScissProcs.ConfigBuilder, nCfg: Nuages.ConfigBuilder,
                                   aCfg: Server.ConfigBuilder): Unit = {
    super.configure(sCfg, nCfg, aCfg)
    sCfg.genNumChannels  = config.genNumChannels
    // println(s"generatorChannels ${sCfg.generatorChannels}")
    nCfg.micInputs          = config.micInputs
    nCfg.lineInputs         = config.lineInputs
    nCfg.lineOutputs        = config.lineOutputs
    sCfg.mainGroups         = config.masterGroups
    // sCfg.highPass           = 100
    val dirMusic = userHome / "Music"
    sCfg.audioFilesFolder   = Some(dirMusic / "tapes")
    sCfg.plugins            = true
    sCfg.recDir             = dirMusic / "nuages_rec"

    // println(s"master max = ${Turbulence.ChannelIndices.max}")
    nCfg.mainChannels       = Some(config.masterChannels)
    nCfg.soloChannels       = if (config.soloChannels.nonEmpty) Some(config.soloChannels) else None
    nCfg.recordPath         = Some((userHome / "Music" / "rec").path) // XXX has no effect?

    aCfg.wireBuffers        = 512 // 1024
    aCfg.audioBuffers       = 4096
    // aCfg.blockSize          = 128
    if (config.device.isDefined) aCfg.deviceName = config.device
  }

  override protected def registerProcesses(nuages: Nuages[T], nCfg: Nuages.Config, sCfg: ScissProcs.Config)
                                 (implicit tx: T, universe: Universe[T]): Unit = {
    super.registerProcesses(nuages, nCfg, sCfg)
    Populate(nuages, this, nCfg, sCfg)
  }

  def initTablet(): Unit = {
    val penManager = AwtPenToolkit.getPenManager
    penManager.addListener(new PenManagerListener {
      def penDeviceAdded(c: PenProvider.Constructor, d: PenDevice): Unit = {
        println(s"penDeviceAdded($c, $d)")
      }

      def penDeviceRemoved(c: PenProvider.Constructor, d: PenDevice): Unit = ()
    })

    val awtComp = view.panel.display
    AwtPenToolkit.addPenListener(awtComp, new PenAdapter {
//      override def penButtonEvent(ev: PButtonEvent): Unit =
//        println(s"penButtonEvent($ev)")

      @volatile
      private[this] var lastLevel = 0f
      private[this] val panel     = view.panel
      // we take the maximum of these readings, so we don't lose the level when releasing the pen
      private[this] val levelMax  = new Array[Float](6)
      private[this] var levelMaxI = 0

      override def penLevelEvent(ev: PLevelEvent): Unit = {
        //        println(s"penLevelEvent($ev)")
        val levels = ev.levels
//        println(s"level: ${levels.mkString("[", ", ", "]")}")
        var i = 0
        while (i < levels.length) {
          val lvl = levels(i)

          // JPen 2.4 -- Tilt information is broken: https://github.com/nicarran/jpen/issues/12
          if (lvl.getType === PLevel.Type.PRESSURE /* TILT_Y */) {
            val raw: Float = lvl.value
            val arr = levelMax
            arr(levelMaxI) = raw
            levelMaxI = (levelMaxI + 1) % arr.length
            var j = 1
            var max = arr(0)
            while (j < arr.length) {
              val k = arr(j)
              if (k > max) max = k
              j += 1
            }
            val lvlClip = math.min(1.0f, math.max(0f, max - 0.1f) / 0.9f)
            // println(s"$raw | $max | $lvlClip | $lastLevel | ${panel.acceptGlideTime}")
            if (lvlClip != lastLevel) {
              lastLevel = lvlClip
              if (panel.acceptGlideTime) Swing.onEDT {
                if (panel.glideTimeSource !== "key") {
                  panel.glideTime       = lastLevel
                  panel.glideTimeSource = "tablet"
                }
              }
            }
            i = levels.length
          } else {
            i += 1
          }
        }
      }

//      override def penKindEvent(ev: PKindEvent): Unit = {
//        // println(s"penKindEvent($ev)")
//        println(s"kind: ${ev.kind}")
//      }

//      override def penScrollEvent(ev: PScrollEvent): Unit =
//        println(s"penScrollEvent($ev)")

      //      override def penTock(availableMillis: Long): Unit =
      //        println(s"penTock($availableMillis)")
    })
  }

  override def run(nuagesH: Source[T, Nuages[T]])(implicit cursor: Cursor[T]): Unit = {
    super.run(nuagesH)

    if (config.tablet) cursor.step { implicit tx =>
      auralSystem.whenStarted { server =>
        Swing.onEDT {
          initTablet()
        }
      }
    }
  }
}
