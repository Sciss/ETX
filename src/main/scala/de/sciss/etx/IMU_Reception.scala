package de.sciss.etx

import de.sciss.lucre.synth.{Bus, Txn}
import de.sciss.lucre.{Cursor, IntObj}
import de.sciss.nuages.{Nuages, ScissProcs, WolkenpumpeMain}
import de.sciss.proc.{AuralSystem, Proc, SoundProcesses}
import de.sciss.synth.{GE, message}
import de.sciss.synth.proc.graph.ScanOut
import de.sciss.{nuages, osc}

object IMU_Reception {
  def apply[T <: Txn[T]](nm: WolkenpumpeMain[T], dsl: nuages.DSL[T], sCfg: ScissProcs.Config, nCfg: Nuages.Config)
                        (implicit tx: T, n: Nuages[T]): Unit = {
    import dsl._

    class Sensor(val name: String, val bus: IntObj.Var[T])

    def genMul(name: String, sensors: Seq[Sensor])(fun: => GE)(implicit tx: T, n: Nuages[T]): Proc[T] = {
      val obj = mkProcObj(name) {
        val sig = fun
        import de.sciss.synth.GEOps._
        sensors.zipWithIndex.foreach { case (sensor, ni) =>
          ScanOut(sensor.name, sig.out(ni))
        }
      }
      val pOuts = obj.outputs
      val pIns  = obj.attr
      sensors.foreach { sensor =>
        pOuts.add(sensor.name)
        pIns.put(sensor.name, sensor.bus)
      }
      val genOpt = n.generators
      insertByName(genOpt.get, obj)
      obj
    }

    val sensorHead  = new Sensor("head" , IntObj.newVar[T](0))
    val sensorRoll  = new Sensor("roll" , IntObj.newVar[T](0))
    val sensorPitch = new Sensor("pitch", IntObj.newVar[T](0))
    val sensorsE    = Seq(sensorHead, sensorRoll, sensorPitch)

    genMul("euler-L", sensorsE) {
      import de.sciss.synth.proc.graph.Ops._
      import de.sciss.synth.ugen._
      val sigSeq: GE = sensorsE.map { sensor =>
        val bus = sensor.name.kr(0)
        val sig = In.kr(bus)
        sig
      }
      sigSeq
    }

    nm.auralSystem.reactNow { implicit tx => {
      case AuralSystem.Running(s) =>
        tx.afterCommit {
          val oCfg = osc.UDP.Config()
          oCfg.localPort = 7771
          oCfg.localIsLoopback = true
          val rcv = osc.UDP.Receiver(oCfg)
          rcv.connect()

          implicit val cursor: Cursor[T] = nm.view.cursor

          SoundProcesses.step[T]("sensor-init") { implicit tx =>
            val busIndexH = s.allocControlBus(1)
            sensorHead.bus() = IntObj.newConst[T](busIndexH)
//            val peerH = de.sciss.synth.AudioBus(s.peer, busIndexH, 1)
//            val busH  = Bus.wrap(s, peerH)

            rcv.action = {
              case (osc.Message(cmd, eh: Float, er: Float, ep: Float, ax: Float, ay: Float, az: Float), _) if cmd.startsWith("/sensor") =>
                val ch = cmd.charAt(cmd.length - 1) - '1'
                // println(s"head $ch is $eh")
                if (ch == 0) {
                  s.peer.!(message.ControlBusSet(busIndexH -> (eh / 180f)))
                }

              case (p, _) =>
                println(s"Unknown OSC packet $p")
            }
          }
        }

      case _ => ()
    }}
  }
}
