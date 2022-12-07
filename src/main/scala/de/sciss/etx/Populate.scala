/*
 *  Populate.scala
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

import de.sciss.lucre.Folder
import de.sciss.lucre.synth.Txn
import de.sciss.nuages.{Nuages, ScissProcs}
import de.sciss.proc.Implicits._
import de.sciss.proc.{ParamSpec, Warp}
import de.sciss.{nuages, synth}

object Populate {

  // private val _registerActions = Ref(initialValue = false)

  // final val BaseDir = userHome/"Documents"/"applications"/"140616_SteiermarkRiga"/"tallin"

  // private final val RecDir  = BaseDir / "rec"

  final val NuagesName = "Nuages"

  def getNuages[T <: Txn[T]](root: Folder[T])(implicit tx: T): Option[Nuages[T]] =
    (root / NuagesName).flatMap {
      case n: Nuages[T] => Some(n)
      case _ => None
    }

  def apply[T <: Txn[T]](n: Nuages[T], nConfig: Nuages.Config, sConfig: ScissProcs.Config)
                        (implicit tx: T): Unit = {
    implicit val _n: Nuages[T] = n
    val dsl = nuages.DSL[T]
    import dsl._
    import synth._
    import ugen._
    import Import._

    Mutagens          (dsl, sConfig, nConfig)
    Almat             (dsl, sConfig, nConfig)
    ShouldGens        (dsl, sConfig, nConfig)

    def default(in: Double): ControlValues =
      if (sConfig.genNumChannels <= 0)
        in
      else
        Vector.fill(sConfig.genNumChannels)(in)

    // -------------- TALLINN --------------

//    filter(">mono") { in =>
//      shortcut = "GREATER"
//      Mix.mono(in) / NumChannels(in)
//    }


  }
}